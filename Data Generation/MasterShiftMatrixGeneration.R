# Main Functions

# Making 5v5 stints function

make_game_shift_matrix <- function(game_id) {
  
  # ---- Helper functions ----
  
  clock_to_sec <- function(x) {
    x <- as.character(x)
    pieces <- strsplit(x, ":", fixed = TRUE)[[1]]
    as.integer(pieces[1]) * 60 + as.integer(pieces[2])
  }
  
  pad_to <- function(x, n) {
    x <- sort(unique(as.integer(x)))
    c(x, rep(NA_integer_, max(0, n - length(x))))[seq_len(n)]
  }
  
  game_id_input <- game_id
  
  # ---- Download play-by-play and roster ----
  
  pbp <- httr2::request(
    paste0(
      "https://api-web.nhle.com/v1/gamecenter/",
      game_id,
      "/play-by-play"
    )
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
  
  home_team_id <- pbp$homeTeam$id
  away_team_id <- pbp$awayTeam$id
  
  roster <- purrr::map_dfr(
    pbp$rosterSpots,
    \(x) tibble::tibble(
      player_id = x$playerId,
      team_id = x$teamId,
      position = x$positionCode
    )
  ) |>
    dplyr::distinct(player_id, team_id, .keep_all = TRUE)
  
  # ---- Download individual player shifts ----
  
  shifts <- httr2::request(
    "https://api.nhle.com/stats/rest/en/shiftcharts"
  ) |>
    httr2::req_url_query(
      cayenneExp = paste0("gameId=", game_id),
      limit = 5000
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("data") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      team_id = teamId,
      player_id = playerId,
      period = as.integer(period),
      start_sec = (period - 1) * 1200 +
        vapply(startTime, clock_to_sec, numeric(1)),
      end_sec = (period - 1) * 1200 +
        vapply(endTime, clock_to_sec, numeric(1))
    ) |>
    dplyr::filter(
      !is.na(start_sec),
      !is.na(end_sec),
      end_sec > start_sec
    )
  
  if (nrow(shifts) == 0) {
    stop("No usable shift data found for game_id = ", game_id)
  }
  
  # ---- Create every possible no-substitution interval ----
  
  boundaries <- sort(unique(c(0, shifts$start_sec, shifts$end_sec)))
  
  intervals <- tibble::tibble(
    start_sec = boundaries[-length(boundaries)],
    end_sec = boundaries[-1]
  ) |>
    dplyr::filter(end_sec > start_sec)
  
  # ---- Determine the 2D + 3F lineup in one interval ----
  
  make_lineup <- function(start, end) {
    
    on_ice <- shifts |>
      dplyr::filter(start_sec <= start, end_sec > start) |>
      dplyr::distinct(team_id, player_id) |>
      dplyr::left_join(
        roster,
        by = c("team_id", "player_id")
      )
    
    get_team_players <- function(id) {
      
      team_on_ice <- on_ice |>
        dplyr::filter(team_id == id)
      
      list(
        defenders = team_on_ice |>
          dplyr::filter(position == "D") |>
          dplyr::pull(player_id),
        
        forwards = team_on_ice |>
          dplyr::filter(!position %in% c("D", "G")) |>
          dplyr::pull(player_id)
      )
    }
    
    away <- get_team_players(away_team_id)
    home <- get_team_players(home_team_id)
    
    away_d <- pad_to(away$defenders, 2)
    away_f <- pad_to(away$forwards, 3)
    home_d <- pad_to(home$defenders, 2)
    home_f <- pad_to(home$forwards, 3)
    
    tibble::tibble(
      start_sec = start,
      end_sec = end,
      
      away_d1 = away_d[1],
      away_d2 = away_d[2],
      away_f1 = away_f[1],
      away_f2 = away_f[2],
      away_f3 = away_f[3],
      
      home_d1 = home_d[1],
      home_d2 = home_d[2],
      home_f1 = home_f[1],
      home_f2 = home_f[2],
      home_f3 = home_f[3],
      
      is_5v5 =
        length(away$defenders) == 2 &&
        length(away$forwards) == 3 &&
        length(home$defenders) == 2 &&
        length(home$forwards) == 3
    )
  }
  
  # ---- Build and merge adjacent identical 5v5 shifts ----
  
  shift_matrix <- purrr::map_dfr(
    seq_len(nrow(intervals)),
    \(i) make_lineup(
      intervals$start_sec[i],
      intervals$end_sec[i]
    )
  ) |>
    dplyr::mutate(
      lineup_key = ifelse(
        is_5v5,
        paste(
          away_d1, away_d2, away_f1, away_f2, away_f3,
          home_d1, home_d2, home_f1, home_f2, home_f3,
          sep = "|"
        ),
        paste0("not_5v5_", dplyr::row_number())
      ),
      
      # Internal ID used only to merge adjacent identical lineups
      shift_block_id = cumsum(
        lineup_key != dplyr::lag(lineup_key, default = "")
      )
    ) |>
    dplyr::filter(is_5v5) |>
    dplyr::group_by(
      shift_block_id,
      away_d1, away_d2, away_f1, away_f2, away_f3,
      home_d1, home_d2, home_f1, home_f2, home_f3
    ) |>
    dplyr::summarise(
      shift_start_sec = min(start_sec),
      shift_end_sec = max(end_sec),
      shift_duration_sec = shift_end_sec - shift_start_sec,
      .groups = "drop"
    ) |>
    dplyr::arrange(shift_start_sec) |>
    dplyr::mutate(
      shift_id = dplyr::row_number(),
      .before = 1
    ) |>
    dplyr::select(-shift_block_id)
  
  # ---- Extract goal events ----
  
  goal_events <- pbp$plays |>
    purrr::keep(\(x) identical(x$typeDescKey, "goal")) |>
    purrr::map_dfr(\(x) {
      tibble::tibble(
        event_id = x$eventId,
        event_sec = (x$periodDescriptor$number - 1) * 1200 +
          clock_to_sec(x$timeInPeriod),
        scoring_team_id = x$details$eventOwnerTeamId
      )
    })
  
  # ---- Attach goals to each 5v5 stint ----
  
  shift_matrix <- shift_matrix |>
    dplyr::rowwise() |>
    dplyr::mutate(
      away_goals = sum(
        goal_events$scoring_team_id == away_team_id &
          goal_events$event_sec >= shift_start_sec &
          goal_events$event_sec < shift_end_sec,
        na.rm = TRUE
      ),
      
      home_goals = sum(
        goal_events$scoring_team_id == home_team_id &
          goal_events$event_sec >= shift_start_sec &
          goal_events$event_sec < shift_end_sec,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(game_id = game_id_input, .before = 1)
  
  return(shift_matrix)
}

make_shift_player_long <- function(shift_matrix) {
  
  lineup_cols <- c(
    "away_d1", "away_d2", "away_f1", "away_f2", "away_f3",
    "home_d1", "home_d2", "home_f1", "home_f2", "home_f3"
  )
  
  required_cols <- c(
    "game_id",
    "shift_id",
    "shift_start_sec", "shift_end_sec", "shift_duration_sec",
    "away_goals", "home_goals",
    lineup_cols
  )
  
  missing_cols <- setdiff(required_cols, names(shift_matrix))
  
  if (length(missing_cols) > 0) {
    stop(
      "shift_matrix is missing: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  shift_matrix |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(lineup_cols),
      names_to = "player_slot",
      values_to = "player_id",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      player_team = dplyr::if_else(
        stringr::str_starts(player_slot, "home"),
        "home",
        "away"
      ),
      
      player_role = dplyr::if_else(
        stringr::str_detect(player_slot, "_d"),
        "defense",
        "forward"
      )
    ) |>
    dplyr::select(
      game_id,
      shift_id,
      shift_start_sec,
      shift_end_sec,
      shift_duration_sec,
      player_id,
      player_slot,
      player_team,
      player_role,
      away_goals,
      home_goals
    ) |>
    dplyr::arrange(
      game_id,
      shift_id,
      player_team,
      player_role,
      player_id
    )
}

create_combined_shift_matrix <- function(game_ids) {
  
  all_games <- vector("list", length(game_ids))
  
  for (i in seq_along(game_ids)) {
    
    game_id <- game_ids[i]
    
    message(
      "Processing game ", i, " of ", length(game_ids),
      ": ", game_id
    )
    
    # Creates the game-level 5v5 shift matrix
    shift_matrix <- make_game_shift_matrix(game_id)
    
    # Converts that game's shifts to 10 player-level rows per shift
    all_games[[i]] <- make_shift_player_long(
      shift_matrix = shift_matrix
    )
  }
  
  dplyr::bind_rows(all_games)
}

# Main Logic

# Get every regular-season game ID for 2025-26
games_2025_26 <- httr2::request(
  "https://api.nhle.com/stats/rest/en/game"
) |>
  httr2::req_url_query(
    cayenneExp = "season=20252026 and gameType=2",
    limit = -1
  ) |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyVector = TRUE) |>
  purrr::pluck("data") |>
  tibble::as_tibble() |>
  dplyr::arrange(gameDate)

game_ids_2025_26 <- games_2025_26 |>
  dplyr::pull(id)

# Create the 5v5 player-level shift table across the full season
season_shift_player_long_2025_26 <- create_combined_shift_matrix(
  game_ids = game_ids_2025_26
)

# Save it
write.csv(
  season_shift_player_long_2025_26,
  "nhl_2025_26_5v5_shifts.csv",
  row.names = FALSE
)

