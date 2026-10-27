# This script converts 5v5 player-shift data from the 2025-26 NHL season
# into defender-by-forward matchup matrices.
#
# Rows represent defenders; columns represent forwards.
# Each numeric cell describes opponent 5v5 exposure only:
#   - GPM: goals scored against the defender's team per minute
#   - goals: raw goals scored against the defender's team
#   - shared time: seconds the defender faced the forward at 5v5
#
# The relationship matrix records whether a defender-forward pair shared
# 5v5 ice time as opponents, teammates, both, or neither.

library(dplyr)

# Data Import
season_shift_player_long_2025_26 <- read.csv("nhl_2025_26_5v5_shifts.csv")

# Initial Prep

# Create one consistent role assignment for every player who appeared at 5v5.
# This uses the role appearing most often for a player, which protects against
# an occasional inconsistent roster listing across games.
player_role_lookup <- season_shift_player_long_2025_26 |>
  dplyr::count(player_id, player_role, name = "n_player_shift_rows") |>
  dplyr::group_by(player_id) |>
  dplyr::slice_max(
    order_by = n_player_shift_rows,
    n = 1,
    with_ties = FALSE
  ) |>
  dplyr::ungroup() |>
  dplyr::select(player_id, player_role)

# Optional diagnostic: players listed in more than one role during the season.
# For the 2025/26 season, the players_with_multiple_roles matrix comes up empty. 
players_with_multiple_roles <- season_shift_player_long_2025_26 |>
  dplyr::distinct(player_id, player_role) |>
  dplyr::count(player_id, name = "n_roles") |>
  dplyr::filter(n_roles > 1)

# Get the complete sets of defenders and forwards.
defender_ids <- player_role_lookup |>
  dplyr::filter(player_role == "defense") |>
  dplyr::pull(player_id) |>
  sort()

forward_ids <- player_role_lookup |>
  dplyr::filter(player_role == "forward") |>
  dplyr::pull(player_id) |>
  sort()


# Final structure:
# rows    = defenders
# columns = opposing forwards
# values  = goals against per minute (GPM)
# NA means that matchup had no 5v5 ice time.


# ------------------------------------------------------------
# 1. For one defender, summarize every player who shared one of
#    the defender's 5v5 shifts, as teammate or opponent.
# ------------------------------------------------------------

make_defender_player_summary <- function(
    all_player_stints,
    defender_id
) {
  
  defender_shift_keys <- all_player_stints |>
    filter(player_id == defender_id) |>
    distinct(
      game_id,
      shift_id,
      defender_team = player_team
    )
  
  if (nrow(defender_shift_keys) == 0) {
    stop(
      "Defender ", defender_id,
      " does not appear in all_player_stints."
    )
  }
  
  all_player_stints |>
    inner_join(
      defender_shift_keys,
      by = c("game_id", "shift_id")
    ) |>
    mutate(
      teammate_or_opponent = if_else(
        player_team == defender_team,
        "teammate",
        "opponent"
      ),
      
      # Convert game home/away scoring into defender-relative scoring.
      goals_against_defender = if_else(
        defender_team == "home",
        away_goals,
        home_goals
      ),
      
      goals_for_defender = if_else(
        defender_team == "home",
        home_goals,
        away_goals
      )
    ) |>
    filter(player_id != defender_id) |>
    group_by(
      defender_id = defender_id,
      player_id,
      player_role,
      teammate_or_opponent
    ) |>
    summarise(
      total_time_sec = sum(shift_duration_sec),
      total_goals_against_defender = sum(goals_against_defender),
      total_goals_for_defender = sum(goals_for_defender),
      .groups = "drop"
    ) |>
    arrange(
      teammate_or_opponent,
      player_role,
      desc(total_time_sec)
    )
}


# ------------------------------------------------------------
# 2. Keep only opposing forwards and calculate GPM.
#    The teammate rows remain available in the full player
#    summary from the first function.
# ------------------------------------------------------------

make_defender_opponent_gpm <- function(defender_player_summary) {
  
  defender_player_summary |>
    filter(
      teammate_or_opponent == "opponent",
      player_role == "forward"
    ) |>
    transmute(
      defender_id,
      forward_id = player_id,
      opponent_time_sec = total_time_sec,
      goals_against_defender = total_goals_against_defender,
      gpm = goals_against_defender / (opponent_time_sec / 60)
    )
}


# ------------------------------------------------------------
# 3. Make one complete named GPM row for one defender.
#    All NHL forward columns are present.
#    NA = no opponent 5v5 time against that forward.
# ------------------------------------------------------------

make_defender_super_row <- function(
    defender_id,
    defender_opponent_gpm,
    forward_ids
) {
  
  super_row <- stats::setNames(
    rep(NA_real_, length(forward_ids)),
    as.character(forward_ids)
  )
  
  matched_forward_ids <- intersect(
    names(super_row),
    as.character(defender_opponent_gpm$forward_id)
  )
  
  super_row[matched_forward_ids] <- defender_opponent_gpm$gpm[
    match(
      matched_forward_ids,
      as.character(defender_opponent_gpm$forward_id)
    )
  ]
  
  super_row
}


make_defender_companion_rows <- function(
    defender_player_summary,
    forward_ids
) {
  
  forward_names <- as.character(forward_ids)
  
  # NA means the defender and forward had no 5v5 opponent exposure.
  goals_row <- stats::setNames(
    rep(NA_real_, length(forward_ids)),
    forward_names
  )
  
  shared_time_row <- stats::setNames(
    rep(NA_real_, length(forward_ids)),
    forward_names
  )
  
  relationship_row <- stats::setNames(
    rep("no_shared_5v5_time", length(forward_ids)),
    forward_names
  )
  
  opponent_forwards <- defender_player_summary |>
    filter(
      teammate_or_opponent == "opponent",
      player_role == "forward"
    ) |>
    transmute(
      forward_id = player_id,
      opponent_time_sec = total_time_sec,
      goals_against = total_goals_against_defender
    )
  
  teammate_forwards <- defender_player_summary |>
    filter(
      teammate_or_opponent == "teammate",
      player_role == "forward"
    ) |>
    transmute(forward_id = player_id)
  
  opponent_ids <- as.character(opponent_forwards$forward_id)
  teammate_ids <- as.character(teammate_forwards$forward_id)
  
  # These values are meaningful only for opponent matchups.
  goals_row[opponent_ids] <- opponent_forwards$goals_against
  shared_time_row[opponent_ids] <- opponent_forwards$opponent_time_sec
  
  relationship_row[opponent_ids] <- "opponents_only"
  relationship_row[teammate_ids] <- "teammates_only"
  
  both_ids <- intersect(opponent_ids, teammate_ids)
  relationship_row[both_ids] <- "opponents_and_teammates"
  
  list(
    goals_row = goals_row,
    shared_time_row = shared_time_row,
    relationship_row = relationship_row
  )
}


# ------------------------------------------------------------
# 4. Main function: loop through every defender, retain the
#    long matchup summaries, and fill the GPM master matrix.
# ------------------------------------------------------------

create_master_matrices <- function(
    all_player_stints,
    defender_ids,
    forward_ids
) {
  
  matrix_dimnames <- list(
    defender_id = as.character(defender_ids),
    forward_id = as.character(forward_ids)
  )
  
  # Total seconds that the defender faced each forward at 5v5.
  master_shared_time_matrix <- matrix(
    NA_real_,
    nrow = length(defender_ids),
    ncol = length(forward_ids),
    dimnames = matrix_dimnames
  )
  
  # Numeric rate: goals against per minute
  master_gpm_matrix <- matrix(
    NA_real_,
    nrow = length(defender_ids),
    ncol = length(forward_ids),
    dimnames = matrix_dimnames
  )
  
  # Raw goal count underlying the GPM calculation
  master_goals_against_matrix <- matrix(
    NA_real_,
    nrow = length(defender_ids),
    ncol = length(forward_ids),
    dimnames = matrix_dimnames
  )
  
  # Why a pair has a value or NA
  master_relationship_matrix <- matrix(
    "no_shared_5v5_time",
    nrow = length(defender_ids),
    ncol = length(forward_ids),
    dimnames = matrix_dimnames
  )
  
  all_defender_player_summaries <- vector(
    "list",
    length(defender_ids)
  )
  
  all_defender_opponent_gpms <- vector(
    "list",
    length(defender_ids)
  )
  
  for (i in seq_along(defender_ids)) {
    
    defender_id <- defender_ids[i]
    
    message(
      "Processing defender ", i, " of ", length(defender_ids),
      ": ", defender_id
    )
    
    # Includes both teammate and opponent rows.
    defender_player_summary <- make_defender_player_summary(
      all_player_stints = all_player_stints,
      defender_id = defender_id
    )
    
    # Only opposing forwards receive GPM values.
    defender_opponent_gpm <- make_defender_opponent_gpm(
      defender_player_summary
    )
    
    # Numeric GPM row
    defender_gpm_row <- make_defender_super_row(
      defender_id = defender_id,
      defender_opponent_gpm = defender_opponent_gpm,
      forward_ids = forward_ids
    )
    
    # Raw goals-against and relationship rows
    companion_rows <- make_defender_companion_rows(
      defender_player_summary = defender_player_summary,
      forward_ids = forward_ids
    )
    
    # Insert all three rows into their corresponding matrices.
    master_gpm_matrix[
      as.character(defender_id),
      names(defender_gpm_row)
    ] <- defender_gpm_row
    
    master_goals_against_matrix[
      as.character(defender_id),
      names(companion_rows$goals_row)
    ] <- companion_rows$goals_row
    
    master_shared_time_matrix[
      as.character(defender_id),
      names(companion_rows$shared_time_row)
    ] <- companion_rows$shared_time_row
    
    master_relationship_matrix[
      as.character(defender_id),
      names(companion_rows$relationship_row)
    ] <- companion_rows$relationship_row
    
    # Keep long-format data for auditing and future analyses.
    all_defender_player_summaries[[i]] <- defender_player_summary
    all_defender_opponent_gpms[[i]] <- defender_opponent_gpm
  }
  
  list(
    master_gpm_matrix = master_gpm_matrix,
    master_goals_against_matrix = master_goals_against_matrix,
    master_shared_time_matrix = master_shared_time_matrix,
    master_relationship_matrix = master_relationship_matrix,
    defender_player_summary = bind_rows(all_defender_player_summaries),
    defender_opponent_gpm = bind_rows(all_defender_opponent_gpms)
  )
}

# Running the functions
master_results <- create_master_matrices(
  all_player_stints = season_shift_player_long_2025_26,
  defender_ids = defender_ids,
  forward_ids = forward_ids
)

master_gpm_matrix <- master_results$master_gpm_matrix
master_shared_time_matrix <- master_results$master_shared_time_matrix
defender_player_summary <- master_results$defender_player_summary
master_goals_against_matrix <- master_results$master_goals_against_matrix
defender_opponent_gpm <- master_results$defender_opponent_gpm
master_relationship_matrix <- master_results$master_relationship_matrix

# Helper function for saving
write_matchup_matrix <- function(matchup_matrix, file_name) {
  
  output <- data.frame(
    defender_id = rownames(matchup_matrix),
    matchup_matrix,
    check.names = FALSE
  )
  
  write.csv(
    output,
    file_name,
    row.names = FALSE
  )
}

write_matchup_matrix(
  master_gpm_matrix,
  "master_gpm_matrix_2025_26.csv"
)

write_matchup_matrix(
  master_shared_time_matrix,
  "master_shared_time_matrix_2025_26.csv"
)

write_matchup_matrix(
  master_goals_against_matrix,
  "master_goals_against_matrix_2025_26.csv"
)

write_matchup_matrix(
  master_relationship_matrix,
  "master_relationship_matrix_2025_26.csv"
)

