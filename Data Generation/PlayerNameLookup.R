# This script is for grabbing player names from the available data.
# Must import shift data. 

library(httr2)
library(dplyr)
library(purrr)
library(tibble)

# Data Import
season_shift_player_long_2025_26 <- read.csv("nhl_2025_26_5v5_shifts.csv")

season_id <- 20252026
page_size <- 100

# Download one page of NHL regular-season skater data
get_skater_page <- function(start_value) {
  
  request(
    "https://api.nhle.com/stats/rest/en/skater/summary"
  ) |>
    req_url_query(
      cayenneExp = paste0(
        "seasonId=", season_id,
        " and gameTypeId=2"
      ),
      start = start_value,
      limit = page_size
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

# First page tells us the total number of records.
first_page <- get_skater_page(start_value = 0)

total_skaters <- first_page$total

page_starts <- seq(
  from = 0,
  to = total_skaters - 1,
  by = page_size
)

# Combine the first page with every remaining page.
all_skater_data <- c(
  list(first_page$data),
  map(
    page_starts[-1],
    \(start_value) get_skater_page(start_value)$data
  )
) |>
  bind_rows() |>
  as_tibble()

# Create your player ID-to-name lookup table.
player_ids_in_data <- season_shift_player_long_2025_26 |>
  distinct(player_id) |>
  pull(player_id)

nhl_player_names <- all_skater_data |>
  transmute(
    player_id = playerId,
    player_name = skaterFullName
  ) |>
  distinct(player_id, .keep_all = TRUE) |>
  filter(player_id %in% player_ids_in_data) |>
  arrange(player_name)

# Named vector for fast lookup by ID
player_name_lookup <- stats::setNames(
  nhl_player_names$player_name,
  as.character(nhl_player_names$player_id)
)