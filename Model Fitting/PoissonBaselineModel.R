# This is a Poisson Factorization model on just raw goal counts. 
# Model is fit for players with at least 10 minutes playing time. 

library(dplyr)
library(poismf)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

min_5v5_time_sec <- 100 * 60
k_factors <- 5

# ------------------------------------------------------------
# 1. Keep players with at least five actual 5v5 minutes
# ------------------------------------------------------------

player_5v5_toi <- season_shift_player_long_2025_26 |>
  group_by(player_id, player_role) |>
  summarise(
    total_5v5_time_sec = sum(shift_duration_sec),
    total_5v5_time_min = total_5v5_time_sec / 60,
    .groups = "drop"
  )

eligible_defender_ids <- player_5v5_toi |>
  filter(
    player_role == "defense",
    total_5v5_time_sec >= min_5v5_time_sec
  ) |>
  pull(player_id) |>
  as.character()

eligible_forward_ids <- player_5v5_toi |>
  filter(
    player_role == "forward",
    total_5v5_time_sec >= min_5v5_time_sec
  ) |>
  pull(player_id) |>
  as.character()

# Restrict matrices to eligible players.
filtered_goals_matrix <- master_goals_against_matrix[
  eligible_defender_ids,
  eligible_forward_ids,
  drop = FALSE
]

filtered_shared_time_matrix <- master_shared_time_matrix[
  eligible_defender_ids,
  eligible_forward_ids,
  drop = FALSE
]

# ------------------------------------------------------------
# 2. Convert observed defender-forward matchups to long format
# ------------------------------------------------------------

observed_pairs <- which(
  !is.na(filtered_goals_matrix) &
    !is.na(filtered_shared_time_matrix) &
    filtered_shared_time_matrix > 0,
  arr.ind = TRUE
)

poismf_input <- data.frame(
  defender_id = rownames(filtered_goals_matrix)[
    observed_pairs[, 1]
  ],
  
  forward_id = colnames(filtered_goals_matrix)[
    observed_pairs[, 2]
  ],
  
  goals_against = filtered_goals_matrix[observed_pairs],
  stringsAsFactors = FALSE
)

# poismf only accepts non-zero counts in sparse-format input.
# This is the main limitation of this exploratory baseline:
# observed zero-goal matchups are treated as unobserved by poismf.
poismf_input_positive <- poismf_input |>
  filter(goals_against > 0)

if (nrow(poismf_input_positive) == 0) {
  stop(
    "No positive goal counts remain after filtering. ",
    "Check master_goals_against_matrix."
  )
}

# ------------------------------------------------------------
# 3. Fit exploratory raw-goals Poisson factorization
# ------------------------------------------------------------

set.seed(123)

poismf_baseline <- poismf(
  X = poismf_input_positive |>
    dplyr::select(defender_id, forward_id, goals_against),
  
  k = k_factors,
  method = "tncg",
  nthreads = 1
)

# ------------------------------------------------------------
# 4. Predict every defender-forward combination retained
#    by the poismf model
# ------------------------------------------------------------

poismf_mapping <- get.model.mappings(poismf_baseline)

poismf_defender_ids <- as.character(poismf_mapping$rows)
poismf_forward_ids <- as.character(poismf_mapping$columns)

prediction_grid <- expand.grid(
  defender_id = poismf_defender_ids,
  forward_id = poismf_forward_ids,
  stringsAsFactors = FALSE
)

prediction_grid$predicted_goal_count <- predict(
  poismf_baseline,
  a = prediction_grid$defender_id,
  b = prediction_grid$forward_id
)

predicted_goal_count_matrix <- matrix(
  prediction_grid$predicted_goal_count,
  nrow = length(poismf_defender_ids),
  ncol = length(poismf_forward_ids),
  dimnames = list(
    defender_id = poismf_defender_ids,
    forward_id = poismf_forward_ids
  )
)

# ------------------------------------------------------------
# 5. Standardize every defender to one common mix of forwards
# ------------------------------------------------------------

time_for_model_players <- filtered_shared_time_matrix[
  poismf_defender_ids,
  poismf_forward_ids,
  drop = FALSE
]

# Give more weight to forwards with more observed league-wide
# opponent 5v5 time.
forward_weights <- colSums(
  time_for_model_players,
  na.rm = TRUE
)

forward_weights <- forward_weights / sum(forward_weights)

# Lower standardized expected goal count means better defense.
standardized_predicted_goal_count <- drop(
  predicted_goal_count_matrix %*% forward_weights
)

# Convert to a higher-is-better relative defensive rating.
mean_predicted_goal_count <- mean(
  standardized_predicted_goal_count,
  na.rm = TRUE
)

poismf_defensive_rating <- mean_predicted_goal_count -
  standardized_predicted_goal_count

# ------------------------------------------------------------
# 6. Create the final ranking table
# ------------------------------------------------------------

poismf_defender_rankings <- data.frame(
  defender_id = names(standardized_predicted_goal_count),
  predicted_goal_count_index = standardized_predicted_goal_count,
  poismf_defensive_rating = poismf_defensive_rating,
  stringsAsFactors = FALSE
) |>
  left_join(
    player_5v5_toi |>
      filter(player_role == "defense") |>
      transmute(
        defender_id = as.character(player_id),
        total_5v5_time_sec,
        total_5v5_time_min
      ),
    by = "defender_id"
  ) |>
  mutate(
    defender_name = unname(player_name_lookup[defender_id])
  ) |>
  arrange(desc(poismf_defensive_rating))

# Best estimated defenders under this exploratory model
head(poismf_defender_rankings, 20)

# Save results
write.csv(
  poismf_defender_rankings,
  "poismf_defender_rankings_2025_26.csv",
  row.names = FALSE
)