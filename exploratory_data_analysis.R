# Load necessary libraries
library(jsonlite)
library(ggplot2)
library(e1071)

# Load data
international_box <- fromJSON("../intern_application_prithvi_project-dev/api_data_files/international_box_player_season.json")
nba_box <- fromJSON(".../intern_application_prithvi_project-dev/api_data_files/nba_box_player_season.json")
player <- fromJSON(".../intern_application_prithvi_project-dev/api_data_files/player.json")

# Examine structure of the three JSON's
str(international_box)
str(nba_box)
str(player)

# Further exploration of player data
head(player)
summary(player)

# Further exploration of nba_box data
head(nba_box)
summary(nba_box)

# Further exploration of international_box data
head(international_box)
summary(international_box)

# Candidates for outlier detection based on game performance metrics
variables <- c('minutes', 'points', 'two_points_made', 'two_points_attempted', 
               'three_points_made', 'three_points_attempted', 'free_throws_made', 
               'free_throws_attempted', 'blocked_shot_attempts', 'offensive_rebounds', 
               'defensive_rebounds', 'assists', 'screen_assists', 'turnovers', 'steals', 
               'deflections', 'loose_balls_recovered', 'blocked_shots', 'personal_fouls', 
               'personal_fouls_drawn', 'offensive_fouls', 'charges_drawn', 'technical_fouls', 
               'flagrant_fouls', 'ejections', 'points_off_turnovers', 'points_in_paint', 
               'second_chance_points', 'fast_break_points')

# Generate scatter plots for game performance metrics
num_vars <- length(variables)
plots_per_page <- 4
num_pages <- ceiling(num_vars / plots_per_page)
for (page in 1:num_pages) {
  start_idx <- (page - 1) * plots_per_page + 1
  end_idx <- min(page * plots_per_page, num_vars)
  par(mfrow=c(2, 2), mar=c(4, 4, 2, 2))
  for (idx in start_idx:end_idx) {
    var <- variables[idx]
    plot(international_box[[var]], main=var, xlab="Index", ylab=var, col="blue", pch=19, cex=0.6)
  }
}

# Calculate and display outliers from the above variable plots
outlier_list <- list()
for (var in variables) {
  IQR_val <- IQR(international_box[[var]])
  upper_bound <- quantile(international_box[[var]], 0.75) + 1.5 * IQR_val
  lower_bound <- quantile(international_box[[var]], 0.25) - 1.5 * IQR_val
  outliers <- international_box[[var]][international_box[[var]] < lower_bound | international_box[[var]] > upper_bound]
  outlier_list[[var]] <- outliers
}
print(outlier_list)

# Histograms for better understanding
hist(international_box$minutes, 
     main = "Distribution of Minutes Played",
     xlab = "Minutes",
     ylab = "Frequency",
     col = "blue")
hist(international_box$personal_fouls, 
     main = "Distribution of personal fouls",
     xlab = "fouls",
     ylab = "Frequency",
     col = "blue")

# Capture outlier indices and corresponding player details
outlier_indices <- list()
outlier_details <- list()
for (var in variables) {
  IQR_val <- IQR(international_box[[var]])
  upper_bound <- quantile(international_box[[var]], 0.75) + 1.5 * IQR_val
  lower_bound <- quantile(international_box[[var]], 0.25) - 1.5 * IQR_val
  outlier_idx <- which(international_box[[var]] < lower_bound | international_box[[var]] > upper_bound)
  outlier_indices[[var]] <- outlier_idx
  outlier_values <- international_box[[var]][outlier_idx]
  outlier_df <- data.frame(
    first_name = international_box$first_name[outlier_idx],
    last_name = international_box$last_name[outlier_idx],
    team = international_box$team[outlier_idx],
    outlier_value = outlier_values
  )
  outlier_df <- outlier_df[order(outlier_df$outlier_value), ]  # Sort by outlier value in ascending order
  outlier_details[[var]] <- outlier_df
}
print(outlier_details)

# List of advanced statistics variables to check for outliers in int_box
variables_int_box_adv_stats <- c('possessions', 'estimated_possessions', 'team_possessions', 
                                 'usage_percentage', 'true_shooting_percentage', 'assist_percentage', 
                                 'turnover_percentage', 'offensive_rebound_percentage', 'defensive_rebound_percentage', 
                                 'total_rebound_percentage', 'steal_percentage', 'block_percentage', 
                                 'offensive_rating', 'defensive_rating', 'net_rating', 'player_impact_estimate')

# Generate scatter plots for advanced statistics
num_vars <- length(variables_int_box_adv_stats)
plots_per_page <- 4
num_pages <- ceiling(num_vars / plots_per_page)
for (page in 1:num_pages) {
  start_idx <- (page - 1) * plots_per_page + 1
  end_idx <- min(page * plots_per_page, num_vars)
  par(mfrow=c(2, 2), mar=c(4, 4, 2, 2))
  for (idx in start_idx:end_idx) {
    var <- variables_int_box_adv_stats[idx]
    plot(international_box[[var]], main=var, xlab="Index", ylab=var, col="blue", pch=19, cex=0.6)
  }
}

# Calculate and display outliers from the above variable plots
outlier_list_adv_stats <- list()
for (var in variables_int_box_adv_stats) {
  IQR_val <- IQR(international_box[[var]])
  upper_bound <- quantile(international_box[[var]], 0.75) + 1.5 * IQR_val
  lower_bound <- quantile(international_box[[var]], 0.25) - 1.5 * IQR_val
  outliers <- international_box[[var]][international_box[[var]] < lower_bound | international_box[[var]] > upper_bound]
  outlier_list_adv_stats[[var]] <- outliers
}
print(outlier_list_adv_stats)

# Capture outlier indices and corresponding player details for advanced stats
outlier_indices_adv_stats <- list()
outlier_details_adv_stats <- list()
for (var in variables_int_box_adv_stats) {
  IQR_val <- IQR(international_box[[var]])
  upper_bound <- quantile(international_box[[var]], 0.75) + 1.5 * IQR_val
  lower_bound <- quantile(international_box[[var]], 0.25) - 1.5 * IQR_val
  outlier_idx <- which(international_box[[var]] < lower_bound | international_box[[var]] > upper_bound)
  outlier_indices_adv_stats[[var]] <- outlier_idx
  outlier_values <- international_box[[var]][outlier_idx]
  outlier_df <- data.frame(
    first_name = international_box$first_name[outlier_idx],
    last_name = international_box$last_name[outlier_idx],
    team = international_box$team[outlier_idx],
    outlier_value = outlier_values
  )
  outlier_df <- outlier_df[order(outlier_df$outlier_value), ]  # Sort by outlier value in ascending order
  outlier_details_adv_stats[[var]] <- outlier_df
}
print(outlier_details_adv_stats)