# Importing necessary libraries
library(jsonlite)

# Importing the 'nba_box_player_season.json' dataset
nba_box_data_url <- '.../intern_application_prithvi_project-dev/api_data_files/nba_box_player_season.json'
nba_box_data <- fromJSON(nba_box_data_url, flatten = TRUE)

# Displaying the first few rows of the dataset
head(nba_box_data)

# Generating a statistical summary of the 'nba_box_data' dataset
summary(nba_box_data)

# Importing necessary libraries for visualization
library(ggplot2)

# Plotting the distribution of key metrics
par(mfrow=c(2,2))

# Distribution of Points
hist(nba_box_data$points, breaks=50, main='Distribution of Points', xlab='Points', col='skyblue', border='black')

# Distribution of Assists
hist(nba_box_data$assists, breaks=50, main='Distribution of Assists', xlab='Assists', col='lightgreen', border='black')

# Distribution of Defensive Rebounds
hist(nba_box_data$defensive_rebounds, breaks=50, main='Distribution of Defensive Rebounds', xlab='Defensive Rebounds', col='lightcoral', border='black')

# Distribution of Offensive Rebounds
hist(nba_box_data$offensive_rebounds, breaks=50, main='Distribution of Offensive Rebounds', xlab='Offensive Rebounds', col='lightyellow', border='black')

# Importing necessary libraries for correlation visualization
library(corrplot)

# Calculating the correlation matrix for key metrics
cor_matrix <- cor(nba_box_data[, c('points', 'assists', 'defensive_rebounds', 'offensive_rebounds')], use='complete.obs')

# Visualizing the correlation matrix using a heatmap
corrplot(cor_matrix, method='color', type='upper', diag=FALSE, tl.col='black', tl.srt=45)
