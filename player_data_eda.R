# Importing necessary libraries
library(jsonlite)

# Importing the 'nba_box_player_season.json' dataset
player_data <- fromJSON('.../intern_application_prithvi_project-dev/api_data_files/player.json')

# Displaying the first few rows of the dataset
head(player_data)

# Checking for missing values in the dataset
missing_values <- sapply(player_data, function(x) sum(is.na(x)))

# Displaying the number of missing values for each column
print(missing_values)

# Converting 'birth_date' column to Date format
player_data$birth_date <- as.Date(player_data$birth_date, format='%Y-%m-%d')

# Calculating age of the players
current_year <- as.numeric(format(Sys.Date(), '%Y'))
birth_year <- as.numeric(format(player_data$birth_date, '%Y'))
player_data$age <- current_year - birth_year

# Displaying the first few rows with the new 'age' column
head(player_data)

# Plotting the age distribution of the players
library(ggplot2)
ggplot(player_data, aes(x=age)) +
  geom_histogram(binwidth=1, fill='blue', color='black', alpha=0.7) +
  labs(title='Age Distribution of Players', x='Age', y='Number of Players') +
  theme_minimal()

# Identifying outliers in the 'age' column using IQR method
Q1 <- quantile(player_data$age, 0.25)
Q3 <- quantile(player_data$age, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - (1.5 * IQR)
upper_bound <- Q3 + (1.5 * IQR)

# Filtering out the outliers
outliers <- player_data[player_data$age < lower_bound | player_data$age > upper_bound,]

# Displaying the outliers
print(outliers)

# Replacing outlier ages with the median age
median_age <- median(player_data$age, na.rm = TRUE)
player_data$age[player_data$age < lower_bound | player_data$age > upper_bound] <- median_age

# Displaying the first few rows after treating outliers
head(player_data)

# Visualizing the top 10 most common first names
first_name_counts <- table(player_data$first_name)
top_first_names <- head(sort(first_name_counts, decreasing = TRUE), 10)

ggplot(as.data.frame(top_first_names), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'black') +
  coord_flip() +
  labs(title = 'Top 10 Most Common First Names', x = 'First Name', y = 'Count') +
  theme_minimal()
