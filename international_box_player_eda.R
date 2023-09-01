# Cell 1
library(tidyverse)
library(lubridate)
library(ggplot2)

# Cell 2
international_box <- read.csv(".../intern_application_prithvi_project-dev/api_data_files/international_box.csv")

# Cell 3
head(international_box)

# Cell 4
str(international_box)

# Cell 5
summary(international_box)

# Cell 6
international_box$Date <- mdy(international_box$Date)

# Cell 7
str(international_box)

# Cell 8
ggplot(international_box, aes(x=Date, y=PTS)) +
  geom_line() +
  labs(title="Points over Time", x="Date", y="Points")

# Cell 9
ggplot(international_box, aes(x=Date, y=AST)) +
  geom_line() +
  labs(title="Assists over Time", x="Date", y="Assists")

# Cell 10
ggplot(international_box, aes(x=Date, y=REB)) +
  geom_line() +
  labs(title="Rebounds over Time", x="Date", y="Rebounds")

# Cell 11
ggplot(international_box, aes(x=Date, y=STL)) +
  geom_line() +
  labs(title="Steals over Time", x="Date", y="Steals")

# Cell 12
ggplot(international_box, aes(x=Date, y=BLK)) +
  geom_line() +
  labs(title="Blocks over Time", x="Date", y="Blocks")

# Cell 13
ggplot(international_box, aes(x=Date, y=TOV)) +
  geom_line() +
  labs(title="Turnovers over Time", x="Date", y="Turnovers")

# Cell 14
ggplot(international_box, aes(x=Date, y=PF)) +
  geom_line() +
  labs(title="Personal Fouls over Time", x="Date", y="Personal Fouls")

# Cell 15
ggplot(international_box, aes(x=Date, y=PTS, color=Player)) +
  geom_line() +
  labs(title="Points over Time by Player", x="Date", y="Points")

# Cell 16
ggplot(international_box, aes(x=Date, y=AST, color=Player)) +
  geom_line() +
  labs(title="Assists over Time by Player", x="Date", y="Assists")

# Cell 17
ggplot(international_box, aes(x=Date, y=REB, color=Player)) +
  geom_line() +
  labs(title="Rebounds over Time by Player", x="Date", y="Rebounds")

# Cell 18
ggplot(international_box, aes(x=Date, y=STL, color=Player)) +
  geom_line() +
  labs(title="Steals over Time by Player", x="Date", y="Steals")

# Cell 19
ggplot(international_box, aes(x=Date, y=BLK, color=Player)) +
  geom_line() +
  labs(title="Blocks over Time by Player", x="Date", y="Blocks")

# Cell 20
ggplot(international_box, aes(x=Date, y=TOV, color=Player)) +
  geom_line() +
  labs(title="Turnovers over Time by Player", x="Date", y="Turnovers")

# Cell 21
ggplot(international_box, aes(x=Date, y=PF, color=Player)) +
  geom_line() +
  labs(title="Personal Fouls over Time by Player", x="Date", y="Personal Fouls")

# Cell 22
ggplot(international_box, aes(x=Date, y=PTS/MP, color=Player)) +
  geom_line() +
  labs(title="Points per Minute over Time by Player", x="Date", y="Points per Minute")

# Cell 23
ggplot(international_box, aes(x=Date, y=AST/MP, color=Player)) +
  geom_line() +
  labs(title="Assists per Minute over Time by Player", x="Date", y="Assists per Minute")

# Cell 24
ggplot(international_box, aes(x=Date, y=REB/MP, color=Player)) +
  geom_line() +
  labs(title="Rebounds per Minute over Time by Player", x="Date", y="Rebounds per Minute")

# Cell 25
ggplot(international_box, aes(x=Date, y=STL/MP, color=Player)) +
  geom_line() +
  labs(title="Steals per Minute over Time by Player", x="Date", y="Steals per Minute")

# Cell 26
ggplot(international_box, aes(x=Date, y=BLK/MP, color=Player)) +
  geom_line() +
  labs(title="Blocks per Minute over Time by Player", x="Date", y="Blocks per Minute")

# Cell 27
ggplot(international_box, aes(x=Date, y=TOV/MP, color=Player)) +
  geom_line() +
  labs(title="Turnovers per Minute over Time by Player", x="Date", y="Turnovers per Minute")

# Cell 28
ggplot(international_box, aes(x=Date, y=PF/MP, color=Player)) +
  geom_line() +
  labs(title="Personal Fouls per Minute over Time by Player", x="Date", y="Personal Fouls per Minute")

# Cell 29
ggplot(international_box, aes(x=Date, y=PTS/G, color=Player)) +
  geom_line() +
  labs(title="Points per Game over Time by Player", x="Date", y="Points per Game")

# Cell 30
ggplot(international_box, aes(x=Date, y=AST/G, color=Player)) +
  geom_line() +
  labs(title="Assists per Game over Time by Player", x="Date", y="Assists per Game")

# Cell 31
ggplot(international_box, aes(x=Date, y=REB/G, color=Player)) +
  geom_line() +
  labs(title="Rebounds per Game over Time by Player", x="Date", y="Rebounds per Game")

# Cell 32
ggplot(international_box, aes(x=Date, y=STL/G, color=Player)) +
  geom_line() +
  labs(title="Steals per Game over Time by Player", x="Date", y="Steals per Game")

# Cell 33
ggplot(international_box, aes(x=Date, y=BLK/G, color=Player)) +
  geom_line() +
  labs(title="Blocks per Game over Time by Player", x="Date", y="Blocks per Game")

# Cell 34
ggplot(international_box, aes(x=Date, y=TOV/G, color=Player)) +
  geom_line() +
  labs(title="Turnovers per Game over Time by Player", x="Date", y="Turnovers per Game")

# Cell 35
ggplot(international_box, aes(x=Date, y=PF/G, color=Player)) +
  geom_line() +
  
  