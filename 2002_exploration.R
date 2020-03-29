# 2002 exploration
library(dplyr)

teams <- read.csv("Teams_2002_2019.csv")
files <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Schedule/2002/", 
                    pattern = "(.*)",
                    full.names = TRUE)
csv_2002 <- lapply(files, read.csv)
games_2002 <- do.call(rbind, csv_2002)
games_2002$team_1_win <- ifelse(games_2002$Team_1_Score > games_2002$Team_2_Score, 1, 0)
games_2002_team_1 <- games_2002
games_2002_team_1$join_column <- paste(games_2002_team_1$Date, games_2002_team_1$Team_1_Season_Index, games_2002_team_1$Team_2_Season_Index, sep="_")
games_2002_team_2 <- games_2002
games_2002_team_2$join_column <- paste(games_2002_team_2$Date, games_2002_team_2$Team_2_Season_Index, games_2002_team_2$Team_1_Season_Index, sep="_")

# function to add column
add_team <- function (x, table, team) {
  y = paste(team, colnames(table)[x], sep="_")
  colnames(table)[x] = y
}

# applying funtion
colnames(games_2002_team_1)[16:39] <- lapply(16:39, add_team, games_2002_team_1, "team_1")
colnames(games_2002_team_2)[16:39] <- lapply(16:39, add_team, games_2002_team_2, "team_2")
# Dropping unncessary columns 
games_2002_team_2 <- games_2002_team_2 %>% 
  select(.,-c(Season, Date, Date_Number, Team_1, Team_1_Unique_Identifier, Team_1_Season_Index, Team_2, 
              Team_2_Unique_Identifier, Team_2_Season_Index, Team_2_Rank, Result, Team_1_Score, Team_2_Score, 
              Location, Pace, team_1_win))

# joining columns 
all_info_2002 <- merge(games_2002_team_1, games_2002_team_2, by.x="join_column", by.y="join_column")







