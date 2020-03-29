every_game <- read.csv("Data/Merge Data Without Dropping Duplicates/Every_Game_With_Duplicates.csv")
height_experience <- read.csv("Data/Merge Data Without Dropping Duplicates/height_experience_combined.csv")
home_page <- read.csv("Data/Merge Data Without Dropping Duplicates/Home_Page_Combined.csv")
offense <- read.csv("Data/Merge Data Without Dropping Duplicates/msc_team_stats_offense_combined.csv")
defense <- read.csv("Data/Merge Data Without Dropping Duplicates/msc_team_stats_defense_combined.csv")
defense <- defense[,-1]
offense <- offense[,-1]
height_experience_col_names <- colnames(height_experience)
home_page_col_names <- colnames(home_page)
offense_col_names <- colnames(offense)
defense_col_names <- colnames(defense)
new_offense_col_names_team_1 <- paste("team_1_offense_", offense_col_names, sep="")
new_offense_col_names_team_2 <- paste("team_2_offense_", offense_col_names, sep="")
new_defense_col_names_team_1 <- paste("team_1_defense_", defense_col_names, sep="")
new_defense_col_names_team_2 <- paste("team_2_defense_", defense_col_names, sep="")
team_1_homepage_names <- paste("team_1_homepage_", home_page_col_names, sep="")
team_2_homepage_names <- paste("team_2_homepage_", home_page_col_names, sep="")
team_1_offense <- offense
colnames(team_1_offense) <- new_offense_col_names_team_1
team_2_offense <- offense
colnames(team_2_offense) <- new_offense_col_names_team_2
team_1_defense <- defense
colnames(team_1_defense) <- new_defense_col_names_team_1
team_2_defense <- defense
colnames(team_2_defense) <- new_defense_col_names_team_2
homepage_team_1 <- home_page
colnames(homepage_team_1) <- team_1_homepage_names
homepage_team_2 <- home_page
colnames(homepage_team_2) <- team_2_homepage_names

merged_data <- merge(every_game, homepage_team_1, by.x = "Team_1_Season_Index", by.y="team_1_homepage_Year_Index")
merged_data <- merge(merged_data, homepage_team_2, by.x = "Team_2_Season_Index", by.y = "team_2_homepage_Year_Index")


write.csv(merged_data, file.path("/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/", "schedule_homepage_duplicates.csv"), row.names = FALSE)
