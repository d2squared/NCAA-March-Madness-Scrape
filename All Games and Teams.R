library(dplyr)
library(plyr)
years <- read.csv("Years.csv", stringsAsFactors = FALSE)
get_teams <- function(x, years, path_to_file) {
  year = years[x]
  final_path = paste(path_to_file, year, sep="")
  files = list.files(path = final_path,
                     pattern ="(.*)",
                     full.names = TRUE)
}

file_list <- lapply(1:18, get_teams, years$Year, "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Schedule/")

get_files <- function(x, table) {
  file = table[[x]]
  input_files = lapply(file, read.csv)
}

all_years <- lapply(1:18, get_files, file_list)
all_years_final <- do.call(rbind.fill, all_years2)
all_years_final <- all_years_final %>% 
  select(-x)
write.csv(all_years_final, file.path("/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/", "Every_Game_With_Duplicates.csv"), row.names = FALSE)

# Home Page
home_page_files <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Home_Page More Info/", 
                    pattern = "(.*)",
                    full.names = TRUE)
home_page_csv <- lapply(home_page_files, read.csv)
home_page_combined <- do.call(rbind, home_page_csv)
write.csv(home_page_combined, file.path("/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/", "Home_Page_Combined.csv"), row.names = FALSE)

# Height and Experience
height_experience_files <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Height_Experience/", 
                              pattern = "(.*)",
                              full.names = TRUE)
height_experience_csv <- lapply(height_experience_files, read.csv)
height_experience_combined <- do.call(rbind.fill, height_experience_csv)
height_experience_combined <- height_experience_combined[,-c(21:23)]
write.csv(height_experience_combined, file.path("/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/", "height_experience_combined.csv"), row.names = FALSE)

# Msc_Team_Stats_Defense
msc_team_stats_defense_files <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Msc_Team_Stats_Defense/", 
                                pattern = "(.*)",
                                full.names = TRUE)
msc_team_stats_defense_csv <- lapply(msc_team_stats_defense_files, read.csv)
msc_team_stats_defense_combined <- do.call(rbind, msc_team_stats_defense_csv)
write.csv(msc_team_stats_defense_combined, file.path("/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/", "msc_team_stats_defense_combined.csv"), row.names = FALSE)

# Msc_Team_Stats_Offense
msc_team_stats_offense_files <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Msc_Team_Stats_Offense/", 
           pattern = "(.*)",
           full.names = TRUE)
msc_team_stats_offense_csv <- lapply(msc_team_stats_offense_files, read.csv)
msc_team_stats_offense_combined <- do.call(rbind, msc_team_stats_offense_csv)
write.csv(msc_team_stats_offense_combined, file.path("/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/", "msc_team_stats_offense_combined.csv"), row.names = FALSE)


