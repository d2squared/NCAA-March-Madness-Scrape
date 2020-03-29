#Homepage
library(rvest)
library(stringr)
library(tidyr)
library(dplyr)

URL <- "https://kenpom.com/index.php?y="
years <- read.csv("Years.csv", stringsAsFactors = FALSE)
team_unique_id <- read.csv("Team_Unique_Id.csv")
team_unique_id$Team <- str_squish(team_unique_id$Team)

# Get Home Page
get_home_page_pre_2020 <- function(table_name, url, year) {
  Sys.sleep(1)
  out = tryCatch({
    link = paste0(url, year, sep = "")
    ken_pom = read_html(link)
    ken_pom_table = ken_pom %>% 
      html_table(fill=TRUE)
    home_table = data.table::rbindlist(ken_pom_table, fill=TRUE)
    output = list(table_name = home_table)
    return(output)
  }, error = function(e){
    print(year)
  }) 
  closeAllConnections()
  return(out)
}

home_page <- mapply(get_home_page_pre_2020, years$Home_Page, URL, years$Year)

# Names of Tables
home_page_col_names <- c("Overall_Rank", "Team", "Conf", "W_L", "AdjEM", "AdjO", "AdjO_Rank", "AdjD", "AdjD_Rank", "AdjT", "AdjT_Rank",
                         "Home_Page_Luck", "Home_Page_Luck_Rank", "Home_Page_Strength_Of_Schedule_AdjEM", "Home_Page_Strength_Of_Schedule_AdjEM_Rank",
                         "Home_Page_Strength_Of_Schedule_Opp_O", "Home_Page_Strength_Of_Schedule_Opp_O_Rank",
                         "Home_Page_Strength_Of_Schedule_Opp_D", "Home_Page_Strength_Of_Schedule_Opp_D_Rank",
                         "Home_Page_NCSOS_AdjEM", "Home_Page_NCSOS_AdjEM_Rank")

# Ken Pom Cleaner
home_page_clean_up <- function(table, name, year) {
  out = tryCatch ({
    cleanup = table
    cleanup = cleanup[-c(1, 42, 43, 84, 85, 126, 127, 168, 169, 210, 211, 252, 253, 294, 295, 336, 337), ]
    cleanup = cleanup[,c(1:21)]
    cleanup$V2 = str_replace_all(cleanup$V2, "[0-9]+", "")
    cleanup$V2 = str_squish(cleanup$V2)
    colnames(cleanup) <- home_page_col_names
    cleanup <- cleanup %>% 
      separate(W_L, c("Wins", "Loss"), "-")
    cleanup = merge(team_unique_id, cleanup, by.x="Team", by.y = "Team")
    cleanup$Year = year
    cleanup$Year_Index = paste(year, cleanup$Unique_Identfier, sep = "_")
    output = list(name = cleanup)
    return(output)
  }, error = function(e){
    print(name)
  })
  
  return(out)
}

home_table_clean_up <- mapply(home_page_clean_up, home_page, years$Home_Page, years$Year)
Path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Home_Page More Info/"
# Write Out Home Page
make_tables_home_page <- function(x, name, table, path) {
  file_name = paste0(name[x], "_Home_Page.csv", sep="")
  path = paste0(path, sep ="")
  file = table[[x]]
  write.csv(file, file.path(path, file_name), row.names = FALSE)
}


# Stats Tab
url <- "https://kenpom.com/"
years <- read.csv("Years.csv", stringsAsFactors = FALSE)

#email triplekrown360@yahoo.com
# Password twentyfivek
url <- "https://kenpom.com/"
my_session <- html_session(url)
login <- my_session %>% 
  html_node("form[id=login]") %>% 
  html_form() %>% 
  set_values(email = "triplekrown360@yahoo.com",
             password = "twentyfivek")

logged_in <- my_session %>% 
  submit_form(login)

get_ken_pom <- function (name, url, year, other_url) {
  Sys.sleep(1)
  out = tryCatch ({
    link <- paste0(url, year, other_url, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE)
    res = data.frame(res)
    x = list(name = res)
    return(x)
  }, error = function(e){
    print(year)
  })
  closeAllConnections()
  
  return(out)
}

# Clean  Up
clean_up <- function(table, name) {
  out = tryCatch ({
    cleanup = table
    colnames(cleanup) = NULL
    colnames(cleanup) = as.character(unlist(cleanup[1,]))
    cleanup = cleanup[-c(1, 42, 43, 84, 85, 126, 127, 168, 169, 210, 211, 252, 253, 294, 295),]
    rownames(cleanup) = NULL
    cleanup$Team = str_replace_all(cleanup$Team, "[0-9]+", "")
    cleanup$Team = str_squish(cleanup$Team)
    output = list(name = cleanup)
    return(output)
  }, error = function(e){
    print(name[x])
  })
  
  return(out)
}

# Clean Up Offense Msc Team Stats
clean_up_msc <- function(table, name) {
  out = tryCatch ({
    cleanup = table
    colnames(cleanup) = NULL
    colnames(cleanup) = as.character(unlist(cleanup[41,]))
    cleanup = cleanup[-c(41, 82, 123, 164, 205, 246, 287, 328),]
    rownames(cleanup) = NULL
    cleanup$Team = str_replace_all(cleanup$Team, "[0-9]+", "")
    cleanup$Team = str_squish(cleanup$Team)
    output = list(name = cleanup)
    return(output)
  }, error = function(e){
    print(name[x])
  })
  
  return(out)
}

# Clean up Team experience
clean_up_team_experience <- function(table, name) {
  out = tryCatch ({
    cleanup = table
    colnames(cleanup) = NULL
    colnames(cleanup) = as.character(unlist(cleanup[41,]))
    cleanup = cleanup[-c(41, 82, 123, 164, 205, 246, 287),]
    rownames(cleanup) = NULL
    cleanup$Team = str_replace_all(cleanup$Team, "[0-9]+", "")
    cleanup$Team = str_squish(cleanup$Team)
    output = list(name = cleanup)
    return(output)
  }, error = function(e){
    print(name[x])
  })
  
  return(out)
}

# Clean Up Columns
clean_up_columns <- function(table, name, x,y) {
  out = tryCatch ({
    cleanup = table
    cleanup = cleanup[, x:y]
    colnames(cleanup) = NULL
    colnames(cleanup) = as.character(unlist(cleanup[1,]))
    cleanup = cleanup[-c(1, 42, 43, 84, 85, 126, 127, 168, 169, 210, 211, 252, 253, 294, 295),]
    cleanup$Team = str_replace_all(cleanup$Team, "[0-9]+", "")
    cleanup$Team = str_squish(cleanup$Team)
    output = list(name=cleanup)
    return(output)
  }, error = function(e){
    print(name[x])
  })
  
  return(out)
}


# Make Tables
make_tables <- function(x, name, table, path) {
  file_name <- paste0(name[x], ".csv", sep="")
  file = table[[x]]
  write.csv(file, file.path(path, file_name))
}

# efficiecny
efficiecny_path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Efficiency/"
efficiency <- mapply(get_ken_pom, 
                     years$Stats, 
                     "https://kenpom.com/summary.php?y=",
                     years$Year,
                     "")
efficiency_clean_up <- mapply(clean_up_columns, efficiency, years$Stats, 1, 18)
lapply(1:18, make_tables, name = years$Stats, table = efficiency_clean_up, efficiecny_path)

# Four Factors
four_factors_path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Four_Factors/"
four_factors <- mapply(get_ken_pom, 
                       years$Four_Factors, 
                       "https://kenpom.com/stats.php?y=",
                       years$Year,
                       "&s=RankAdjOE")
four_factors_clean_up <- mapply(clean_up, four_factors, years$Four_Factors)
four_factors_clean_up <- mapply(clean_up_columns, four_factors, years$Four_Factors, x=1, y=24)
lapply(1:18, make_tables, name=years$Four_Factors, table=four_factors_clean_up, four_factors_path)

# Point Distribution
point_distribution_path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Point_Distribution/"
point_distribution <- mapply(get_ken_pom,
                             years$Point_Distribution,
                             "https://kenpom.com/pointdist.php?y=",
                             years$Year,
                             "")
point_distribution_clean_up <- mapply(clean_up, point_distribution, years$Point_Distribution)
point_distribution_clean_up <- mapply(clean_up_columns, point_distribution_clean_up, years$Point_Distribution)
lapply(1:18, make_tables, name = years$Point_Distribution, table = point_distribution_clean_up, point_distribution_path)

# Offense Msc Teams Stats
offense_msc_team_stats_path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Msc_Team_Stats_Offense/"
offense_msc_team_stats <- mapply(get_ken_pom,
                                 years$Offense_Msc_Team_Stats,
                                 "https://kenpom.com/teamstats.php?s=RankFG3Pct&y=",
                                 years$Year,
                                 "")
offense_msc_team_stats_cleanup <- mapply(clean_up_msc, offense_msc_team_stats, years$Offense_Msc_Team_Stats)
lapply(1:18, make_tables, years$Offense_Msc_Team_Stats, offense_msc_team_stats_cleanup, offense_msc_team_stats_path)

# Defense Msc Team Stats
defense_msc_team_stats_path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Msc_Team_Stats_Defense/"
defense_msc_team_stats <-  mapply(get_ken_pom,
                                  years$Defense_Msc_Team_Stats,
                                  "https://kenpom.com/teamstats.php?s=RankFG3Pct&y=",
                                  years$Year,
                                  "&od=d")
defense_msc_team_stats_clean_up <- mapply(clean_up_msc, defense_msc_team_stats, years$Defense_Msc_Team_Stats)
lapply(1:18, make_tables, years$Defense_Msc_Team_Stats, defense_msc_team_stats_clean_up, defense_msc_team_stats_path)

# Height_Experience
height <- read.csv("height_experience_years.csv", stringsAsFactors = FALSE)
height_experience_path <- "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Height_Experience/"
height_exerperience <-  mapply(get_ken_pom,
                               height$Height_Experience,
                               "https://kenpom.com/height.php?s=SizeRank&y=",
                               height$Year,
                               "")
height_experience_clean_up <- mapply(clean_up_team_experience, height_exerperience, height$Height_Experience)
height_experience_clean_up <- mapply(clean_up_columns, height_experience_clean_up, height$Height_Experience, 1, 22)
lapply(1:13, make_tables, height$Height_Experience, height_experience_clean_up, height_experience_path)

# Four Factors
url <- "https://kenpom.com/"
years <- read.csv("Years.csv", stringsAsFactors = FALSE)

#email triplekrown360@yahoo.com
# Password twentyfivek
url <- "https://kenpom.com/"
my_session <- html_session(url)
login <- my_session %>% 
  html_node("form[id=login]") %>% 
  html_form() %>% 
  set_values(email = "triplekrown360@yahoo.com",
             password = "twentyfivek")

logged_in <- my_session %>% 
  submit_form(login)

get_ken_pom <- function (name, url, year, other_url) {
  Sys.sleep(.5)
  out = tryCatch ({
    link <- paste0(url, year, other_url, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE)
    res = data.frame(res)
    x = list(name = res)
    return(x)
  }, error = function(e){
    print(year)
  })
  closeAllConnections()
  
  return(out)
}

# Write out function
make_tables <- function(x, name, table, path) {
  file_name <- paste0(name[x], ".csv", sep="")
  table_get = table[[x]]
  file = table_get
  write.csv(file, file.path(path, file_name))
}

# 
clean_up <- function(table, name, x, y) {
  out = tryCatch ({
    cleanup = table
    cleanup = cleanup[,x:y]
    colnames(cleanup) = as.character(unlist(cleanup[1,]))
    cleanup = cleanup[-c(1, 42, 43, 84, 85, 126, 127, 168, 169, 210, 211, 252, 253, 294, 295), ]
    cleanup$Team = str_replace_all(cleanup$Team, "[0-9]+", "")
    cleanup$Team = str_squish(cleanup$Team)
    output = list(name = cleanup)
    return(output)
  }, error = function(e){
    print(name[x])
  })
  
  return(out)
}

# Four Factors 
four_factors <- mapply(get_ken_pom, 
                       years$Four_Factors, 
                       "https://kenpom.com/stats.php?y=",
                       years$Year,
                       "&s=RankAdjOE")
four_factors_2 <- mapply(clean_up, four_factors, years$Four_Factors, 1, 24)
lapply(1:18, make_tables, name=years$Four_Factors, table = four_factors_2, "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Four_Factors/")

# Unique Teams
files <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Home_Page/", 
                    pattern = "(.*)",
                    full.names = TRUE)
csv <- lapply(files, read.csv)
teams <- do.call(rbind, csv)
teams <- teams[,-c(3:13)]
teams <- teams[order(teams$Unique_Identfier),]
teams$Link_Name <- str_replace_all(teams$Team, " ", "+")
teams$Link <- str_replace_all(teams$Link, "'", "%27")
teams$Link <- str_replace_all(teams$Link, "&", "%26")
teams$New_Name <- str_replace_all(teams$Team, " ", " _")
teams$Team <- str_squish(teams$Team)
teams$Link_Name <- str_squish(teams$Link_Name)
teams$New_Name <- str_squish(teams$New_Name)
teams$Schedule_Name <- paste(teams$New_Name, teams$Year, "Schedule", sep = "_")
write.csv(teams, "Teams_2002_2019.csv", row.names = FALSE)

# Ken Pom Game Plan Schedule 
library(parallel)

url <- "https://kenpom.com/"
teams_unique_ID <- read.csv("Team_Unique_Id.csv", stringsAsFactors = FALSE)
my_session <- html_session(url)
login <- my_session %>% 
  html_node("form[id=login]") %>% 
  html_form() %>% 
  set_values(email = "triplekrown360@yahoo.com",
             password = "twentyfivek")

logged_in <- my_session %>% 
  submit_form(login)
teams <- read.csv("Teams_2002_2019.csv", stringsAsFactors = FALSE)

teams2 <- teams[501:6157,]
get_game_plan <- function (table_name, name, url, year) {
  Sys.sleep(1)
  out = tryCatch ({
    link <- paste0(url, name, "&y=", year, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE) %>% 
      `[[`(1)
    output = list(table_name = res)
    return(output)
  }, error = function(e){
    print(paste(name, year, sep = " "))
  })
  closeAllConnections()
  
  return(out)
}

game_plan_url <- "https://kenpom.com/gameplan.php?team="

cl <- parallel::makeCluster(detectCores() - 1)
parallel::clusterExport(cl, c("get_game_plan", "teams", "game_plan_url", "url", "my_session",
                              "login", "logged_in"))
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(stringr))
clusterEvalQ(cl, library(tidyr))
clusterEvalQ(cl, library(dplyr))
schedule_1_500 <- parallel::mcmapply(get_game_plan, teams$Schedule_Name[1:500], teams$Link[1:500], game_plan_url, teams$Year[1:500])
schedule_501_6157 <- parallel::mcmapply(get_game_plan, teams$Schedule_Name[501:6157], teams$Link[501:6157], game_plan_url, teams$Year[501:6157])
# Clean up
new_names <- c("Date", "Team_2_Rank", "Team_2", "Result", "Location", "Pace", "Offense_Eff", "Offense_Eff_Rank", "Offense_eFG", 
               "Offense_TO", "Offense_OR", "Offense_FTR", "Offense_2P_Shots", "Offense_2P_Percent", "Offense_3P_Shots", 
               "Offense_3P_Percent", "Offense_3P_Attempts_Percent", "Defense_EFF", "Defense_EFF_Rank", "Defense_eFG", "Deffense_TO",
               "Defense_OR", "Defense_FTR", "Defense_2P_Shots", "Defense_2P_Percent", "Defense_3P_Shots", "Defense_3P_Percent", 
               "Defense_3P_Attempts_Percent")

clean_up_schedule <- function(table, team_name, year, year_index, unique_identifier, table_name){
  out = tryCatch ({
    colnames(table) <- new_names
    table2 <- table[-1,]
    table2 <- table2 %>% 
      filter(!is.na(Location)) %>% 
      filter(Location != "Correlations (R x 100)") %>% 
      filter(Location != "to offensive efficiency:") %>% 
      filter(Location != "to defensive efficiency:")
    table2$Offense_3P_Attempts_Percent <- as.numeric(table2$Offense_3P_Attempts_Percent)
    table2$Offense_2P_Attempts_Percent <- 100 - table2$Offense_3P_Attempts_Percent
    table2$Defense_3P_Attempts_Percent <- as.numeric(table2$Defense_3P_Attempts_Percent)
    table2$Defense_2P_Attempts_Percent <- 100 - table2$Defense_3P_Attempts_Percent
    table2 <- separate(table2, "Result", c("a", "b"), sep = "-", remove = FALSE)
    table2$Team_1_Score = ifelse(str_detect(table2$a, "W"), str_replace_all(table2$a, "W, ", ""), table2$b)
    table2$Team_2_Score = ifelse(str_detect(table2$a, "L"), str_replace_all(table2$a, "L, ", ""), table2$b)
    table2 <- table2[,-c(5,6)]
    table2$Team_1 <- team_name
    table2$Season <- year
    table2$Team_1_Season_Index <- year_index
    table2$Team_1_Unique_Identifier <- unique_identifier
    table2$Date_Number <- str_trim(str_replace_all(table2$Date, "^\\S*", ""))
    table2$Location <- str_replace_all(table2$Location, "H", "Team_1 H")
    table2$Location <- str_replace_all(table2$Location, "^h$", "Team_1_H")
    table2$Location <- str_replace_all(table2$Location, "A", "Team_2_H")
    table2$Location <- str_replace_all(table2$Location, "^a$", "Team_2_H")
    table3 <- merge(table2, teams_unique_ID, by.x = "Team_2", by.y = "Team")
    colnames(table3)[38] <- "Team_2_Unique_Identifier"
    table3$Team_2_Season_Index <- paste(table3$Season, table3$Team_2_Unique_Identifier, sep = "_")
    table3 <- table3[c(34,2,37, 33, 36, 35, 1, 38, 39, 3, 4, 31, 32, 5:17, 29, 18:28, 30)]
    output = list(table_name=table3)
    return(output)}, error = function(e) {
      print(paste(team_name, year_index, sep = " "))
    })
  return(out)
}

debugonce(clean_up_schedule)
final_schedule_1_500 <- mapply(clean_up_schedule, schedule_1_500[1:500], teams$Team[1:500], teams$Year[1:500], teams$Year_Index[1:500],
                               teams$Unique_Identfier[1:500], teams$Schedule_Name[1:500])
final_schedule_501_6157 <- mapply(clean_up_schedule, schedule_501_6157, teams$Team[501:6157], teams$Year[501:6157], teams$Year_Index[501:6157],
                                  teams$Unique_Identfier[501:6157], teams$Schedule_Name[501:6157])

# Export

export <- function(x, name, table, path) {
  file_name_schedule = paste0(name[x], ".csv", sep = "")
  year = str_extract(name[x], "[0-9]+")
  path_schedule = paste0(path, year, "/", sep ="")
  schedule = table[[x]]
  write.csv(schedule, file.path(path_schedule, file_name_schedule), row.names = FALSE)
}
debugonce(export)
lapply(1:500, export, teams$Schedule_Name[1:500], final_schedule_1_500, 
       "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Schedule/")
lapply(1:5657, export, teams2$Schedule_Name, final_schedule_501_6157,
       "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Schedule/")

# Season Schedule
url <- "https://kenpom.com/"
Teams <- read.csv("Teams_2002_2019.csv", stringsAsFactors = FALSE)
years <- read.csv("Years.csv", stringsAsFactors = FALSE)

#email triplekrown360@yahoo.com
# Password twentyfivek
url <- "https://kenpom.com/"
my_session <- html_session(url)
login <- my_session %>% 
  html_node("form[id=login]") %>% 
  html_form() %>% 
  set_values(email = "triplekrown360@yahoo.com",
             password = "twentyfivek")

logged_in <- my_session %>% 
  submit_form(login)

test <- logged_in %>% 
  jump_to("https://kenpom.com/team.php?team=Air+Force&y=2019") %>% 
  read_html()

test2 <- test %>% 
  html_table(fill=TRUE) %>% 
  `[[`(2)

test3 <- test2[, -c(6:7,11)]

# Game Plan Schedule

url <- "https://kenpom.com/"
my_session <- html_session(url)
login <- my_session %>% 
  html_node("form[id=login]") %>% 
  html_form() %>% 
  set_values(email = "triplekrown360@yahoo.com",
             password = "twentyfivek")

logged_in <- my_session %>% 
  submit_form(login)
years <- read.csv("Years.csv", stringsAsFactors = FALSE)
teams <- read.csv("2020_Ken_Pom_Home_Page.csv")
teams <- teams[-c(1, 42, 43, 84, 85, 123, 126:127, 168:169, 210:211, 252:253, 294:295, 336:337),]
teams <- data.frame(teams[, 3])
colnames(teams) <- "Team"
teams$Team <- str_squish(teams$Team)
teams$link <- str_replace_all(teams$Team, " ", "+")
# Way data comes in 
teams$link <- str_replace_all(teams$link, "'", "%27")

# Function
get_game_plan <- function (table_name, name, url, year) {
  Sys.sleep(1)
  out = tryCatch ({
    link <- paste0(url, name, "&y=", year, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE)
    output = list(schedule = res[[1]], points = res[[2]], off_rebounds = res[[3]], def_robounds = res[[4]])
    x = list(table_name = output)
    return(x)
  }, error = function(e){
    print(paste(name, year, sep = " "))
  })
  closeAllConnections()
  
  return(out)
}

get_game_plan_pre_2006 <- function (table_name, name, url, year) {
  Sys.sleep(1)
  out = tryCatch ({
    link <- paste0(url, name, "&y=", year, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE)
    output = list(schedule = res)
    x = list(table_name = output)
    return(x)
  }, error = function(e){
    print(paste(name, year, sep = " "))
  })
  closeAllConnections()
  
  return(out)
}

# Game Plan
game_plan_url <- "https://kenpom.com/gameplan.php?team="

# 2019 team info 
teams$year_2019 <- paste(teams$Team, "2019", sep = "_")
teams$year_2019 <- str_replace_all(teams$year_2019, "\\.", "")
teams$year_2019 <- str_replace_all(teams$year_2019, " ", "_")
teams$year_2018 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2018")
teams$year_2017 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2017")
teams$year_2016 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2016")
teams$year_2015 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2015")
teams$year_2014 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2014")
teams$year_2013 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2013")
teams$year_2012 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2012")
teams$year_2011 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2011")
teams$year_2010 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2010")
teams$year_2009 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2009")
teams$year_2008 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2008")
teams$year_2007 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2007")
teams$year_2006 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2006")
teams$year_2005 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2005")
teams$year_2004 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2004")
teams$year_2003 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2003")
teams$year_2002 <- teams$year_2019 %>% 
  str_replace_all(., "2019", "2002")
#test <- mapply(get_game_plan_pre_2006, teams$year_2006[1], teams$link[1], game_plan_url, "2006", SIMPLIFY = FALSE)
write.csv(teams, "teams_ken_pom.csv")
# Parallel
cl <- parallel::makeCluster(detectCores() - 1)
parallel::clusterExport(cl, c("get_game_plan", "teams", "game_plan_url", "years", "url", "my_session",
                              "login", "logged_in", "get_game_plan_pre_2006"))
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(stringr))
clusterEvalQ(cl, library(tidyr))
clusterEvalQ(cl, library(dplyr))
team_info_2019 <- parallel::mcmapply(get_game_plan, teams$year_2019, teams$link, game_plan_url, "2019", SIMPLIFY = FALSE)
team_info_2018 <- parallel::mcmapply(get_game_plan, teams$year_2018, teams$link, game_plan_url, "2018", SIMPLIFY = FALSE)
team_info_2017 <- parallel::mcmapply(get_game_plan, teams$year_2017, teams$link, game_plan_url, "2017", SIMPLIFY = FALSE)
team_info_2016 <- parallel::mcmapply(get_game_plan, teams$year_2016, teams$link, game_plan_url, "2016", SIMPLIFY = FALSE)
team_info_2015 <- parallel::mcmapply(get_game_plan, teams$year_2015, teams$link, game_plan_url, "2015", SIMPLIFY = FALSE)
team_info_2014 <- parallel::mcmapply(get_game_plan, teams$year_2014, teams$link, game_plan_url, "2014", SIMPLIFY = FALSE)
team_info_2013 <- parallel::mcmapply(get_game_plan, teams$year_2013, teams$link, game_plan_url, "2013", SIMPLIFY = FALSE)
team_info_2012 <- parallel::mcmapply(get_game_plan, teams$year_2012, teams$link, game_plan_url, "2012", SIMPLIFY = FALSE)
team_info_2011 <- parallel::mcmapply(get_game_plan, teams$year_2011, teams$link, game_plan_url, "2011", SIMPLIFY = FALSE)
team_info_2010 <- parallel::mcmapply(get_game_plan, teams$year_2010, teams$link, game_plan_url, "2010", SIMPLIFY = FALSE)
team_info_2009 <- parallel::mcmapply(get_game_plan, teams$year_2009, teams$link, game_plan_url, "2009", SIMPLIFY = FALSE)
team_info_2008 <- parallel::mcmapply(get_game_plan, teams$year_2008, teams$link, game_plan_url, "2008", SIMPLIFY = FALSE)
team_info_2007 <- parallel::mcmapply(get_game_plan, teams$year_2007, teams$link, game_plan_url, "2007", SIMPLIFY = FALSE)
team_info_2006 <- parallel::mcmapply(get_game_plan_pre_2006, teams$year_2006, teams$link, game_plan_url, "2006", SIMPLIFY = FALSE)
team_info_2005 <- parallel::mcmapply(get_game_plan_pre_2006, teams$year_2005, teams$link, game_plan_url, "2005", SIMPLIFY = FALSE)
team_info_2004 <- parallel::mcmapply(get_game_plan_pre_2006, teams$year_2004, teams$link, game_plan_url, "2004", SIMPLIFY = FALSE)
team_info_2003 <- parallel::mcmapply(get_game_plan_pre_2006, teams$year_2003, teams$link, game_plan_url, "2003", SIMPLIFY = FALSE)
team_info_2002 <- parallel::mcmapply(get_game_plan_pre_2006, teams$year_2002, teams$link, game_plan_url, "2002", SIMPLIFY = FALSE)

# Exporting
"/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2019/Schedule"
make_tables <- function(x, name, table, path) {
  file_name_schedule = paste0(name[x], "_schedule.csv", sep="")
  file_name_points = paste0(name[x], "_points.csv", sep="")
  file_name_off_rebounds = paste0(name[x], "_off_rebounds.csv", sep ="")
  file_name_def_rebounds = paste0(name[x], "_def_rebounds.csv", sep="")
  path_schedule = paste0(path, "Schedule", sep = "")
  path_points = paste0(path, "Points", sep = "")
  path_off_rebounds = paste0(path, "Def_Rebounds", sep = "")
  path_def_rebounds = paste0(path, "Off_Rebounds", sep = "")
  schedule = table[[x]][["table_name"]][[1]]
  points = table[[x]][["table_name"]][[2]]
  off_rebounds = table[[x]][["table_name"]][[3]]
  def_rebounds = table[[x]][["table_name"]][[4]]
  write.csv(schedule, file.path(path_schedule, file_name_schedule))
  write.csv(schedule, file.path(path_points, file_name_points))
  write.csv(schedule, file.path(path_off_rebounds, file_name_off_rebounds))
  write.csv(schedule, file.path(path_def_rebounds, file_name_def_rebounds))
}

# Exprorting pre 2006
# check This
make_tables_pre_2006 <- function(x, name, table, path) {
  file_name_schedule = paste0(name[x], "_schedule.csv", sep="")
  path_scheudle= paste0(path, "Schedule", sep ="")
  schedule = team_info_2006[[table[x]]][[1]][[1]][[1]]
  write.csv(schedule, file.path(path_scheudle, file_name_schedule))
}


#2019
lapply(1:352, make_tables, name = teams$year_2019, table = team_info_2019, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2019/")
#2018
lapply(1:352, make_tables, name = teams$year_2018, table = team_info_2018, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2018/")
#2017
lapply(1:352, make_tables, name = teams$year_2017, table = team_info_2017, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2017/")
#2016
lapply(1:352, make_tables, name = teams$year_2016, table = team_info_2016, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2016/")
#2015
lapply(1:352, make_tables, name = teams$year_2015, table = team_info_2015, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2015/")
#2014
lapply(1:352, make_tables, name = teams$year_2014, table = team_info_2014, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2014/")
#2013
lapply(1:352, make_tables, name = teams$year_2013, table = team_info_2013, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2013/")
#2012
lapply(1:352, make_tables, name = teams$year_2012, table = team_info_2012, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2012/")
#2011
lapply(1:352, make_tables, name = teams$year_2011, table = team_info_2011, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2011/")
#2010
lapply(1:352, make_tables, name = teams$year_2010, table = team_info_2010, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2010/")
#2009
lapply(1:352, make_tables, name = teams$year_2009, table = team_info_2009, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2009/")
#2008
lapply(1:352, make_tables, name = teams$year_2008, table = team_info_2008, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2008/")
#2007
lapply(1:352, make_tables, name = teams$year_2007, table = team_info_2007, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2007/")
#2006
lapply(1:352, make_tables_pre_2006, name = teams$year_2006, table = team_info_2006, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2006/")
#2005
lapply(1:352, make_tables_pre_2006, name = teams$year_2005, table = team_info_2005, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2005/")
#2004
lapply(1:352, make_tables_pre_2006, name = teams$year_2004, table = team_info_2004, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2004/")
#2003
lapply(1:352, make_tables, name = teams$year_2003, table = team_info_2003, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2003/")
#2003
lapply(1:352, make_tables, name = teams$year_2002, table = team_info_2002, 
       path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Gameplan/2002/")

# Combining Season Features:
Unique_ID <- read.csv("Team_Unique_Id.csv")
years <- read.csv("Years.csv")

# New Names
four_factors_col_names <- c("Team", "Conf", "FF_AdjTempo", "FF_Adj_Tempo_Rank", "FF_Offense_AdjOE",
                            "FF_Offense_AdjOE_Rank", "FF_Offense_eFG", "FF_Offense_eFG_Rank", "FF_Offense_TO", 
                            "FF_Offense_TO_Rank", "FF_Offense_OR", "FF_Offense_OR_Rank", "FF_Offense_FTRate", "FF_FTRate_Rank",
                            "FF_Deffense_AdjOE", "FF_Deffense_AdjOE_Rank", "FF_Deffense_eFG", "FF_Deffense_eFG_Rank", 
                            "FF_Deffense_TO", "FF_Deffense_TO_Rank", "FF_Deffense_OR", "FF_Deffense_OR_Rank", 
                            "FF_Defense_FTRate", "FF_Defense_FTRate_Rank")
experience_col_names <- c("Team", "Experience_Experience", "Experience_Experience_Rank",
                          "Experience_Bench", "Experience_Bench_Rank", "Experience_Continuity",
                          "Experience_Continuity_Rank")
offense_col_names <- c("Team", "Offense_3P", "Offense_3P_Rank", "Offense_2P", "Offense_2P_Rank",
                       "Offense_FT", "Offense_FT_Rank", "Offense_Blk", "Offense_Blk_Rank", 
                       "Offense_Stl", "Offense_Stl_Rank", "Offense_NST", "Offense_NST_Rank",
                       "Offense_A", "Offense_A_Rank", "Offense_3PA", "Offense_3PA_Rank",
                       "Offense_AdjOE", "Offense_AdjOE_Rank")
defense_col_names <- c("Team", "Defense_3P", "Defense_3P_Rank", "Defense_2P", 'Defense_2P_Rank',
                       "Defense_FT", "Defense_FT_Rank", "Defense_Blk", "Defense_Blk_Rank",
                       "Defense_Stl", "Defense_Stl_Rank", "Defense_NST", "Defense_NST_Rank",
                       "Defense_A", "Defense_A_Rank", "Defense_3PA", "Defense_3PA_Rank",
                       "Defense_AdjDE", "Defense_AdjDE_Rank")
efficiency_col_names <- c("Team", "Efficiency_Tempo_Adjusted", "Efficiency_Tempo_Adjusted_Rank",
                          "Efficiency_Tempo_Raw", "Efficiecny_Tempo_Raw_Rank", "Efficiency_Offense_Avg_Poss_Length",
                          "Efficiency_Offense_Avg_Poss_Length_Rank", "Efficiency_Deffense_Avg_Poss_Length",
                          "Efficiency_Deffense_Avg_Poss_Length_Rank", "Efficiecny_Offense_Efficiecny_Adjusted",
                          "Efficiecny_Offense_Efficiecny_Adjusted_Rank", "Efficiecny_Offense_Efficiecny_Raw",
                          "Efficiecny_Offense_Efficiency_Raw_Rank", "Efficiecny_Deffense_Efficiecny_Adjusted",
                          "Efficiecny_Dffense_Efficiecny_Adjusted_Rank", "Efficiecny_Defense_Raw", 
                          "Efficiecny_Defense_Raw_Rank")
home_page_col_names <- c("Overall_Rank", "Team", "Home_Page_Luck", "Home_Page_Luck_Rank", 
                         "Home_Page_Strength_Of_Schedule_AdjEM", "Home_Page_Strength_Of_Schedule_AdjEM_Rank",
                         "Home_Page_Strength_Of_Schedule_Opp_O", "Home_Page_Strength_Of_Schedule_Opp_O_Rank",
                         "Home_Page_Strength_Of_Schedule_Opp_D", "Home_Page_Strength_Of_Schedule_Opp_D_Rank",
                         "Home_Page_NCSOS_AdjEM", "Home_Page_NCSOS_AdjEM_Rank")

# Importing Names of Files
four_factors_path <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Four_Factors/", 
                                pattern = "(.*)",
                                full.names = TRUE)
experience_path <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Height_Experience/", 
                              pattern = "(.*)",
                              full.names = TRUE)
offense_path <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Msc_Team_Stats_Offense/", 
                           pattern = "(.*)",
                           full.names = TRUE)
Defense_path <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Msc_Team_Stats_Defense/", 
                           pattern = "(.*)",
                           full.names = TRUE)
Efficiency_path <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Efficiency/", 
                              pattern = "(.*)",
                              full.names = TRUE)
Home_Page_path <- list.files(path = "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/Home_Page/",
                             pattern = "(.*)",
                             full.names = TRUE)

# Ken Pom Game Plan 
Teams <- read.csv("Teams_2002_2019.csv", stringsAsFactors = FALSE)
Teams_Post_2006 <- Teams %>% 
  filter(., Year > 2006)
Teams_Pre_2006 <- Teams %>% 
  filter(., Year < 2007)
url <- "https://kenpom.com/"
my_session <- html_session(url)
login <- my_session %>% 
  html_node("form[id=login]") %>% 
  html_form() %>% 
  set_values(email = "triplekrown360@yahoo.com",
             password = "twentyfivek")

logged_in <- my_session %>% 
  submit_form(login)

#get_game_plan
game_plan_url <- "https://kenpom.com/gameplan.php?team="
get_game_plan <- function (table_name, name, url, year) {
  Sys.sleep(1.05)
  out = tryCatch ({
    link <- paste0(url, name, "&y=", year, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE)
    output = list(schedule = res[[1]], points = res[[2]], off_rebounds = res[[3]], def_robounds = res[[4]])
    x = list(table_name = output)
    return(x)
  }, error = function(e){
    print(paste(name, year, sep = " "))
  })
  closeAllConnections()
  
  return(out)
}

# Game Plan Pre 2006
get_game_plan_pre_2006 <- function (table_name, name, url, year) {
  Sys.sleep(1)
  out = tryCatch ({
    link <- paste0(url, name, "&y=", year, sep = "")
    res <- logged_in %>% 
      jump_to(link) %>% 
      read_html()
    res <- res %>%
      html_table(fill=TRUE)
    x = res
    return(x)
  }, error = function(e){
    print(paste(name, year, sep = " "))
  })
  closeAllConnections()
  
  return(out)
}

# Exporting Post 2006
export_post_2006 <- function(x, name, table, path) {
  out = tryCatch({
    file_name_schedule = paste0(name[x], "_schedule.csv", sep="")
    file_name_points = paste0(name[x], "_points.csv", sep="")
    file_name_off_rebounds = paste0(name[x], "_off_rebounds.csv", sep ="")
    file_name_def_rebounds = paste0(name[x], "_def_rebounds.csv", sep="")
    year = str_extract(name[x], "[0-9]+")
    path_schedule = paste0(path, "Game_Plan_Schedule/", year, "/", sep ="")
    path_points = paste0(path, "Game_Plan_Points/", year, "/", sep ="")
    path_off_rebounds = paste0(path, "Game_Plan_Off_Rebounds/", year, "/", sep ="")
    path_def_rebounds = paste0(path, "Game_Plan_Def_Rebounds/", year, "/", sep ="")
    schedule = table[[x]][["table_name"]][["schedule"]]
    points = table[[x]][["table_name"]][["points"]]
    off_rebounds = table[[x]][["table_name"]][["off_rebounds"]]
    def_rebounds = table[[x]][["table_name"]][["def_rebounds"]]
    write.csv(schedule, file.path(path_schedule, file_name_schedule))
    write.csv(points, file.path(path_points, file_name_points))
    write.csv(off_rebounds, file.path(path_off_rebounds, file_name_off_rebounds))
    write.csv(def_rebounds, file.path(path_def_rebounds, file_name_def_rebounds))
    success = paste0("Success: ", name[x], sep=" ")
    return(success)
  }, error = function(e){
    failure = paste0("Failure: ", name[x], sep= " ")
    return(failure)
  })
}

# Exporting Pre 2006
"/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/"
export_pre_2006 <- function(x, name, table, path) {
  file_name_schedule = paste0(name[x], "_schedule.csv", sep="")
  year = str_extract(name[x], "[0-9]+")
  path_scheudle= paste0(path, "Game_Plan_Schedule/", year, "/", sep ="")
  schedule = table[[name[x]]][[1]]
  write.csv(schedule, file.path(path_scheudle, file_name_schedule))
}

# Parallel
cl <- parallel::makeCluster(detectCores() - 1)
parallel::clusterExport(cl, c("get_game_plan", "Teams_Post_2006", "game_plan_url", "url", "my_session",
                              "login", "logged_in", "get_game_plan_pre_2006", "Teams_Pre_2006"))
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(stringr))
clusterEvalQ(cl, library(tidyr))
clusterEvalQ(cl, library(dplyr))
# Parallel Post 2006
Teams_Post_2006_Results <- parallel::mcmapply(get_game_plan, Teams_Post_2006$New_Name, 
                                              Teams_Post_2006$Game_Plan_Link,
                                              game_plan_url,
                                              Teams_Post_2006$Year,
                                              SIMPLIFY = FALSE)
# Parallel Pre 2006
Teams_Pre_2006_Results <- parallel::mcmapply(get_game_plan_pre_2006, Teams_Pre_2006$New_Name, 
                                             Teams_Pre_2006$Game_Plan_Link,
                                             game_plan_url,
                                             Teams_Pre_2006$Year,
                                             SIMPLIFY = FALSE)

# Exporting
post_2006 <- lapply(1:4400, export_post_2006, 
                    Teams_Post_2006$New_Name, 
                    game_plan_post_2006, 
                    "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/")
lapply(1:1757, export_pre_2006, 
       Teams_Pre_2006$New_Name,
       Teams_Pre_2006_Results,
       "/Users/joeyj/Desktop/Desktop/R/March Madness/Ken Pom/Data/")









