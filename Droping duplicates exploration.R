villanova1 <- all_info_2002 %>% 
  filter(Team_1 == "Villanova")
villanova2 <- all_info_2002 %>% 
  filter(Team_2 == "Villanova")
villanova_all <- rbind(villanova1, villanova2)
test_1 <- villanova_all %>% 
  filter(Team_1 == "Bucknell")
test_2 <- villanova_all %>% 
  filter(Team_2 == "Bucknell")
test3 <- rbind(test_1, test_2)
test4 <- test3[,1:20]
test4$Drop_1 <- paste(test4$Date_Number, test4$Team_1, test4$Team_2, sep="_")
test4$Drop_2 <- paste(test4$Date_Number, test4$Team_2, test4$Team_1, sep="_")
test4$Drop_final <- paste(test4$Drop_1, test4$Drop_2, sep="_")
