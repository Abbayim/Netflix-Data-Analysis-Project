library(wru)

census.key <- as.character(read.csv("keys.csv")[1, "key"])
name.frame <- read.xlsx("netflix_imdb_cast_crew.xlsx", 1)

total_names <- nrow(name.frame)

for (i in c(1:total_names)) {
  name <- as.character(name.frame[i, "person"])
  name_list <- strsplit(name, " ")[[1]]
  name_list <- name_list[name_list != "Jr." & name_list != "Sr." & name_list != "I" & name_list != "II" & name_list != "III" & name_list != "IV"]
  surname <- toupper(name_list[length(name_list)])
  name_index_2010 <- which(surnames$surname == surname)
  name_index_2000 <- which(surnames2000$surname == surname)
  if (length(name_index_2000) == 0 & length(name_index_2010) == 0) {
    next
  } else if (length(name_index_2000) == 0 & length(name_index_2010) != 0) {
    name.frame[i, "p_whi"] <- surnames[name_index_2010, 2]
    name.frame[i, "p_bla"] <- surnames[name_index_2010, 3]
    name.frame[i, "p_his"] <- surnames[name_index_2010,4]
    name.frame[i, "p_asi"] <- surnames[name_index_2010,5]
    name.frame[i, "p_oth"] <- surnames[name_index_2010,6]
  } else if(length(name_index_2000) != 0 & length(name_index_2010) == 0) {
    name.frame[i, "p_whi"] <- surnames2000[name_index_2000, 2]
    name.frame[i, "p_bla"] <- surnames2000[name_index_2000, 3]
    name.frame[i, "p_his"] <- surnames2000[name_index_2000, 4]
    name.frame[i, "p_asi"] <- surnames2000[name_index_2000, 5]
    name.frame[i, "p_oth"] <- surnames2000[name_index_2000, 6]
  } else {
    name.frame[i, "p_whi"] <- mean(surnames[name_index_2010,2], surnames2000[name_index_2000, 2])
    name.frame[i, "p_bla"] <- mean(surnames[name_index_2010,3], surnames2000[name_index_2000, 3])
    name.frame[i, "p_his"] <- mean(surnames[name_index_2010,4], surnames2000[name_index_2000, 4])
    name.frame[i, "p_asi"] <- mean(surnames[name_index_2010,5], surnames2000[name_index_2000, 5])
    name.frame[i, "p_oth"] <- mean(surnames[name_index_2010,6], surnames2000[name_index_2000, 6])
  }
}

write.xlsx(x = name.frame, file = "racial_compositions.xlsx",
           sheetName = "Sheet1")

