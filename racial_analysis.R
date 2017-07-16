item.frame <- read.xlsx("netflix_items_watson_analysis.xlsx", 1, stringsAsFactors=FALSE)
name.frame <- read.xlsx("racial_compositions.xlsx", 1, stringsAsFactors=FALSE)

for (i in (c(1:nrow(item.frame)))){
  title <- as.character(item.frame[i,"title"])
  people <- name.frame[which(name.frame$media == title & name.frame$year == item.frame[i, "year"]), ]
  cast <- people[which(name.frame$position == "Cast"), ]
  crew <- people[which(name.frame$position == "Production"), ]
  item.frame[i, "p_whi"] = mean(as.double(people$p_whi), na.rm = TRUE)
  item.frame[i, "p_whi_cast"] = mean(as.double(cast$p_whi), na.rm = TRUE)
  item.frame[i, "p_whi_crew"] = mean(as.double(crew$p_whi), na.rm = TRUE)
  item.frame[i, "p_bla"] = mean(as.double(people$p_bla), na.rm = TRUE)
  item.frame[i, "p_bla_cast"] = mean(as.double(cast$p_bla), na.rm = TRUE)
  item.frame[i, "p_bla_crew"] = mean(as.double(crew$p_bla), na.rm = TRUE)
  item.frame[i, "p_his"] = mean(as.double(people$p_his), na.rm = TRUE)
  item.frame[i, "p_his_cast"] = mean(as.double(cast$p_his), na.rm = TRUE)
  item.frame[i, "p_his_crew"] = mean(as.double(crew$p_his), na.rm = TRUE)
  item.frame[i, "p_asi"] = mean(as.double(people$p_asi), na.rm = TRUE)
  item.frame[i, "p_asi_cast"] = mean(as.double(cast$p_asi), na.rm = TRUE)
  item.frame[i, "p_asi_crew"] = mean(as.double(crew$p_asi), na.rm = TRUE)
  item.frame[i, "p_oth"] = mean(as.double(people$p_oth), na.rm = TRUE)
  item.frame[i, "p_oth_cast"] = mean(as.double(cast$p_oth), na.rm = TRUE)
  item.frame[i, "p_oth_crew"] = mean(as.double(crew$p_oth), na.rm = TRUE)
}

item.frame$p_whi_ranked <- rank(item.frame$p_whi)
item.frame$p_whi_cast_ranked <- rank(item.frame$p_whi_cast)
item.frame$p_whi_crew_ranked <- rank(item.frame$p_whi_crew)

item.frame$p_bla_ranked <- rank(item.frame$p_bla)
item.frame$p_bla_cast_ranked <- rank(item.frame$p_bla_cast)
item.frame$p_bla_crew_ranked <- rank(item.frame$p_bla_crew)

item.frame$p_his_ranked <- rank(item.frame$p_his)
item.frame$p_his_cast_ranked <- rank(item.frame$p_his_cast)
item.frame$p_his_crew_ranked <- rank(item.frame$p_his_crew)

item.frame$p_asi_ranked <- rank(item.frame$p_asi)
item.frame$p_asi_cast_ranked <- rank(item.frame$p_asi_cast)
item.frame$p_asi_crew_ranked <- rank(item.frame$p_asi_crew)

item.frame$p_oth_ranked <- rank(item.frame$p_oth)
item.frame$p_oth_cast_ranked <- rank(item.frame$p_oth_cast)
item.frame$p_oth_crew_ranked <- rank(item.frame$p_oth_crew)


write.xlsx(x = item.frame, file = "item_tone_racial_aggregates.xlsx",
           sheetName = "Sheet1")
