words <- colnames(read.csv("poverty_word_list.csv"))

df <- read.xlsx("item_tone_racial_aggregates.xlsx", 1)

for(i in c(1:nrow(df))){
  synopsis_list <- strsplit(tolower(as.character(df[i, "synopsis"])), " ")[[1]]
  numer <- length(which(synopsis_list %in% words))
  denom <- length(which(!(synopsis_list %in% words)))
  df[i, "poverty_1"] <- numer/denom
  synopsis_list <- unique(synopsis_list)
  numer <- length(which(synopsis_list %in% words))
  denom <- length(which(!(synopsis_list %in% words)))
  df[i, "poverty_2"] <- numer/denom
}

df$poverty_1_ranked <- rank(df$poverty_1)
df$poverty_2_ranked <- rank(df$poverty_2)

write.xlsx(x = df, file = "item_tone_racial_aggregates_poverty_analysis.xlsx",
           sheetName = "Sheet1")
