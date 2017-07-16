library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

SERVICE_USERNAME_PASSWORD = as.character(read.csv("keys.csv")[2, "key"])

df <- read.xlsx("netflix_items_expanded_synopsis.xlsx", 1, stringsAsFactors=FALSE)

total <- nrow(df)

for (i in c(1:total)) {
  analysis <- cognizer::text_tone(as.character(df[i,"synopsis"]), SERVICE_USERNAME_PASSWORD)
  analysis_dic <- strsplit(unlist(analysis), " ")
  df[i, "anger"] <- as.double(analysis_dic$document_tone.tone_categories.tones.score1)
  df[i, "disgust"] <- as.double(analysis_dic$document_tone.tone_categories.tones.score2)
  df[i, "fear"] <- as.double(analysis_dic$document_tone.tone_categories.tones.score3)
  df[i, "joy"] <- as.double(analysis_dic$document_tone.tone_categories.tones.score4)
  df[i, "sadness"] <- as.double(analysis_dic$document_tone.tone_categories.tones.score5)
  
  denom <- df[i, "anger"] + df[i, "disgust"] + df[i, "fear"] + df[i, "joy"] + df[i, "sadness"]
  
  df[i, "anger_norm"] <- df[i,"anger"]/denom
  df[i, "disgust_norm"] <- df[i, "disgust"]/denom
  df[i, "fear_norm"] <- df[i,"fear"]/denom
  df[i, "joy_norm"] <- df[i, "joy"]/denom
  df[i, "sadness_norm"] <- df[i, "sadness"]/denom
  
  df
}

write.xlsx(x = df, file = "netflix_items_watson_analysis.xlsx",
           sheetName = "Sheet1")