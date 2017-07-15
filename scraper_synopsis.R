
df <- read.xlsx("netflix_items.xlsx", 1)

expand_synopsis <- function(details) {
  page <- tryCatch({
    read_html(paste("http://www.imdb.com/title/", details["IMDB id"], "/synopsis?ref_=ttqu_ql_stry_3", sep=""))
  }, error = function(err) {
    return("Failure")
  })
  if (page == "Failure"){
    return("")
  }
  html_text(html_nodes(page, "#swiki\\.2\\.1"))
}

trim_IMDB_list <- function(input) {
  input <- trimws(unlist(strsplit(input, "\n", fixed = TRUE)))
  return <- c()
  for (i in c(1:length(input))) {
    if (input[i] == "" | input[i] == "/ ..." | input[i] == "..." | nchar(input[i]) < 1) {
      next
    } else {
      return <- c(return, input[i])
    }
  }
  return
}



for (i in 1:nrow(df_clean)){
  print(i)
  df[i, "synopsis"] <- paste(df[i, "synopsis"], expand_synopsis(df[i,]), collapse=", ")
}

write.xlsx(x = df_clean, file = "netflix_items_expanded_synopsis.xlsx",
           sheetName = "Sheet1")