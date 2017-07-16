"netflix_items.xlsx"
df <- read.xlsx("netflix_items.xlsx", 1, stringsAsFactors=FALSE)

# df <- data.frame(df, stringsAsFactors=FALSE)

expand_synopsis <- function(ID) {
  page <- tryCatch({
    read_html(paste("http://www.imdb.com/title/", ID, "/synopsis?ref_=ttqu_ql_stry_3", sep=""))
  }, error = function(err) {
    return(NULL)
  })
  if (is.null(page)){
    return("")
  }
  text <- as.character(html_text(html_nodes(page, "#swiki\\.2\\.1")))
  text
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
  new_text <- expand_synopsis(df[i, "IMDB_id"])
  df[i, "synopsis"] = paste(df[i, "synopsis"], new_text, collapse=", ")
}

write.xlsx(x = df, file = "netflix_items_expanded_synopsis.xlsx",
           sheetName = "Sheet1")