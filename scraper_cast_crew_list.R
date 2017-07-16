

df_clean <- read.xlsx("netflix_items.xlsx", 1, stringsAsFactors=FALSE)

categories2 <- c("media", "person", "year", "position", "note")
m2 <- matrix(ncol=length(categories2)) # only 2 columns for 2 things I care about: Actor, Things they're in, and we still dont' know how long it will be, so for now I make it very long and hope it doesn't eat too much space.
colnames(m2) <- categories2
df_actors <- data.frame(m2)

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

process_cast <- function(details, df) {
  ITEM_COUNTER <- nrow(df)
  Sys.sleep(sample(10, 1) * 0.1)
  page <- tryCatch({
    read_html(paste("http://www.imdb.com/title/", as.character(details$IMDB_id), "/fullcredits?ref_=tt_ql_1", sep="")) 
  }, error = function(err) {
    return("Failure")
  })
  if (page == "Failure"){
    return(df)
  }
  
  # I notice looking at "span" gives me actors and "div" text gives me roles, duration, and year. Even better, ".listo" gives me everything.
  cast_names <- trim_IMDB_list(html_text(html_nodes(page, "a span")))
  a <- which(cast_names == "Sign in with Facebook") + 1
  b <- which(cast_names == "Amazon Video")-1
  cast_names <- cast_names[a:b]
  
  cast_roles <- trim_IMDB_list(html_text(html_nodes(page, "td div")))
  
  cast_names_roles_crew_names <- trim_IMDB_list(html_text(html_nodes(page, "td a")))
  
  production_and_crew_names <- cast_names_roles_crew_names[!(cast_names_roles_crew_names %in% c(cast_names, cast_roles))]
  # production_and_crew_names <- production_and_crew_names[1:which(production_and_crew_names == "Amazon Video" | production_and_crew_names == "Amazon VideoWatch Movies &TV Online")-1]
  
  big_list <- trim_IMDB_list(html_text(html_nodes(page, "tr td")))
  big_list <- big_list[1:which(big_list == "Amazon Video" | big_list == "Amazon VideoWatch Movies &TV Online")-1]
  
  name_list <- c(cast_names, production_and_crew_names)
  
  for (i in c(1:length(name_list))) {
    df[ITEM_COUNTER, "media"] <- details["title"]
    df[ITEM_COUNTER, "year"] <- details["year"]
    df[ITEM_COUNTER, "person"] <- name_list[i]
    if (name_list[i] %in% cast_names){
      df[ITEM_COUNTER, "position"] <- "Cast"
      rank <- which(cast_names == name_list[i])
      if (length(rank) > 1) {
        df[ITEM_COUNTER, "rank"] <- rank[length(df$person==name_list[i])]
      } else {
        df[ITEM_COUNTER, "rank"] <- rank
      }
    } else {
      df[ITEM_COUNTER, "position"] <- "Production"
      rank <- which(production_and_crew_names == name_list[i])
      if (length(rank) > 1) {
        df[ITEM_COUNTER, "rank"] <- rank[length(df$person==name_list[i])]
      } else {
        df[ITEM_COUNTER, "rank"] <- rank
      }
    }
    a <- which(big_list == name_list[i])
    a <- a[length(which(df$"person" == name_list[i]))] + 1
    if (is.na(a)){
      df[ITEM_COUNTER, "note"] <- "NA error"
      ITEM_COUNTER <- ITEM_COUNTER + 1
      next
    }
    if (i == length((name_list))){
      b <- length(big_list)
    } else {
      b <- which(big_list == name_list[i+1])  
      b <- b[length(which(df$"person" == name_list[i+1]))+1] - 1
      if (is.na(b)){
        b <- a
      }
    }
    df[ITEM_COUNTER, "note"] <- paste(big_list[a:b], collapse=", ")
    ITEM_COUNTER <- ITEM_COUNTER + 1
  }
  df
}

for (i in 1:nrow(df_clean)){
  print(i)
  df_actors <- process_cast(df_clean[i,], df_actors)
}

write.xlsx(x = df_actors, file = "netflix_imdb_cast_crew.xlsx",
           sheetName = "Sheet1")