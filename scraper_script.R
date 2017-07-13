library(rvest, stringi, stringr)
library(xlsx)

# for execution, commented because the list itself is fat.
# every_8_digit_number <- c(10000000:999999999)

# for construction
sample_ids_movies <- c(60037627, 80108238, 60036691)
sample_ids_tv <- c(80025678, 70177083)
sample_ids_bad <- c(100000000, "10000000", "abcdefgh")
sample_ids_people <- c(20033457)
  
sample_ids <- c(sample_ids_movies, sample_ids_tv, sample_ids_bad)

id_dictionary <- sample_ids # development, will switch to every_8_digit_number when the time is right.

categories <- c("title", "year", "rating", "duration", "synopsis", "genres", "director", "starring", "cast", "creator", "episodes")

m <- matrix(ncol=length(categories), nrow=length(id_dictionary))
colnames(m) <- categories
rownames(m) <- id_dictionary
df <- data.frame(m)
df_movies <- data.frame(m)
df_tv <- data.frame(m)

categories2 <- c("media", "person", "year", "note")
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
  # Assumption: If I search <title> <year> the first result will be what I want.
  # Generate search URL on IMDB: The Crown 2016 should look like: http://www.imdb.com/find?ref_=nv_sr_fn&q=the+crown+2016&s=all
  title_formatted <- sub(pattern=" ", x=tolower(details["title"]), replacement="+")
  page <- read_html(paste("http://www.imdb.com/find?ref_=nv_sr_fn&q=", title_formatted, "+", details["year"], "&s=all", sep=""))
  result <- toString(html_nodes(page, ".result_text")[1])
  result <- strsplit(sub(pattern="href=\\\"", result, replacement=":::"), ":::")[[1]][2] # Get rid of left side
  # result <- strsplit(sub(pattern="/?", result, replacement=":::"), ":::")[[1]][1] # Get rid of right side
  IMDB_id <- strsplit(result, "/")[[1]][3]
  page <- read_html(paste("http://www.imdb.com/title/", IMDB_id, "/fullcredits?ref_=tt_ql_1", sep=""))
  # I notice looking at "span" gives me actors and "div" text gives me roles, duration, and year. Even better, ".listo" gives me everything.
  cast_names <- trim_IMDB_list(html_text(html_nodes(page, "a span")))
  a <- which(cast_names == "Sign in with Facebook") + 1
  b <- which(cast_names == "Amazon Video")-1
  print(a)
  print(b)
  cast_names <- cast_names[a:b]
  
  cast_roles <- trim_IMDB_list(html_text(html_nodes(page, "td div")))
  
  cast_names_roles_crew_names <- trim_IMDB_list(html_text(html_nodes(page, "td a")))
  
  production_and_crew_names <- cast_names_roles_crew_names[!(cast_names_roles_crew_names %in% c(cast_names, cast_roles))]
  # production_and_crew_names <- production_and_crew_names[1:which(production_and_crew_names == "Amazon Video" | production_and_crew_names == "Amazon VideoWatch Movies &TV Online")-1]
  
  big_list <- trim_IMDB_list(html_text(html_nodes(page, "tr td")))
  big_list <- big_list[1:which(big_list == "Amazon Video" | big_list == "Amazon VideoWatch Movies &TV Online")-1]
  
  
  name_list <- c(cast_names, production_and_crew_names)
  
  for (i in c(1:length(name_list))) {
    print(i)
    df[ITEM_COUNTER, "media"] <- details["title"]
    df[ITEM_COUNTER, "year"] <- details["year"]
    df[ITEM_COUNTER, "person"] <- name_list[i]
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
      print(name_list[i+1])
      b <- b[length(which(df$"person" == name_list[i+1]))+1] - 1
      if (is.na(b)){
        b <- a
      }
    }
    print(a)
    print(b)
    df[ITEM_COUNTER, "note"] <- paste(big_list[a:b], collapse=", ")
    print(df[ITEM_COUNTER,])
    ITEM_COUNTER <- ITEM_COUNTER + 1
  }
  
  df
}
  
 

process_media_page <- function(page, df) {
  info1 <- html_text(html_nodes(page, "span"))
  info2 <- html_text(html_nodes(page, "div div"))
  info3 <- html_text(html_nodes(page, "div ul"))
  if (html_text(html_nodes(page, "h3"))[1] == "MORE DETAILS"){ # IT'S A MOVIE
    df[i, "duration"] <- info1[6]
    syn <- strsplit(sub(pattern="Starring: ", strsplit(sub("[0-9]+h [0-9]+m", info2[16], replacement=":::"), ":::")[[1]][2], replacement=":::"), ":::")[[1]][1] # An obnoxious way of extracting the synopsis from the string, on the left by the duration and on the right by cast information. 
    df[i, "synopsis"] <- strsplit(sub(pattern="Genres: ", syn, replacement=":::"), ":::")[[1]][1] # Can add more of these removers if necessary. Easier to do one at a time and copy some code, might need to write function to do this later but for now this seems ok.
  } else { # IT'S A TV SHOW
    # R won't let me store a list inside a data frame. I refuse to do a list of list, which is the only solution I could find on Stack Overflow, so I'm making a fake list string and will re-list later.
    df[i, "episodes"] <- paste(html_text(html_nodes(page, "h3"))[2:(length(html_text(html_nodes(page, "h3"))) - 2)], collapse = ", ") # first item is "EPISODES", last 2 items are "MORE DETAILS" and "More TV Shows & Movies" which I don't care about storing.
    df[i, "duration"] <- paste(info1[c(grep("[0-9]+m", info1), grep("[0-9]+h [0-9]+m", info1))], collapse = ", ")
    # Snatch the synopses!
    a <- grep("Release Year: [0-9][0-9][0-9][0-9]", info2) # The items where the words "Release year: YYYY", of which the synopses of interest ot us are immediately after, consistently.
    b <- grep(".+Release Year: [0-9][0-9][0-9][0-9].+", info2) # The items where the same phrase appears, but before and after something else (i.e. not at the beginning)
    df[i, "synopsis"] <- paste(info2[a[!(a %in% b)] + 1], collapse = ", ") # We want the indices where it just says "Release year: YYYY: " and look at the very next item. 
  }
  # In these sections, the interesting information is consistently in the next item in the list.
  df[i, "title"] <- html_text(html_nodes(page, "h1"))[1]
  df[i, "year"] <- info1[3]
  df[i, "rating"] <- info1[4]
  df[i, "genres"] <- info1[which(info1 == "Genre: " | info1 == "Genres: ") + 1][1]    
  df[i, "director"] <- info1[which(info1 == "Director: " | info1 == "Directors: ") + 1][1]
  df[i, "starring"] <- info1[which(info1 == "Starring: ") + 1][1]
  df[i, "cast"] <- info1[which(info1 == "Cast: ") + 1][1]
  df[i, "creator"] <- info1[which(info1 == "Creator: " | info1 == "Creators: ") + 1][1]
  
  df
}



for (i in c(1:length(sample_ids))) {
  print(i)
  page <- read_html(paste("https://www.netflix.com/title/", sample_ids[i], sep =""))
  if (html_text(html_nodes(page, "h1"))[1] == "Sign In" | html_text(html_nodes(page, "h1"))[1] == "See what’s next." | html_text(html_nodes(page, "h1"))[1] == "Page Not Found") { # Check to see if the page didn't load anything. If it did you can continue on.
    next }
  df <- process_media_page(page, df) # Gosh I really hope R knows to pass pointers through or this is SO inefficient
  if (html_text(html_nodes(page, "h3"))[1] == "MORE DETAILS"){ # IT'S A MOVIE
    df_movies <- process_media_page(page, df_movies)
  } else { # IT'S A TV SHOW
    df_tv <- process_media_page(page, df_tv)
  }
} 


# Remove the rows in which there are no items from each data frame.
df_clean <- df[rowSums(is.na(df))!= (length(categories)), ]
df_movies_clean <- df_movies[rowSums(is.na(df_movies))!= (length(categories)), ]
df_tv_clean <- df_tv[rowSums(is.na(df_tv))!= (length(categories)), ]

for (i in 1:nrow(df_clean)){
  print(i)
  df_actors <- process_cast(df_clean[i,], df_actors)
}


# Write everything to Excel for compatability (yes?)
write.xlsx(x = df_clean, file = "netflix_items.xlsx",
           sheetName = "Sheet1")
write.xlsx(x = df_movies_clean, file = "netflix_films.xlsx",
           sheetName = "Sheet1")
write.xlsx(x = df_tv_clean, file = "netflix_tv.xlsx",
           sheetName = "Sheet1")
write.xlsx(x = df_actors, file = "test.excelfile.xlsx",
           sheetName = "Sheet1")
