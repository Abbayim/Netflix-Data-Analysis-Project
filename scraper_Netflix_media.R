library("stringi", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rvest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# for execution, commented because the list itself is fat.
# every_8_digit_number <- c(10000000:999999999)

# for construction
sample_ids_movies <- c(60037627, 80108238, 60036691)
sample_ids_tv <- c(80025678, 70177083)
sample_ids_bad <- c(100000000, "10000000", "abcdefgh")
sample_ids_expanded <- c(80144588, 80037759, 70184207, 80025384, 80000770, 80107369, 80097003, 70052705, 80091741, 80117470, 80053653, 939827)
sample_ids_people <- c(20033457)
  
sample_ids <- c(sample_ids_movies, sample_ids_tv, sample_ids_expanded)

id_dictionary <- sample_ids # development, will switch to every_8_digit_number when the time is right.

categories <- c("Netflix_id", "title", "year", "rating", "duration", "synopsis", "genres", "director", "starring", "creator", "episodes", "IMDB_id")

m <- matrix(ncol=length(categories), nrow=length(id_dictionary))
colnames(m) <- categories
rownames(m) <- id_dictionary
df <- data.frame(m, stringsAsFactors=FALSE)
df_movies <- data.frame(m)
df_tv <- data.frame(m)


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



process_media_page <- function(page, df) {
  info1 <- html_text(html_nodes(page, "span"))
  info2 <- html_text(html_nodes(page, "div div"))
  info3 <- html_text(html_nodes(page, "div ul"))
  if (html_text(html_nodes(page, "h3"))[1] == "MORE DETAILS"){ # IT'S A MOVIE
    df[i, "duration"] <- info1[6]
    syn <- strsplit(sub(pattern="Starring: ", strsplit(sub("[0-9]+h [0-9]+m", info2[16], replacement=":::"), ":::")[[1]][2], replacement=":::"), ":::")[[1]][1] # An obnoxious way of extracting the synopsis from the string, on the left by the duration and on the right by cast information. 
    df[i, "synopsis"] <- strsplit(sub(pattern="Genres: ", syn, replacement=":::"), ":::")[[1]][1] # Can add more of these removers if necessary. Easier to do one at a time and copy some code, might need to write function to do this later but for now this seems ok.
    df[i, "type"] <- "Movie"
  } else { # IT'S A TV SHOW
    # R won't let me store a list inside a data frame. I refuse to do a list of list, which is the only solution I could find on Stack Overflow, so I'm making a fake list string and will re-list later.
    df[i, "episodes"] <- paste(html_text(html_nodes(page, "h3"))[2:(length(html_text(html_nodes(page, "h3"))) - 2)], collapse = ", ") # first item is "EPISODES", last 2 items are "MORE DETAILS" and "More TV Shows & Movies" which I don't care about storing.
    df[i, "duration"] <- paste(info1[c(grep("[0-9]+m", info1), grep("[0-9]+h [0-9]+m", info1))], collapse = ", ")
    # Snatch the synopses!
    a <- grep("Release Year: [0-9][0-9][0-9][0-9]", info2) # The items where the words "Release year: YYYY", of which the synopses of interest ot us are immediately after, consistently.
    b <- grep(".+Release Year: [0-9][0-9][0-9][0-9].+", info2) # The items where the same phrase appears, but before and after something else (i.e. not at the beginning)
    df[i, "synopsis"] <- paste(info2[a[!(a %in% b)] + 1], collapse = ", ") # We want the indices where it just says "Release year: YYYY: " and look at the very next item. 
    df[i, "type"] <- "TV Show"  
  }
  # In these sections, the interesting information is consistently in the next item in the list.
  df[i, "title"] <- html_text(html_nodes(page, "h1"))[1]
  df[i, "year"] <- info1[3]
  df[i, "rating"] <- info1[4]
  df[i, "genres"] <- info1[which(info1 == "Genre: " | info1 == "Genres: ") + 1][1]    
  df[i, "director"] <- info1[which(info1 == "Director: " | info1 == "Directors: ") + 1][1]
  df[i, "starring"] <- info1[which(info1 == "Starring: ") + 1][1]
  df[i, "creator"] <- info1[which(info1 == "Creator: " | info1 == "Creators: ") + 1][1]
  
  
  # Assumption: If I search <title> <year> the first result will be what I want.
  # Generate search URL on IMDB: The Crown 2016 should look like: http://www.imdb.com/find?ref_=nv_sr_fn&q=the+crown+2016&s=all
  
  # This is imperfect, I just now realize. Make it like this instead: 
  # http://www.imdb.com/search/title?release_date=,2015&title=dope&title_type=movie
  # http://www.imdb.com/search/title?release_date=,2017&title=blue%20bloods&title_type=tv_series
  
  
  title_formatted <- strsplit(df[i, "title"], "(", fixed=TRUE)[[1]][1] # If there's a comment in parentheses after the title (i.e. "Shameless (U.S.)") then we want to get rid of these comments because they're likely not on IMDB.
  title_formatted <- sub(pattern=" ", x=tolower(title_formatted), replacement="%20")
  
  Sys.sleep(sample(10, 1) * 0.1) # Weirdo error, the internet is sensitive, have to pause every once in a while
  page <- tryCatch({
    if (df[i, "type"] == "Movie") {
      read_html(paste("https://www.imdb.com/search/title?release_date=", df[i, "year"], ",", df[i, "year"], "&title=", title_formatted, "&title_type=", "movie", sep = "")) 
    } else {
      read_html(paste("https://www.imdb.com/search/title?release_date=,",df[i, "year"], "&title=", title_formatted, "&title_type=", "tv_series", sep="")) 
    }
  }, error = function(err) {
    return("Failure")
  })
  if (page == "Failure"){
    print("IMDB fail")
    return(df)
  }
  # page <- read_html(paste("https://www.imdb.com/find?ref_=nv_sr_fn&q=", title_formatted, "+", df[i, "year"], "&s=all", sep=""))
  result <- html_nodes(page, ".lister-item-content")
  if (length(result) < 1){
    print("uh oh!")
  }
  IMDB_id <- unlist(strsplit(as.character(result[1]), "/"))[4]
  df[i, "IMDB_id"] <- IMDB_id
  df
}



for (i in c(1:length(sample_ids))) {
  print(i)
  page <- read_html(paste("https://www.netflix.com/title/", sample_ids[i], sep =""))
  if (html_text(html_nodes(page, "h1"))[1] == "Sign In" | html_text(html_nodes(page, "h1"))[1] == "See what’s next." | html_text(html_nodes(page, "h1"))[1] == "Page Not Found") { # Check to see if the page didn't load anything. If it did you can continue on.
    next }
  df <- process_media_page(page, df) # Gosh I really hope R knows to pass pointers through or this is SO inefficient
  df[i, "Netflix_id"] <- sample_ids[i]
  if (html_text(html_nodes(page, "h3"))[1] == "MORE DETAILS"){ # IT'S A MOVIE
    df_movies[nrow(df_movies)+1, ] <- df[i,]
  } else { # IT'S A TV SHOW
    df_tv[nrow(df_tv)+1,] <- df[i,]
  }
} 


# Remove the rows in which there are no items from each data frame.
df_clean <- df[rowSums(is.na(df))!= ncol(df), ]
df_movies_clean <- df_movies[rowSums(is.na(df_movies))!= ncol(df), ]
df_tv_clean <- df_tv[rowSums(is.na(df_tv))!= ncol(df), ]




# Write everything to Excel for compatability (yes?)
write.xlsx(x = df_clean, file = "netflix_items.xlsx",
           sheetName = "Sheet1")
write.xlsx(x = df_movies_clean, file = "netflix_films.xlsx",
           sheetName = "Sheet1")
write.xlsx(x = df_tv_clean, file = "netflix_tv.xlsx",
           sheetName = "Sheet1")

