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

categories2 <- c("media", "person", "position", "role", "episodes involved", "year", "rank")
m2 <- matrix(ncol=length(categories2), nrow=length(id_dictionary)) # only 2 columns for 2 things I care about: Actor, Things they're in, and we still dont' know how long it will be, so for now I make it very long and hope it doesn't eat too much space.
colnames(m2) <- categories2
rownames(m2) <- id_dictionary
df_actors <- data.frame(m2)


process_item <- function(i, details, df){
  for (j in details) {
    df[i, j] <- details[j]
  }
  df
}

process_cast <- function(details, df) {
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
  useful_text <- strsplit(html_text(html_nodes(page, ".listo")), "\n")[[1]]
  useful_text <- useful_text[which(
                               useful_text != "              ..." 
                             & useful_text != "          ..."
                             & useful_text != "         / ...  "
                             & useful_text != "                ?"
                             & useful_text != "              " 
                             & useful_text != "             "
                             & useful_text != "            "
                             & useful_text != "           "
                             & useful_text != "          "
                             & useful_text != "         "
                             & useful_text != "        "
                             & useful_text != "       "
                             & useful_text != "      "
                             & useful_text != "     "
                             & useful_text != "    "
                             & useful_text != "   "
                             & useful_text != "  "
                             & useful_text != " "
                             & useful_text != "")]
  useful_text <- gsub("[[:space:]]{3}", "", useful_text)
  useful_text <- trimws(useful_text, which="both")
  # Is there a less stupid way of trimming useful_text?
  
  # In sum, this is useful_text's pattern:
  # Full Cast & Crew
  # Series {Thing'd} by
  # {Person}
  # ({x} Episodes, {Year})
  # Series Writing Credits
  # {Person}
  # ({x} Episodes, {Year})
  # Series 
  # Cast
  # {Actor}
  # {Role}
  # (Episodes, Year)
  # Create a character page for:
  # nonsense
  # nonsense
  # Series {Job'd} by || Series {Job}
  # {Person}
  # **optional* {Subjob}* ({x} Episodes, {Year})            ## note *'s are for notation, and do not actually appear.
  
  # First, there's a section of high-totem behind-screen. Then there's the actors on-screen, progressively lower-totem. Then low-totem behind-screen.
  # How do I process this?
  # We can rank by IMDB popularity. This seems like a pretty good approximation for something real. It is consistent and not alphabetical order, so there's some semblence of relevance to it.
  # 
  
  ITEM_COUNTER <- length(df$title)
  
  form1 <- grep("Series ([A-Z][a-z]+|[A-Z][a-z]+ [A-Z][a-z]+)", useful_text)
  form2 <- grep("Series ([A-Z][a-z]+|[A-Z][a-z]+ [A-Z][a-z]+) (B|b)y", useful_text) # Series {Job} by
  form3 <- form1[!(form1 %in% form2)] # Series {Job}
  
  # c("media", "person", "position", "role", "number of appearances", "year", "rank")
  
  i <- grep("Cast", useful_text)[2]
  end <- grep("Create a character page for", useful_text)
  if (length(end) > 1 | end < i) {
    print("something's not right")
  } else {
    for (j in c(i+2:end)) {
      df[ITEM_COUNTER, "media"] <- details["title"]
      df[ITEM_COUNTER, "person"] <- useful_text[i]
      df[ITEM_COUNTER, "position"] <- "Cast"
      j <- j + 1
      df[ITEM_COUNTER, "role"] <- useful_text[j]
      j <- j + 1
      term <- unlist(strsplit(strsplit(strsplit(useful_text[j], "(", fixed=TRUE)[[1]][2], ", ", fixed=TRUE)[[1]], ")", fixed=TRUE))
      df[ITEM_COUNTER, "episodes involved"] <- term[1]
      df[ITEM_COUNTER, "year"] <- term[2]
      ITEM_COUNTER <- ITEM_COUNTER + 1
      print(ITEM_COUNTER)
    }
  }
  
  for (j in c(1:length(form1))){
    k <- form1[j]
    if (j+1 > length(form1)){
      end <- length(useful_text) # Maybe sloppy.
    } else{
      end <- form1[j+1]
    }
    term <- strsplit(useful_text[k], " ", fixed=TRUE)[[1]]
    if (k %in% form2) {
      term <- paste(term[2:(length(term)-1)], collapse= " ") # from the 2nd word to the 2nd to last
    }
    else {
      term <- term[2:length(term)]
    }
    l <- k+1
    while (l < end) {
      print(l)
      print(end)
      if (length(useful_text[l]) < 2){
        l <- l+1
        next
        print("weird case2")
      }
      df[ITEM_COUNTER, "position"] <- term
      df[ITEM_COUNTER, "media"] <- details["title"]
      df[ITEM_COUNTER, "person"] <- useful_text[l]  
      l <- l+1
      term2 <- unlist(strsplit(unlist(strsplit(unlist(strsplit(unlist(strsplit(useful_text[l], "(", fixed=TRUE)), ", ", fixed=TRUE)), ")", fixed=TRUE)), " "))
      print(term2)
      print(k)
      print(l)   #j, k, l)
      if (length(term2) < 2){
        l <- l+1
        term2 <- unlist(strsplit(unlist(strsplit(unlist(strsplit(unlist(strsplit(useful_text[l], "(", fixed=TRUE)), ", ", fixed=TRUE)), ")", fixed=TRUE)), " "))
        print(term2)
        print(k)
        print(l)   #j, k, l)
        print("weird case")
      }
      if (grep("[0-9]+", term2[1])){ #they dont' have a more particular job.
        df[ITEM_COUNTER, "role" ] <- term
        df[ITEM_COUNTER, "episodes involved"] <- paste(term2[1:2], collapse=" ") # keep the word episodes
        df[ITEM_COUNTER, "year"] <- term2[3]
      } else {
        word_episode_loc <- which(term=="episode" | term == "episodes")
        df[ITEM_COUNTER, "role"] <- paste(term2[1:(word_episode_loc-2)], collapse=" ")
        df[ITEM_COUNTER, "episodes involved"] <- paste(term2[word_episode_loc:length(term2)], collapse=" ")
        if (grep("[0-9]{4}", term2[length(term2)])) {
          df[ITEM_COUNTER, "year"] <- term2[length(term2)]  
        } else {
          df[ITEM_COUNTER, "year"] <- NA
        }
      }
      ITEM_COUNTER <- ITEM_COUNTER + 1
      print(ITEM_COUNTER)
      l <- l + 1
    }
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
    df_actors <- process_cast(df_tv[i,], df)
  }
} 


# Remove the rows in which there are no items from each data frame.
df_clean <- df[rowSums(is.na(df))!= (length(categories)), ]
df_movies_clean <- df_movies[rowSums(is.na(df_movies))!= (length(categories)), ]
df_tv_clean <- df_tv[rowSums(is.na(df_tv))!= (length(categories)), ]

# Write everything to Excel for compatability (yes?)
write.xlsx(x = df_clean, file = "test.excelfile.xlsx",
           sheetName = "Sheet1")
write.xlsx(x = df_movies_clean, file = "test.excelfile.xlsx",
           sheetName = "Sheet2")
write.xlsx(x = df_tv_clean, file = "test.excelfile.xlsx",
           sheetName = "Sheet3")
