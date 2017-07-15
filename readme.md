This code pulls metadata from every movie/tv show on Netflix, gets the cast/crew list and synopses from IMDB, uses US census data to estimate the racial composition of each media item based on its cast, uses IBM Watson to analyze its tone, and frequency analysis (how often certain words come up) to determine relevance to poverty.

If you wish to run this code, run it in this order:
===================================================

**scraper_Netflix_media.R** Goes through every ID, or sample IDs, and saves a dataframe in Excel with fields:
* title
* year
* rating
* duration
* synopsis (to be expanded later)
* genres
* director
* starring
* creator
* episodes
* IMDB id (scraped from IMDB)

**scraper_synopsis.R** expands synopsis from IMDB

**tone_analysis.R** using coginizer from Columbus Collaboratory: https://github.com/ColumbusCollaboratory/cognizer#tone
(note: You need your own keys! See below.)

**scraper_cast_crew.R** For each media item, finds cast and crew names from IMDB

**racial_composition.R** using wru package from the US Census, approximates racial distribution by last name for each name.

**racial_analysis.R** takes averages for everyone/cast/crew for each race named on the Census, 

**poverty_analysis.R** frequency analysis for commonly associated words, stored at poverty_word_list.csv


Keys
-----
I have 2 keys for this project currently, not on Github, stored in a file called "keys.csv"
1. US Census API key
2. IBM Tone Analysis username:password