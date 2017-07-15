

** My code does the following: **
  1. **scraper_script.R** Gets the movie/tv-show info from Netflix. For each:
    1. On a separate table, gets the cast/crew info from IMDB. For each:
      1. Organizes by Cast/Crew
      2. Assigns rank based on where the name is on either the cast or crew list.
      3. **racial_composition.R** Looks up last name on tables other people made of names -> racial composition calculated from the 2000 and 2010 US Census by totaling the number of people who identified as whatever race with a particular last name, divided by the total number of people with that last name, giving %white, %black, %hispanic, %asian, %other.
        If there are distributions from both censuses, I take the average, otherwise I pick the one that has information.
      4. **racial_analysis.R** Calculates aggregate racial distribution, total movie and cast/crew
    2. Gets the synopsis from IMDB.
    3. **tone_analysis.R** Runs IBM Watson's Tone Analysis on each synopsis, and normalizes so each item has %anger, %disgust, %fear, %joy, %sadness.
    4. **poverty_analysis.R** Analyzes synopsis with respect to relevance to poverty by scoring with frequency of words I chose by skimming some books I have from classes (for example, "poor", "dirt", "bootstraps", "afford", "welfare", "walmart")  divided by total words
    5. Assigns each item a rank, so to distinguish between stories more/less related to poverty.