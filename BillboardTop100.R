# load web access package
library(rvest)
# load other packages needed
library(tidyverse)
library(dplyr)

# load the url for the web page to scrape
url <- "https://www.billboard.com/charts/hot-100/"

webpage <- read_html(url)
html_text(webpage)

ArtistsList <- html_elements(webpage, ".c-label")
ArtistsList <- html_text(ArtistsList)
ArtistsList

# create a dataframe for artists names only 
artistsData <- data.frame(Artist = ArtistsList)

# clean the artists names data from characters
artistsData$Artist <- sub(pattern = "\n\t\n\t", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "\n", replacement = "", x = artistsData$Artist, fixed = TRUE)

artistsData

# now clean artists data from numbers
for (i in 1:2) { 
artistsData$Artist <- sub(pattern = "1", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "2", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "3", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "4", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "5", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "6", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "7", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "8", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "9", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "0", replacement = "", x = artistsData$Artist, fixed = TRUE)
}


# now clean artists data from "NEW" and "-" entries
artistsData$Artist <- sub(pattern = "NEW", replacement = "", x = artistsData$Artist, fixed = TRUE)
artistsData$Artist <- sub(pattern = "-", replacement = "", x = artistsData$Artist, fixed = TRUE)


# filter the data to remove all the "null" entries
artistsData <- filter(artistsData, Artist != "")

# remove last few entries that are not top 100 songs' artists names
artistsData <- filter(artistsData, row_number() <= 100)
#
#
# DONE SCRAPING ARTISTS NAMES IN THE ORIGINAL ORDER
# NOW MOVING TO SCRAPE SONG TITLES
#
#
songTitlesList <- html_elements(webpage, ".c-title")
songTitlesList <- html_text(songTitlesList)
songTitlesList

# make a data frame for only song titles
songTitlesData <- data.frame(Title = songTitlesList)


# clean entries from weird characters
songTitlesData$Title <- gsub(pattern = "\n", replacement = "", x = songTitlesData$Title, fixed = TRUE)
songTitlesData$Title <- gsub(pattern = "\t", replacement = "", x = songTitlesData$Title, fixed = TRUE)

songTitlesData

# Looking at the songTitlesData, it seems like starting from 7th entry, there is 3 entries between every actual song title 
# so I need a way to filter the data in a way that I get only actual song names
# I am using temp data frame so I do not mess it up while trying to figure out
temp <- songTitlesData 
temp <- filter(temp, row_number() >= 4)
temp <- filter(temp, row_number()%%4 == 0)

# now remove the last few entries that are not actual song titles
temp <- filter(temp, row_number() <= 100)
songTitlesData <- temp
#
#
# DONE SCRAPING SONG TITLES IN ORIGINAL ORDER
# NOW MOVING TO SCRAPING LAST WEEKS RATING NUMBER
#
#
lastWeeksRatingsList <- html_elements(webpage, ".o-chart-results-list__item")
lastWeeksRatingsList <- html_text(lastWeeksRatingsList)
lastWeeksRatingsList

lastWeeksRatingData<- data.frame(Last_Week_Rating = lastWeeksRatingsList)

temp <-lastWeeksRatingData
temp$Last_Week_Rating <- gsub(pattern = "\n", replacement = "", x = temp$Last_Week_Rating, fixed = TRUE)
temp$Last_Week_Rating <- gsub(pattern = "\t", replacement = "", x = temp$Last_Week_Rating, fixed = TRUE)
temp$Last_Week_Rating <- gsub(pattern = "NEW", replacement = "", x = temp$Last_Week_Rating, fixed = TRUE)
temp

temp <- filter(temp, Last_Week_Rating != "")
temp

#Since all of the ratings are scraped from this, let's save this to reuse for other attributes
attributesData <- temp

temp <- filter(temp, (row_number()+5)%%8 == 0)
lastWeeksRatingData <- temp
#
#
# DONE SCRAPING LAST WEEKS RATING NUMBErS IN ORDER
# MOVING TO PEAK POSITION
#
#
temp <- attributesData
temp <- filter(temp, (row_number()+5)%%8 == 1)
peakPosData <- temp
#
#
# DONE SCRAPING PEAK POS NUMBERS IN ORDER
# MOVING TO WEEKS IN CHART 
#
#
temp <- attributesData
temp <- filter(temp, (row_number()+5)%%8 == 2)
weeksOnChartData <- temp
weeksOnChartData
#
#
# DONE SCRAPING EVERYTHING, NOW LET'S PUT IT TOGETHER
#
#
BillboardTop100 <- data.frame(This_Week = 1:100, Title = songTitlesData$Title, Artist = artistsData$Artist, Last_Week = lastWeeksRatingData$Last_Week_Rating, Peak_Position = peakPosData$Last_Week_Rating, Weeks_On_Chart = weeksOnChartData$Last_Week_Rating)

# Save this dataframe into the csv file
# write.csv(BillboardTop100,"/Users/davitishakarashvili/Desktop/Fall2022/DATA300.3/WebScraping/BillboardTop100.csv", row.names = FALSE)


