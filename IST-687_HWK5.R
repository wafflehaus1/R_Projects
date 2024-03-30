#HWK 5 Code

#Step 1: Load the data
  #install libraries if not installed yet
install.packages("RCurl")
install.packages("RJSONIO")
install.packages("jsonlite")
install.packages("sqldf")

  #Call the libraries
library("RCurl")
library("RJSONIO")
library("jsonlite")
library("sqldf")
  #Read in the following JSON dataset
'https://opendata.maryland.gov/resource/pdvh-tf2u.json'

link <- 'https://opendata.maryland.gov/resource/pdvh-tf2u.json'
JSONData <- fromJSON(link)

MyData <- data.frame(JSONData,stringsAsFactors = FALSE)


str(JSONData)
View(MyData)

#Step 2: Clean the data
  #Remove the first 8 columns, and then, to make it easier to work with, name the rest of the columns as follows:
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")

colnames(MyData) <- namesOfColumns

MyData$DIST_FROM_INTERSECT <- as.numeric(MyData$DIST_FROM_INTERSECT)
MyData$VEHICLE_COUNT <- as.numeric(MyData$VEHICLE_COUNT)
MyData$DAY_OF_WEEK <- trimws(MyData$DAY_OF_WEEK, which = c("right"))

str(MyData)

CleanMyData <- na.omit(MyData)
rownames(CleanMyData) <- NULL
View(CleanMyData)
attach(CleanMyData)

#Step 3: Understand the data using SQL(via SQLDF)
  #Answer the following questions:
    #How many accidents happen on SUNDAY
FirstQuery <- sqldf("SELECT COUNT(CASE_NUMBER) AS SundayAccidents FROM CleanMyData WHERE DAY_OF_WEEK = 'SUNDAY'")
FirstQuery    
    #How many accidents had injuries (might need to remove NAs from the data)
SecondQuery <- sqldf("SELECT COUNT(CASE_NUMBER) AS AccidentsWithInjuries FROM CleanMyData WHERE INJURY = 'YES'")
SecondQuery
    #List the injuries by day
ThirdQuery <- sqldf("SELECT COUNT(CASE_NUMBER) AS NumberOfInjuries,DAY_OF_WEEK AS Day FROM CleanMyData WHERE INJURY = 'YES' GROUP BY Day ORDER BY NumberOfInjuries DESC")
ThirdQuery
#Step 4: Understand the data using tapply
  #Answer the following questions (same as before) - compare results:
    #How many accidents happen on SUNDAY
tapply(CASE_NUMBER, DAY_OF_WEEK=='SUNDAY', length)

    #How many accidents had injuries (might need to remove NAs from the data)
tapply(CASE_NUMBER, INJURY=='YES', length)

    #List the injuries by day
tapply(CASE_NUMBER, list(DAY_OF_WEEK, INJURY=='YES'), length)

       