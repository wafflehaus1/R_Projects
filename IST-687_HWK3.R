#R Code - unexecuted

# ---------- HW3: Intro -----------

#Step 1: Create a function (named readStates) to read a CSV file into R.
  #1) Note that you are to read a URL, not a file local to your computer.
  #2) The file is a dataset on state populations (?ithin the United States)
  #The URL is: http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv

statesurl <- read.csv(url("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01?csv"))
str(statesurl)
head(statesurl, 1)
View(statesurl)

StateFrameFunc <- function(){
  statesurl <- read.csv(url("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"))
  statesframe <- statesurl
  statesframe <- statesframe[-1:-8,]
  rownames(statesframe) <- NULL
  statesframe <- statesframe[-52:-58,]
  statesframe <- statesframe[,-6:-10]
  rownames(statesframe) <- NULL
  newcolumns <- c("stateName","base2010","base2011","Jul2010","Jul2011")
  colnames(statesframe) <- newcolumns
  statesframe$stateName <- gsub("\\.","",statesframe$stateName)
  statesframe$base2010 <- as.numeric(gsub(",","",statesframe$base2010)) 
  statesframe$base2011 <- as.numeric(gsub(",","",statesframe$base2011)) 
  statesframe$Jul2010 <- as?numeric(gsub(",","",statesframe$Jul2010)) 
  statesframe$Jul2011 <- as.numeric(gsub(",","",statesframe$Jul2011))
  return(statesframe)
}

str(StateFrameFunc())
head(StateFrameFunc())

#Step 2: Clean the dataframe
  #3) Note the issues that need to be fixed?(removing columns, removing rows, changing column names).
      #See function above

  #4) Within your function, make sure there are 51 rows (one per state + the District of Columbia). 
    #Make sure there are only 5 columns with the columns having the fo?lowing name (stateName, base 2010, base 2011, Jul2010, Jul2011).
      #See function above

  #5) Make sure the last four columns are numbers (i.e., not strings)
      #See function above

#Step 3: Store and Explore the dataset
  #6) Store the dataset into?a dataframe, called dfStates
dfStates <- StateFrameFunc()
  
  #7) Test your dataframe by calculating the mean for the Jul2011 data, by doing: mean(dfStates$Jul2011) = 6,109,645
mean(dfStates$Jul2011)

#Step 4: Find the state with the Highest Population
  ?8) Based on the Jul2011 data, what is the population of the state with the highest population? What is the name of that state?
MaxPopulation <- max(dfStates$Jul2011)
MaxPopulation

MaxStateName <- dfStates[which.max(dfStates$Jul2011),1]
MaxStateName


  #9? Sort the data, in increasing order, based on the Jul2011 data.
SortedStates <- dfStates[order(+dfStates$Jul2011),]
SortedStates

#Step 5: Explore the distribution of the states
  #10) Write a function that takes two parameters. The first is a vector and t?e second is a number.
a <- c(4802740, 722718, 6482505, 2937979, 37691912, 5116796, 3580709, 907135, 617996, 19057542)

b = 1000000

functionPercent <- function(){
  c <- length(a[a<b])
  e <- c/length(a)
  return(e)
}
functionPercent()

fpercent2 <- functi?n(){
  c <- length(a[a<b])/length(a)
  return(c)
}
fpercent2()

  
  #11) The function will return the percentage of the elements within the vector that is less than the same 
      #(i.e., the cumulative distribution below the value provided)
functionPerc?nt()

  #12) For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number passed into the function, the function would return 0.2
      #(since 20% of the numbers were below 2).

  #13) Test the function with the vector 'dfStates$Jul2011N?m', and the mean of 'dfStates$Jul2011Num'
dfStates$Jul2011Num <- dfStates$Jul2011

f <- dfStates$Jul2011Num

functionPercentTest <- function(){
  c <- length(f[f<b])
  e <- c/length(f)
  return(e)
}
functionPercentTest()

mean(f)

#Write multiple versions ?f this function - which do you think is best?
  # I prefer to catalog each component of a function or calculation separately so it makes it easier to triage where the
  #errors are within your work, as well as, making it easier to interchange values/tweak ?he formulas as needed.



