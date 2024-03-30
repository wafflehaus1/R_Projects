#HWK 4 Code

install.packages("moments")
library(moments)

#Step 1: Write a summarizing function to understand the distribution of a vector
#1) The function, call it 'printVecInfo' should take a vector as input
testVect <- c(1,2,3,4,5,6,7,8,9,50,75,100)

printVecInfo <- function(vec){
  cat("Mean:",mean(vec),"\n")
  cat("Median:",median(vec),"\n")
  cat("Min:",min(vec),"& ")
  cat("Max:",max(vec),"\n")
  cat("Standard deviation:",sd(vec),"\n")
  cat("Quantiles (0.05 - 0.95):", quantile(vec,0.05), "-- ")
  cat(quantile(vec,0.95), "\n")
  cat("Skewness:", skewness(vec), "\n")
}

#2) The function should print the following information:
#Mean
#Median
#Min & Max
#Standard deviation
#Quantiles (at 0.05 and 0.95)
#Skewness
printVecInfo(testVect)

#3) Test the function with a vector that has (1,2,3,4,5,6,7,8,9,10,50). You should see something such as:
#[1]"mean:9.54545454545454" [1]"median:6" [1]"min:1 max:50" [1]"sd:13.7212509368762"
#[1]"quantile(0.05-0.95):1.5--30"[1]"skewness:2.62039633563579"
testFunction <- c(1,2,3,4,5,6,7,8,9,10,50)

printVecInfo(testFunction)

#Step 2: Creating Samples in a Jar
#4) Create a variable 'jar' that has 50 red and 50 blue marbles
R <- "Red"
B <- "Blue"

RepR <- replicate(50,R)
RepB <- replicate(50,B)

jar <- c(RepR, RepB)

#5) Confirm there are 50 reds by summing the samples that are red
length(which(jar == "Red"))
length(which(jar == "Blue"))
length(jar)

#6) Sample 10 'marbles' (really strings) from the jar. How many are red? What was the percentage of red marbles?
SampleJar <- sample(jar,10,replace = TRUE)
SampleJar

RedCount <- length(SampleJar[SampleJar=="Red"])
RedCount

#RedCount/length(SampleJar)
RedPerc <- RedCount/length(SampleJar) * 100 
RedPerc

cat(RedCount, "Red equal to" ,RedPerc, "%")

sam <- function(jar, num){
  samp <- sample(jar,num,replace=TRUE)
  num <- length(samp[samp=="Red"])/length(samp) 
  return(num)
}

sam(jar, 1000)

#7) Do the sampling 20 times, using the 'replicate' command. This should generate a list of 20 numbers. Each number is the
#mean of how many reds there were in 10 samples. Use your printVecInfo to see information of the samples. Also generate a 
#histogram of the samples.
replicate(20,sam(jar,10))
sampleMeans7 <- replicate(20,mean(replicate(20,sam(jar,10))))
hist(sampleMeans7)
printVecInfo(sampleMeans7)

#8) Repeat #7, bu this time, sample the jar 100 times.
replicate(20,sam(jar,100))
sampleMeans8 <- replicate(20,mean(replicate(20,sam(jar, 100))))
hist(sampleMeans8)
printVecInfo(sampleMeans8)

#9) Repeat # 8, but this time, replicate the sampling 100 times.
replicate(100,sam(jar,100))
sampleMeans9 <- replicate(100,mean(replicate(100,sam(jar, 100))))
hist(sampleMeans9)
printVecInfo(sampleMeans9)

#Step 3: Explore the airquaity dataset
#10) Store the 'airquality' dataset into a temporary variable
myData <- airquality

#11) Clean the dataset (i.e., remove the NAs)
cleanData <- na.omit(myData)
cleanData

#12)Explore Ozone, Wind, and Temp by doing a 'printVecInfo' on each as well as generating a histogram for each
printVecInfo(cleanData$Ozone)
hist(cleanData$Ozone)

printVecInfo(cleanData$Wind)
hist(cleanData$Wind)

printVecInfo(cleanData$Temp)
hist(cleanData$Wind)
