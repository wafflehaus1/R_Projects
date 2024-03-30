#HWK 10 ####

install.packages("tidytext")
install.packages("readtext")
install.packages("OptimalCutpoints")
install.packages("tm")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("slam")
library(tidytext)
library(readtext)
library(OptimalCutpoints)
library(tm)
library(wordcloud)
library(ggplot2)
library(slam)

#Load the Speech ####
setwd("C:\\Users\\mstoc\\OneDrive - Syracuse University\\Summer 2020\\IST 687 - Introduction to Data Science\\Week 10")

mlk <- readLines(file("MLK.txt"))
mlk <- mlk[which(mlk != "")] #remove all blank lines in the text

words.vec <- VectorSource(mlk)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

tdm <- TermDocumentMatrix(words.corpus)
tdm

#Create a list of counts for each word convert tdm into a matrix called "m"
m <- as.matrix(tdm)
m[1:10,]

wordCounts <- rowSums(m)
wordCounts[1:10]

totalWords <- sum(wordCounts)
totalWords
words <- names(wordCounts)
head(words)

wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
View(cloudFrame)
wordcloud(cloudFrame$word,cloudFrame$freq)

#Step 1: Read in the AFINN word list. ####

#Read Afffinity file
afinn <- read.delim("HW10AFINN.txt",sep="\t",header = FALSE)
str(afinn)
colnames(afinn) <- c("Word", "Score")
#join the df match with AFINN by "word" col in match and "Word" col in AFINN
mergedTable <- merge(cloudFrame,afinn,by.x="word",by.y="Word")
str(mergedTable)
View(afinn)


#Step 2: Compute the overall score for the MLK speech using the AFINN word list. ####
overallScore <- sum(mergedTable$freq*mergedTable$Score)
overallScore 

#Step 3: Create four functions that split the data into quarters and returns score. ####

#1st 25%:
mlk1 <- function(){
  a <- mergedTable[1:15,]
  b <- sum(a$freq*a$Score)
  return(b)
}
mlk1() #1st 25% has score of 5

mlk2 <- function(){
  a <- mergedTable[16:30,]
  b <- sum(a$freq*a$Score)
  return(b)
}
mlk2()

mlk3 <- function(){
  a <- mergedTable[31:45,]
  b <- sum(a$freq*a$Score)
  return(b)
}
mlk3()

mlk4 <- function(){
  a <- mergedTable[46:61,]
  b <- sum(a$freq*a$Score)
  return(b)
}
mlk4()

#Check total score by summing
sum(mlk1()+mlk2()+mlk3()+mlk4()) #checks out

mlkQuarter<- c("1st Quarter","2nd Quarter", "3rd Quarter","4th Quarter")
mlkQuarterScore <- c(mlk1(),mlk2(),mlk3(),mlk4())
mlkQuarterTotal <- data.frame(mlkQuarter,mlkQuarterScore)

#Step 4: Finally, plot the results via a bar chart. ####

#Plot the score of each quarter on a bar graph:
ggplot(mlkQuarterTotal, aes(x=mlkQuarter, y=mlkQuarterScore)) + geom_bar(stat="identity")+ labs(x="Quarter",y="Score",title="Sentiment Score for Each Quarter of MLK Speech")+coord_cartesian(ylim=c(0, 100)) + scale_y_continuous(breaks=seq(0,100,10)) + theme(plot.title = element_text(hjust = 0.5))
