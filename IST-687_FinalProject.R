# CREATE BASE DATASET ####

install.packages("ggplot2")
install.packages("ggQC")
install.packages("plyr")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("readxl")
install.packages("kernlab")
install.packages("caret")
install.packages("e1071")
install.packages("party")
install.packages("randomForest")
install.packages("corrplot")
install.packages("ggthemes")
install.packages("gganimate")
install.packages("gifski")
install.packages("reshape2")
install.packages("devtools")
install.packages("dplyr")
install.packages("gdata")
install.packages("ggmap")
install.packages("mapdata")
install.packages("maps")
install.packages("openintro")
install.packages("sqldf")
install.packages("stringr")
install.packages("zipcode")
library(plyr)
library(tidyverse)
library(ggQC)
library(gridExtra)
library(readxl)
library(kernlab)
library(caret)
library(e1071)
library(party)
library(randomForest)
library(corrplot)
library(ggthemes)
library(gganimate)
library(gifski)
library(reshape2)
library(devtools)
library(ggmap)
library(mapdata)
library(maps)
library(openintro)
library(sqldf)
library(zipcode)

#Excel file should be in working directory, named as Data_Raw.xlsx
#assumes only one sheet in Excel and there are no formulas, so copy-paste-as-values
#ignore Warning message for expecting logical since it's not a column we'll be using
Base <- read_excel("Data_Raw.xlsx",range = "A5:BA3342")

#Keep only columns of interest, check structure
Base <- Base[,c(2,4,5,6,7,8,9,25,26,27,28,29,30,31,32,33,34,35,36,48,49,51,52,53)]
str(Base)

#Change column names
colnames(Base) <- c("District","School","Region","ZipCode","Income","Students","Size","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","Charter","TitleI","Type","Percent_Minority","Percent_Econ_Disadv")

#Select for only elementary schools, and then remove that column
Base <- Base %>%
  filter(Type =="01") %>%
  select(-contains("Type"))

#Use only complete cases
Base <- Base %>%
  filter(complete.cases(.))

# CREATE PERFORMANCE SET FOR EVALUATING SCHOOLS ####
#function to convert letter grades into numbers (works specifically with Base, note year_index as column numbers)
recode_col <- function(df){
  copydf <- df
  year_index <- 8:19
  for (i in year_index){
    copydf[i] <- recode(pull(df,i),"A"=4,"B"=3,"C"=2,"D"=1,"F"=0)
  }
  return (copydf)
}
Performance <- recode_col(Base)

#Average Moving Range function (not generic, works just for our dataset)
AverageMovingRange <- function(df){
  difference <- function(vector){
    vector <- as.numeric(vector)
    index <- 1:length(vector)
    differences <- c()
    for (i in index){
      if (i==length(vector)){
        break
      }
      else {
        differences[i] <- abs(vector[i]-vector[i+1])
      }
    }
    return (differences)
  }
  copydf <- df
  row_index <- 1:nrow(df)
  movingRangeVector <- c()
  for (i in row_index){
    currentrow <- df[i,][8:19]
    movingRangeVector[i] <- mean(difference(currentrow))
  }
  copydf$AverageMovingRange <- movingRangeVector
  return (copydf)
}
Performance <- AverageMovingRange(Performance)
str(Performance)

#Average of Grades function (again, function specific to our dataset)
AverageofGrades <- function(df){
  copydf <- df
  row_index <- 1:nrow(df)
  AverageGradesVector <- c()
  for (i in row_index){
    AverageGradesVector[i] <- mean(as.numeric(copydf[i,][8:19]))
  }
  copydf$AverageGrade <- AverageGradesVector
  return(copydf)
}
Performance <- AverageofGrades(Performance)

#Make columns for Upper & Lower Natural Process Limit
Performance <- Performance %>%
  mutate(UNPL=(AverageGrade+(2.66*AverageMovingRange))) %>%
  mutate(LNPL=(AverageGrade-(2.66*AverageMovingRange)))

#Function that evaluates school's performance
Evaluation <- function(df){
  copydf <- df
  row_index <- 1:nrow(df)
  EvaluationVector <- c()
  for (i in row_index){
    LNPLbreaks <- 0
    UNPLbreaks <- 0
    year_index <- 8:12
    for (j in year_index){
      if (df[i,][j] > df[i,]$UNPL) {
        UNPLbreaks <- UNPLbreaks+1
      }
      else if (df[i,][j] < df[i,]$LNPL) {
        LNPLbreaks <- LNPLbreaks+1
      }
    }
    EvaluationScore <- case_when(
      (LNPLbreaks == 0 & UNPLbreaks == 0) ~ "Stable",
      (
        (LNPLbreaks == 2 & UNPLbreaks == 0) |
          (LNPLbreaks == 3 & UNPLbreaks == 0) |
          (LNPLbreaks == 4 & UNPLbreaks == 0) |
          (LNPLbreaks == 5 & UNPLbreaks == 0)
      ) ~ "Declining",
      (
        (LNPLbreaks == 0 & UNPLbreaks == 2) |
          (LNPLbreaks == 0 & UNPLbreaks == 3) |
          (LNPLbreaks == 0 & UNPLbreaks == 4) |
          (LNPLbreaks == 0 & UNPLbreaks == 5)
      ) ~ "Improving",
      (
        (LNPLbreaks == 1 & UNPLbreaks == 0) |
          (LNPLbreaks == 0 & UNPLbreaks == 1)
      ) ~ "Inconclusive",
      TRUE ~ "Fluctuating"
    )
    EvaluationVector[i] <- EvaluationScore
  }
  copydf$EvaluationType <- EvaluationVector
  return(copydf)
}
Performance <- Evaluation(Performance)

#View how many schools fall into what category
EvaluationTab <- Performance %>%
  group_by(EvaluationType) %>%
  summarise(Number_of_Schools = n()) %>%
  arrange(desc(Number_of_Schools))

#Create new column called "Label" to be used for prediction along with 2019 Grades
Performance <- Performance %>%
  mutate(Label=case_when(
    EvaluationType=="Declining" ~ "Declining", 
    AverageGrade==4 ~ "Excellent", 
    TRUE ~ "Other"))

#XMR Chart####

Improving <- Performance %>%
  filter(EvaluationType=="Improving") %>%
  slice(3) %>%
  select(starts_with("20")) %>%
  gather(key="Year",value="Grade") %>%
  arrange(Year)
(imp_xmr_plot <-
    ggplot(Improving, aes(x = Year, y = Grade,group=1)) +
    geom_point() + geom_line() + ggtitle("Improving Schools") +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_QC(method = "XmR", auto.label = T, label.digits = 2, show.1n2.sigma = T))

Fluctuating <- Performance %>%
  filter(EvaluationType=="Fluctuating") %>%
  slice(2) %>%
  select(starts_with("20")) %>%
  gather(key="Year",value="Grade") %>%
  arrange(Year)
(fluc_xmr_plot <- ggplot(Fluctuating, aes(x = Year, y = Grade,group=1)) +
    geom_point() + geom_line() + ggtitle("Fluctuating Schools") +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_QC(method = "XmR", auto.label = T, label.digits = 2, show.1n2.sigma = T))

Stable <- Performance %>%
  filter(EvaluationType=="Stable") %>%
  slice(4) %>%
  select(starts_with("20")) %>%
  gather(key="Year",value="Grade") %>%
  arrange(Year)
(stab_xmr_plot <- ggplot(Stable, aes(x = Year, y = Grade,group=1)) +
    geom_point() + geom_line() + ggtitle("Stable Schools") +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_QC(method = "XmR", auto.label = T, label.digits = 2, show.1n2.sigma = T))

Declining <- Performance %>%
  filter(EvaluationType=="Declining") %>%
  slice(2) %>%
  select(starts_with("20")) %>%
  gather(key="Year",value="Grade") %>%
  arrange(Year)
(decl_xmr_plot <- ggplot(Declining, aes(x = Year, y = Grade,group=1)) +
    geom_point() + geom_line() + ggtitle("Declining Schools") +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_QC(method = "XmR", auto.label = T, label.digits = 2, show.1n2.sigma = T))

grid.arrange(imp_xmr_plot, fluc_xmr_plot, stab_xmr_plot, decl_xmr_plot, nrow = 2)

ddply(Improving, .variables = "Grade",
      .fun = function(df)
      {QC_Lines(data = Improving$Grade, method = "XmR")})

ddply(Fluctuating, .variables = "Grade",
      .fun = function(df)
      {QC_Lines(data = Fluctuating$Grade, method = "XmR")})

ddply(Stable, .variables = "Grade",
      .fun = function(df)
      {QC_Lines(data = Stable$Grade, method = "XmR")})

ddply(Declining, .variables = "Grade",
      .fun = function(df)
      {QC_Lines(data = Declining$Grade, method = "XmR")})

#LINEAR MODELS FOR CLUSTERS ####
#Isolate 3 clusters into different datasets
ExcellentSchools <- Performance %>%
  filter(AverageGrade==4)
DecliningSchools <- Performance %>%
  filter(EvaluationType=="Declining")
AllOtherSchools <- Performance %>%
  filter(!(AverageGrade==4 | EvaluationType=="Declining"))
#Clean datasets to retain only columns to be used for linear models
CleanforLM <- function(df){
  removethiscolumns <- c(1:2,4,7,9:19,24:29)
  df <- df[,-removethiscolumns]
  return(df)
}
ExcellentSchoolsforLM <- CleanforLM(ExcellentSchools)
DecliningSchoolsforLM <- CleanforLM(DecliningSchools)
AllOtherSchoolsforLM <- CleanforLM(AllOtherSchools)
#create linear models
names(ExcellentSchoolsforLM) <- make.names(names(ExcellentSchoolsforLM))
ExcellentSchools_LinearModel <- lm(X2019 ~ Charter + Income + Percent_Econ_Disadv + 
                                     Percent_Minority + Region + Students + TitleI, data=ExcellentSchoolsforLM)
summary(ExcellentSchools_LinearModel)

names(DecliningSchoolsforLM) <- make.names(names(DecliningSchoolsforLM))
DecliningSchools_LinearModel <- lm(X2019 ~ Charter + Income + Percent_Econ_Disadv + 
                                     Percent_Minority + Region + Students + TitleI, data=DecliningSchoolsforLM)
summary(DecliningSchools_LinearModel)
names(AllOtherSchoolsforLM) <- make.names(names(AllOtherSchoolsforLM))
AllOtherSchools_LinearModel <- lm(X2019 ~ Charter + Income + Percent_Econ_Disadv + 
                                    Percent_Minority + Region + Students + TitleI, data=AllOtherSchoolsforLM)
summary(AllOtherSchools_LinearModel)

#Linear Model for all Schools
AllSchools_LM <- CleanforLM(Performance)
names(AllSchools_LM) <- make.names(names(AllSchools_LM))
AllSchools_LinearModel <- lm(X2019 ~ Charter + Income + Percent_Econ_Disadv + Percent_Minority + Region + Students + TitleI, data=AllSchools_LM)
summary(AllSchools_LinearModel)

#Models for 3 clusters only using significant variables from above results (except Region since it's a 5-level factor)
ExcellentSchools_LinearModel_sigonly <- lm(X2019 ~ Percent_Econ_Disadv + TitleI, data=ExcellentSchoolsforLM)
summary(ExcellentSchools_LinearModel_sigonly)

DecliningSchools_LinearModel_sigonly <- lm(X2019 ~ Percent_Econ_Disadv, data=DecliningSchoolsforLM)
summary(DecliningSchools_LinearModel_sigonly)

AllOtherSchools_LinearModel_sigonly <- lm(X2019 ~ Charter + Income + Percent_Econ_Disadv + Percent_Minority, data=AllOtherSchoolsforLM)
summary(AllOtherSchools_LinearModel_sigonly)

#MACHINE LEARNING FOR PREDICTING GRADES (2019)####
MLData <- Base[,-c(1:2,4,7,9:19)]
colnames(MLData)[colnames(MLData) == "2019"] <- "Grade"
#Convert to Factor
MLData <- MLData %>%
  mutate_at(c(1,4:6),factor)
#Setup test and train data
set.seed(5678)
pd <- sample(2,nrow(MLData), replace = TRUE, prob = c(.75,.25))
train <- MLData[pd==1,]
test <- MLData[pd==2,]

#KSVM
ksvmresults <- ksvm(Grade~., data = train, kernel = "rbfdot",
                    kpar="automatic",C=10, cross=10, prob.model=TRUE)
ksvmresults
#Confusion Matrix for Train
confusionMatrix(predict(ksvmresults,train),train$Grade)
#Confusion Matrix for Test
confusionMatrix(predict(ksvmresults,test),test$Grade)

#SVM
svmresults <- svm(Grade~., data = train, kernel = "radial",
                  kpar="automatic",C=10, cross=10, prob.model=TRUE)
svmresults

#Confusion Matrix for Train
confusionMatrix(predict(svmresults,train),train$Grade)
#Confusion Matrix for Test
confusionMatrix(predict(svmresults,test),test$Grade)

#Decision Tree
tree <- ctree (Grade ~ .,train)
plot(tree)
#Confusion Matrix for Train
confusionMatrix(predict(tree,train),train$Grade)
#Confusion Matrix for Test
confusionMatrix(predict(tree,test),test$Grade)

#Naive Bayes
NBmodel <- naiveBayes(Grade~., data=train)
#Confusion Matrix for Train
confusionMatrix(predict(NBmodel,train),train$Grade)
#Confusion Matrix for Test
confusionMatrix(predict(NBmodel,test),test$Grade)

#Random Forest
set.seed(9876)
RFmodel <- randomForest(Grade~., data=train)
#Confusion matrix with train data
confusionMatrix(predict(RFmodel,train),train$Grade)
#Confusion matrix with test data
confusionMatrix(predict(RFmodel,test),test$Grade)
#Plot Variables of Importance
varImpPlot(RFmodel, sort = TRUE)

#MODELS TO PREDICT "LABEL"####
Label <- Performance %>%
  select(Region, Income, Students, Charter, TitleI, Percent_Minority,Percent_Econ_Disadv,Label) %>%
  mutate_at(c(1,4:5,8),factor)
#Setup test and train data
trainLabel <- Label[pd==1,]
testLabel <- Label[pd==2,]

#KSVM
ksvmLabel <- ksvm(Label~., data = trainLabel, kernel = "rbfdot",
                  kpar="automatic",C=10, cross=10, prob.model=TRUE)
ksvmLabel
#Confusion matrix with train data
confusionMatrix(predict(ksvmLabel,trainLabel),trainLabel$Label)
#Confusion matrix with test data
confusionMatrix(predict(ksvmLabel,testLabel),testLabel$Label)

#SVM
svmLabel <- svm(Label~., data = trainLabel, kernel = "radial",
                kpar="automatic",C=5, cross=5, prob.model=TRUE)
svmLabel
#Confusion matrix with train data
confusionMatrix(predict(svmLabel,trainLabel),trainLabel$Label)
#Confusion matrix with test data
confusionMatrix(predict(svmLabel,testLabel),testLabel$Label)


#Decision Tree
treeLabel <- ctree (Label ~ .,trainLabel)
(plot(treeLabel))
#Confusion matrix with train data
confusionMatrix(predict(treeLabel,trainLabel),trainLabel$Label)
#Confusion matrix with test data
confusionMatrix(predict(treeLabel,testLabel),testLabel$Label)

#NaiveBayes
NBmodelLabel <- naiveBayes(Label~., data=trainLabel)
#Confusion matrix with train data
confusionMatrix(predict(NBmodelLabel,trainLabel),trainLabel$Label)
#Confusion matrix with test data
confusionMatrix(predict(NBmodelLabel,testLabel),testLabel$Label)

#Random Forest
set.seed(9943)
RFmodelLabel <- randomForest(Label~., data=trainLabel)
#Confusion matrix with train data)
confusionMatrix(predict(RFmodelLabel,trainLabel),trainLabel$Label)
#Confusion matrix with test data
confusionMatrix(predict(RFmodelLabel,testLabel),testLabel$Label)
#Plot Variables of Importance
varImpPlot(RFmodelLabel, sort = TRUE)


#UNDERSAMPLING FOR PREDICTING LABEL####
fixSample_Label <- Label %>%
  filter(Label=="Excellent" | Label =="Declining")
OtherOnly <- Label %>%
  filter(Label=="Other")
set.seed(384)
pd_Other <- sample(2,nrow(OtherOnly), replace = TRUE, prob = c(.75,.25))
OtherOnly <- OtherOnly[pd_Other==2,]
fixSample_Label <- rbind(fixSample_Label,OtherOnly)

#Setup test and train data
pd_fixSample <- sample(2,nrow(fixSample_Label), replace = TRUE, prob = c(.75,.25))
trainLabel_fixSample <- fixSample_Label[pd_fixSample==1,]
testLabel_fixSample <- fixSample_Label[pd_fixSample==2,]

#KSVM
ksvmlabelunder <- ksvm(Label~., data = trainLabel_fixSample, kernel = "rbfdot",
                       kpar="automatic",C=10, cross=10, prob.model=TRUE)
ksvmlabelunder
#Confusion Matrix for Train and Test Data
confusionMatrix(predict(ksvmlabelunder,trainLabel_fixSample),trainLabel_fixSample$Label)
confusionMatrix(predict(ksvmlabelunder,testLabel_fixSample),testLabel_fixSample$Label)

#SVM
svmlabelunder <- svm(Label~., data = trainLabel_fixSample, kernel = "radial",
                     kpar="automatic",C=5, cross=5, prob.model=TRUE)
svmlabelunder
#Confusion Matrix for Train and Test Data
confusionMatrix(predict(svmlabelunder,trainLabel_fixSample),trainLabel_fixSample$Label)
confusionMatrix(predict(svmlabelunder,testLabel_fixSample),testLabel_fixSample$Label)

#Decision Tree
treeLabel_fixSample <- ctree (Label ~ .,trainLabel_fixSample)
(plot(treeLabel_fixSample))
#Confusion Matrix for Train and Test Data
confusionMatrix(predict(treeLabel_fixSample,trainLabel_fixSample),trainLabel_fixSample$Label)
confusionMatrix(predict(treeLabel_fixSample,testLabel_fixSample),testLabel_fixSample$Label)

#NaiveBayes
NBmodelLabel_fixSample <- naiveBayes(Label~., data=trainLabel_fixSample)
#Confusion Matrix for Train and Test Data
confusionMatrix(predict(NBmodelLabel_fixSample,trainLabel_fixSample),trainLabel_fixSample$Label)
confusionMatrix(predict(NBmodelLabel_fixSample,testLabel_fixSample),testLabel_fixSample$Label)

#Random Forest
set.seed(9543)
RFmodelLabel_fixSample <- randomForest(Label~., data=trainLabel_fixSample)
#Confusion matrix with train data)
confusionMatrix(predict(RFmodelLabel_fixSample,trainLabel_fixSample),trainLabel_fixSample$Label)
#Confusion matrix with test data
confusionMatrix(predict(RFmodelLabel_fixSample,testLabel_fixSample),testLabel_fixSample$Label)
#Plot Variables of Importance
varImpPlot(RFmodelLabel_fixSample, sort = TRUE)


#VISUALIZATIONS GENERAL####

#Piechart for business problem
myPalette <- c("cadetblue3","darkblue","darkseagreen","coral1")
PieChartLabels <- c("Building Capacity","Funds for Data-Driven Initiatives", "Performance Based Incentives", "Support for Underperforming Schools") 
BudgetInDollars <- c(31219426, 25819420, 6500000, 123750850) 
pie(BudgetInDollars , labels = PieChartLabels, border="white",col=myPalette,main="Student Performance\nSupport Budget\nTotal ~ $187M")

#Correlation Matrix for continuous variables
corrplot(cor(Base[,-c(1:4,7:21)]))

#Title I and Charter Schools viz
TitleI_AllSchools <- ggplot(data=Base, aes(x=TitleI,fill=TitleI)) + geom_bar() + ggtitle("All Schools - Title I") + theme_fivethirtyeight()+ theme(panel.grid.major = element_blank())
TitleI_AllSchools
Charter_AllSchools <- ggplot(data=Base, aes(x=Charter,fill=Charter)) + geom_bar() + ggtitle("All Schools - Charter") + theme_fivethirtyeight()+ theme(panel.grid.major = element_blank())
Charter_AllSchools

#Histogram of Percent Economically Disadvantaged for Excellent Schools
hist_PerEconDis_ExcellentSchools <- Performance %>% 
  filter(Label=="Excellent") %>%
  ggplot(aes(x=Percent_Econ_Disadv)) + geom_histogram(bins=12) + theme_fivethirtyeight()+ theme(panel.grid.major = element_blank(),axis.text.y=element_blank()) + ggtitle("Histogram of % Economically Disadvantaged\n for Excellent Schools") + scale_x_continuous(labels = function(x) paste0(x, '%'))
hist_PerEconDis_ExcellentSchools

#Histogram of Percent Economically Disadvantaged for Declining Schools
hist_PerEconDis_DecliningSchools <- Performance %>% 
  filter(Label=="Declining") %>%
  ggplot(aes(x=Percent_Econ_Disadv)) + geom_histogram(bins=12) + theme_fivethirtyeight()+ theme(panel.grid.major = element_blank(),axis.text.y=element_blank()) + ggtitle("Histogram of % Economically Disadvantaged\n for Declining Schools") + scale_x_continuous(labels = function(x) paste0(x, '%'))
hist_PerEconDis_DecliningSchools

#Bar graph showing distribution of Region
Region_Viz <- ggplot(data=Base, aes(x=Region,fill=Region)) + geom_bar() + theme_fivethirtyeight() + theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(size = 15),legend.position = "none",panel.grid.major = element_blank()) + ggtitle("# of Schools in each Region")
Region_Viz

#Visualization for Title I schools in Excellent Schools
(TitleI_Excellent <- ggplot(data=ExcellentSchools, aes(x=TitleI,fill=TitleI)) + geom_bar() + ggtitle("Excellent Schools - Title I") + ylab("Title I") + theme_fivethirtyeight() + theme(panel.grid.major = element_blank()))

#Bar graph showing distribution of Excellent, Declining, Other
Label_Viz <- ggplot(data=Performance, aes(x=Label,fill=Label)) + geom_bar() + theme_fivethirtyeight() + theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(size = 20),legend.position = "none",panel.grid.major = element_blank()) + ggtitle("# of Schools in each Category")
Label_Viz

#Animation of District Average Grades

#Prep data, use only Grades over years and District, average Grades by District
avgscoreDistrict <- Performance[,c(1,8:19)] %>%
  group_by(District) %>%
  summarise(across(everything(),mean)) %>%
  melt(id="District")
colnames(avgscoreDistrict) <- c("District","Year","Average_Score")
avgscoreDistrict$Year <- as.integer(as.character(avgscoreDistrict$Year))

#Animated GIF to show average scores by District have movement over time
graph1 <- ggplot(avgscoreDistrict) +
  geom_point(aes(x=District, y=Average_Score, color=District)) + 
  theme(panel.grid.major = element_blank(),axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=c("0" = "F", "1" = "D","2" = "C","3"="B","4"="A")) +
  ggtitle("Average Score of Schools in District\n 2008-2019")
graph1.animation <- graph1 +
  transition_time(Year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1) + theme(legend.position = "none")
graph2.animation <- animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 15, end_pause = 60, res =100, renderer = gifski_renderer())
#This functions saves into working directory, latest animation created
anim_save("district_avg_score.gif")

#MAP VISUALIZATIONS####

#Clean the "Base$ZipCode" data using "zipcode" dataset
data(zipcode)
Base$ZipCode <- clean.zipcodes(Base$ZipCode)

#Merge the "Base" and "zipcode" datasets to obtain latlon info
BaseNew <- merge(Base, zipcode, by.x="ZipCode", by.y="zip", all.x=TRUE, all.y=FALSE)

#Call the FL state boundaries data for our map
states <- map_data("state")
FLstate <- subset(states, region=="florida")

#Create a base map of FL
FLbase <- ggplot(data=FLstate, mapping=aes(x=long, y=lat)) + coord_fixed(1.3) + geom_polygon(color="black", fill="cornsilk2") + theme_nothing()
FLbase

#Plot points on the "FLbase" map for all of the schools being assessed
map.all.schools <- FLbase + geom_point(data=BaseNew, aes(x=longitude, y=latitude)) + ggtitle("title")
map.all.schools

#Merge the "ExcellentSchools" and "zipcode" datasets to obtain latlon info
ExcellentSchoolsNew <- merge(ExcellentSchools, zipcode, by.x="ZipCode", by.y="zip", all.x=TRUE, all.y=FALSE)

#Plot points on the "FLbase" map for all of the schools that were excelling
map.excellent.schools <- FLbase + geom_point(data=ExcellentSchoolsNew, aes(x=longitude, y=latitude))
map.excellent.schools

#Merge the "DecliningSchools" and "zipcode" datasets to obtain latlon info
DecliningSchoolsNew <- merge(DecliningSchools, zipcode, by.x="ZipCode", by.y="zip", all.x=TRUE, all.y=FALSE)

#Plot points on the "FLbase" map for all of the schools that were declining
map.declining.schools <- FLbase + geom_point(data=DecliningSchoolsNew, aes(x=longitude, y=latitude))
map.declining.schools

#Plot points on the "FLbase" map for the schools being assessed where green represents excelling and red represents declining
map.colors.schools <- FLbase + geom_point(data=ExcellentSchoolsNew, aes(x=longitude, y=latitude), color="chartreuse4") + geom_point(data=DecliningSchoolsNew, aes(x=longitude, y=latitude), color="brown3") 
map.colors.schools

#Merge the "AllOtherSchools" and "zipcode" datasets to obtain latlon info
AllOtherSchoolsNew <- merge(AllOtherSchools, zipcode, by.x="ZipCode", by.y="zip", all.x=TRUE, all.y=FALSE)

#Plot points on the "FLbase" map for all of the schools that were neither excelling nor declining
map.allother.schools <- FLbase+ geom_point(data=AllOtherSchoolsNew, aes(x=longitude, y=latitude))
map.allother.schools

#Show map by 2019 Grade
ForMap <- merge(Base, zipcode, by.x="ZipCode", by.y="zip", all.x=TRUE, all.y=FALSE)
FL_Map <- ggplot(data=FLstate, mapping=aes(x=long, y=lat)) + coord_fixed(1.3) + geom_polygon(color="black", fill="cornsilk2") + ggtitle("title") + theme_nothing(legend=TRUE)
(Map_Grades <- FL_Map + geom_point(data=ForMap, aes(x=longitude, y=latitude,color=`2019`)) + ggtitle("Schools labeled by their 2019 Grade") + theme(plot.title = element_text(hjust = 0.5)))

#DESCRIPTIVE STATISTICS ####
#Average income by region
avgIncome <- mean(Performance$Income)
avgIncome

#Histogram of income
incomeHist <- ggplot(Performance, aes(x=Income)) + 
  geom_histogram(binwidth=10, color="black", fill="gray") + 
  theme(plot.title = element_text(hjust=.5)) + labs(title = "Income Histogram")
incomeHist

#Boxplot of income
incomeBoxplot <- ggplot(Performance, aes(x=Income, y=Region)) + 
  geom_boxplot(color="black", fill="gray") + 
  theme(plot.title = element_text(hjust=.5)) + labs(title = "Income Boxplot")
incomeBoxplot

#Avg Income by Region
avgIncomeByRegion <- tapply(Performance$Income, Performance$Region, mean)
avgIncomeByRegion

#Median Income by Region
medianIncomeByRegion <- tapply(Performance$Income, Performance$Region, median)
medianIncomeByRegion

#Histogram of students
studentsHist <- ggplot(Performance, aes(x=Students)) + 
  geom_histogram(binwidth=50, color="black", fill="gray") + 
  theme(plot.title = element_text(hjust=.5)) + labs(title = "Students Histogram")
studentsHist

#Boxplot of students
studentsBoxplot <- ggplot(Performance, aes(x=Students, y=Region)) + 
  geom_boxplot(color="black", fill="gray") + 
  theme(plot.title = element_text(hjust=.5)) + labs(title = "Students Boxplot")
studentsBoxplot

#Avg Students by Region
avgStudentsByRegion <- tapply(Performance$Students, Performance$Region, mean)
avgStudentsByRegion
avgstureg <- data.frame(Region=names(avgStudentsByRegion),Mean=avgStudentsByRegion)
str(avgstureg)
View(avgstureg)

#Plot Avg Students by Region
plotAvgStudentsByRegion <- ggplot(avgstureg, aes(x=Region, y=Mean)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(plot.title = element_text(hjust=.5)) + labs(x="Region", y="Charter Schools", title = "Avg. of Students by Region")
plotAvgStudentsByRegion

#Count Schools by Region
countSchoolsByRegion <- tapply(Performance$School, Performance$Region, length)
countSchoolsByRegion

#Count Charter Schools by Region
countCharterByRegion <- tapply(Performance$School, list(Performance$Region, Performance$Charter=="YES"), length)
countCharterByRegion

#Plot Charter Schools by Region
plotCharterByRegion <- ggplot(Performance, aes(fill=Charter, x=Region, y=Charter)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(plot.title = element_text(hjust=.5)) + labs(x="Region", y="Charter Schools", title = "Charter Schools by Region")
plotCharterByRegion

#Avg % Economically Disadvantaged by Region
avgEconDisadvByRegion <- tapply(Performance$Percent_Econ_Disadv, Performance$Region, mean)
avgEconDisadvByRegion

#Boxplot of % Economically Disadvantaged
disadvantagedBoxplot <- ggplot(Performance, aes(x=Percent_Econ_Disadv, y=Region)) + 
  geom_boxplot(color="black", fill="gray") + 
  theme(plot.title = element_text(hjust=.5)) + labs(title = "% Economically Disadvantaged Boxplot")
disadvantagedBoxplot

(Region2_Focus <- Performance %>%
    filter(Region=="Region_2") %>%
    mutate(Excellent=case_when(Label=="Excellent"~"Excellent Schools",TRUE~"All Other Schools")) %>%
    ggplot(aes(x=Percent_Econ_Disadv,y=Excellent,fill=Excellent)) + geom_boxplot() + theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust=.5),axis.title.y=element_blank(),axis.text.y=element_text(size=10),legend.position = "none") + labs(title = "% Economically Disadvantaged Boxplot\nfor Region 2\nExcellent Schools v. All Other Schools"))