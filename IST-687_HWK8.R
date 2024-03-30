#HWK 8 ---------------------------------------
install.packages("readxl")
install.packages("ggplot2")

#Read in data from the following URL:
#http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls

library(readxl)
library(ggplot2)

#Step 1:  Read data into R
df <- data.frame(read_excel("C:\\Users\\mstoc\\Desktop\\hwk8data.xls", na="empty"))
#rename columns
colnames(df) <- c("fawn", "adultPop", "annualPrecip", "badWinter")
#add a year column to add time variable
df$year <- seq(1, 8, by = 1)

#Step 2: Saved to local PC

#Step 3: Inspect the data to ensure 8 observations of 4 variables**added year variable**
str(df)
View(df)

#Step 4: Create Bivariate Plots
  #Number of baby fawns versus adult antelope population
ggplot(df, aes(adultPop, fawn)) + geom_point() + labs(x = 'Adult Antelope Population', y = 'Avg. Fawn per Spring') + ggtitle('Adult Antelope Population vs. Avg. Fawn per Spring')

  #the precipitation that year
ggplot(df, aes(year, annualPrecip)) + geom_line() + labs(x = 'Year', y = 'Annual Precipitation') + ggtitle('Precipitation by Year')

  #the severity of the winter
ggplot(df, aes(year, badWinter)) + geom_line() + labs(x = 'Year', y = 'Number of Bad Winters') + ggtitle('The Severity of Winters')

#Step 5: Create Linear Regression Models
  #Predict the number of fawns from the severity of winter
fawnVSwinter <- lm(fawn ~ badWinter, data = df)
summary(fawnVSwinter)

  #Predict the number of faws from two variables (one should be the severity of the winter)
fawnVStwo <- lm(fawn ~ badWinter + annualPrecip, data = df)
summary(fawnVStwo)
  
  #Predict the number of fawns from the three other variables
fawnVSall <- lm(fawn ~ badWinter + annualPrecip + adultPop, data = df)
summary(fawnVSall)

  #Which model works best?
#the fawn vs all model yields the highest adjusted R squared value - the most reliable model of the three in regards to how x affects y.

  #Which of the predictors are statistically significant in each model?
#in model 1:
#badWinter was statistically significant with 0.03 p-value, overall model had 47% adjusted R-squared value.

#in model 2:
#annualPrecpip was statistically significant with 0.008 p-value, badWinter was not statistically significant (0.188). Overall model had 86% adjusted R-squared value.

#in model 3:
#all coeffecients were statistically significant in model 3, with an overall model adjusted R-squared value of 95%. 

  #If you wanted to create the most parsimonious model, what would it contain?
#I would replicate fawnVSall model; all statistically significant values and very high adjusted R-squared value of 95%.

