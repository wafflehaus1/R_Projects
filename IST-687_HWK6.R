#HWK 6------------------------------------

## Step 1: Load the data
install.packages("ggplot2")
install.packages("reshape2") 
library(ggplot2)
library(reshape2)

airData <- airquality
View(airData)
str(airData)

#Step 2: Clean the data
any(is.na(airData))
colnames(airData)[colSums(is.na(airData))>0]

#Replace NA's with mean of column
airData$Ozone[is.na(airData$Ozone)] <- mean(airData$Ozone,na.rm=TRUE)
airData$Solar.R[is.na(airData$Solar.R)] <- mean(airData$Solar.R,na.rm=TRUE)
any(is.na(airData))
colnames(airData)[colSums(is.na(airData))>0]

#Step 3: Understand the data distribution
  #Create the following visualizations using ggplot:
    #Histograms for each of the variables
#Histogram for Ozone
hist.ozone <- ggplot(airData, aes(x=Ozone)) + geom_histogram(binwidth=5, color="black", fill="white")
hist.ozone 

#Histogram for Solar
hist.solar <- ggplot(airData, aes(x=Solar.R)) + geom_histogram(binwidth=5, color="black", fill="white")
hist.solar

#Histogram for Wind
hist.wind <- ggplot(airData, aes(x=Wind)) + geom_histogram(binwidth=5, color="black", fill="white")
hist.wind

#Histogram for Temp
hist.temp <- ggplot(airData, aes(x=Temp)) + geom_histogram(binwidth=5, color="black", fill="white")
hist.temp

#Histogram for Month
hist.month <- ggplot(airData, aes(x=Month)) + geom_histogram(binwidth=1, color="black", fill="white")
hist.month

#Histogram for Day
hist.day <- ggplot(airData, aes(x=Day)) + geom_histogram(binwidth=5, color="black", fill="white")
hist.day

#Boxplot for Ozone
boxp.ozone <- ggplot(airData, aes(x=factor(0),Ozone)) + geom_boxplot()
boxp.ozone

#Boxplot for Wind (round the wind to get a good number of "buckets")
W <- round(airData$Wind, digits = 0)
boxp.wind <- ggplot(airData, aes(x=factor(0),W)) + geom_boxplot()
boxp.wind

#Step 3: Explore how the data changes over time
airData$Date <- paste("1973", airData$Month, airData$Day, sep="-")
airData$Date <- as.Date(airData$Date,"%Y-%m-%d")

str(airData)
airNew <- airData[,-5:-6]
str(airNew) 

#Create a Line Chart for each, then one chart with 4 lines
#Line Chart for Ozone
linec.ozone <- ggplot(data = airNew, aes(x=Date, y=Ozone)) + geom_line() + theme_classic(base_size = 10)
linec.ozone

#Line Chart for Temp
linec.temp <- ggplot(data = airNew, aes(x=Date, y=Temp)) + geom_line() + theme_classic(base_size = 10)
linec.temp

#Line Chart for Wind
linec.wind <- ggplot(data = airNew, aes(x=Date, y=Wind)) + geom_line() + theme_classic(base_size = 10)
linec.wind

#Line Chart for Solar.R
linec.solar <- ggplot(data = airNew, aes(x=Date, y=Solar.R)) + geom_line() + theme_classic(base_size = 10)
linec.solar

#Line Chart with 4 Lines and Different Color
linec.all <- ggplot(airNew, aes(x=Date)) + geom_line(aes(y=Ozone, color="Ozone")) + geom_line(aes(y=Temp, color="Temp")) +  geom_line(aes(y=Wind, color="Wind")) + geom_line(aes(y=Solar.R, color="Solar.R")) +  theme(plot.title=element_text(hjust=.5)) + labs(title="Air Quality Variables Over Time") + scale_color_manual(values=c("green4", "orange", "purple", "red"))
linec.all

#Step 4: Look at all the data via a Heatmap
airLong <- melt(airNew, id="Date")
heatmap <- ggplot(airLong, aes(x=Date, y=variable)) + geom_tile(aes(fill=value)) + scale_fill_gradient(low="white", high="red")
heatmap

#Step 5: Look at all the data via a scatter chart
scatterp <- ggplot(airNew) +  geom_point(aes(x=Wind, y=Temp, size=Ozone, color=Solar.R))
scatterp 

#Step 6: Final Analysis
  #Do you see any patterns after exploring the data?
#Solar.R variable seems to have the greatest range in data points over time, as well as the largest impact on Ozone--opposite Wind with the smallest.
#It would appear that June - September would be "Summer" timeframe, as Temp and Ozone drop prior to June and after September.
#When Temp increases, so does the Solar.R and Ozone activity.
#When Wind increases, Solar.R and Ozone activity decreases. 

  #What was the most useful visualization?
#For me, the most useful visualization would be the scatter plot chart. You are able to see that Solar.R
#and Ozone values increase as Wind value decreases and Temp value increases (for the most part - there are some outliers)
#indicating that Wind and Temp have an effect on Ozone and Solar.R.
















