#HWK 7 Viz Map HW: Median Income---------------------------------

install.packages("gdata")
install.packages("ggplot2")
install.packages("openintro")
install.packages("ggmap")
install.packages("readxl")
library(gdata)
library(ggplot2)
library(openintro)
library(ggmap)
library(readxl)
library(sqldf)

#Step 1: Load the data

  #1) Read the data - using the gdata package we have previously used.
setwd("C:\\Users\\mstoc\\Desktop")
df <- read_xlsx("MedianZIP.xlsx")

  #2) Clean up the dataframe
    #2a) Remove any info at the front of the file that's not needed
df <- df[-1,]

    #2b) Update column names (zip, median, mean, population)
colnames(df) <- c("zip","median","mean","population")

      #Transform columns to numbers and remove commas
df$median <- as.numeric(gsub(",", "", df$median))
df$mean <- as.numeric(gsub(",", "", df$mean))
df$population <- as.numeric(gsub(",", "", df$population))

  #3) Load the 'zipcode' package
#installed from desktop .tar file
library(zipcode)
data(zipcode)
df$zip <- clean.zipcodes(df$zip)

  #4) Merge the zip code information from two data frames(merge into one dataframe)
dfMerged <- merge(df, zipcode, by="zip")

  #5) Remove Hawaii and Alaska (just focus on the 'lower 48' states)
dfMerged <- dfMerged[dfMerged$state != "HI", ]
dfMerged <- dfMerged[dfMerged$state != "AK", ]
dfMerged <- dfMerged[dfMerged$state != "DC", ]


#Step 2: Show the income & population per state
  #1) Create a simpler dataframe with median income and population for each state
income <- tapply(dfMerged$median, dfMerged$state, mean)
state <- rownames(income)
medInc <- data.frame(state, income)
pop <- tapply(dfMerged$population, dfMerged$state, sum)
state <- rownames(pop)
statePop <- data.frame(state,pop)
dfSimple <- merge(medInc, statePop, by="state")

dfSimple<- sqldf("select state, avg(median) as income, sum(population) as pop from dfMerged group by state")
head(dfSimple)

  #2) Add state abbreviations and state names (lower case)
match(dfSimple$state,state.abb)
dfSimple$stateName <- state.name[match(dfSimple$state,state.abb)]
dfSimple$stateName <- tolower(dfSimple$stateName)


  #3) Show us map, representing color with average median income
us <- map_data("state")
map.income <- ggplot(dfSimple, aes(map_id=stateName))
map.income <- map.income + geom_map(map=us, aes(fill=income))
map.income <- map.income + expand_limits(x=us$long, y=us$lat)
map.income <- map.income + coord_map()
map.income <- map.income + ggtitle("Average Median Income by State") + theme(plot.title = element_text(hjust=0.5))
map.income

remove_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

  #4) Create a second map with color representing state population
map.pop <- ggplot(dfSimple, aes(map_id=stateName))
map.pop <- map.pop + geom_map(map=us, aes(fill=pop))
map.pop <- map.pop + expand_limits(x=us$long, y=us$lat)
map.pop <- map.pop + coord_map()
map.pop <- map.pop + ggtitle("Population by State") + theme(plot.title = element_text(hjust=0.5))
map.pop <- map.pop  + remove_the_axes
map.pop

#Step 3: Show income per zip code
  #1) Draw each zip code on map where color of dot is based on median income
dfMerged$stateName <- state.name[match(dfMerged$state,state.abb)]
dfMerged$stateName <- tolower(dfMerged$stateName)
map.zip <- ggplot(dfMerged, aes(map_id=stateName))
map.zip <- map.zip + geom_map(map=us, fill="black", color="white")
map.zip <- map.zip + expand_limits(x=us$long, y=us$lat)
map.zip <- map.zip + geom_point(data=dfMerged, aes(x=longitude, y=latitude, color=median))
map.zip <- map.zip + coord_map()
map.zip <- map.zip + ggtitle("Income per Zip Code") + theme(plot.title=element_text(hjust=0.5))
map.zip <- map.zip  + remove_the_axes
map.zip
head(dfMerged)

#Step 4: Show Zip Code Density
  #1) Now generate a different map, one where we can easily see where there are lots of zip codes, and where there are few
map.density <- ggplot(dfMerged, aes(map_id=stateName))
map.density <- map.density + geom_map(map=us, fill="black", color="white")
map.density <- map.density + expand_limits(x=us$long, y=us$lat)
map.density <- map.density + geom_density_2d(data=dfMerged, aes(x=longitude, y=latitude))
map.density <- map.density + coord_map()
map.density <- map.density + ggtitle("Zip Code Density") + theme(plot.title=element_text(hjust=0.5))
map.density

#Step 5: Zoom in to the region around NYC (using Syracuse instead)
install.packages("tidyverse")

library(jsonlite)
library(tidyverse)

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

suppressPackageStartupMessages(library(dplyr))

addresses <- c("syracuse University, syracuse")

d <- suppressWarnings(lapply(addresses, function(address) {
  t <- Sys.time()
  api_output <- nominatim_osm(address)
  t <- difftime(Sys.time(), t, 'secs')
  return(data.frame(address = address, api_output, elapsed_time = t))
}) %>% bind_rows() %>% data.frame())

d
d[2]
d[3]

NewLatLon<-function(addresses){
  d <- suppressWarnings(lapply(addresses, function(address) {
    t <- Sys.time()
    api_output <- nominatim_osm(address)
    t <- difftime(Sys.time(), t, 'secs')
    return(data.frame(address = address, api_output, elapsed_time = t))
  }) %>%
    bind_rows() %>% data.frame())
  return(d)}

#Return longitude and latitude values for a string
latlon<-NewLatLon("Syracuse University, NY")
latlon$lon
latlon$lat

  #5a) Show income per zip code around NYC
zoomGeo <- latlon
zoomAmount <- 10
centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)
map.zip.zoom <- map.zip + xlim(xlimit) + ylim(ylimit) + coord_map()
map.zip.zoom <- map.zip.zoom + geom_point(aes(x=centerx, y=centery), color="darkred", size=3)
map.zip.zoom <- map.zip.zoom + ggtitle("Income by Zip Near Syracuse") + theme(plot.title=element_text(hjust=0.5))
map.zip.zoom 
  #5b) Show Zip Code Density Near Syracuse
map.density.zoom <- map.density + xlim(xlimit) + ylim(ylimit) + coord_map()
map.density.zoom <- map.density.zoom + geom_point(aes(x=centerx, y=centery), color="darkred", size=3)
map.density.zoom <- map.density.zoom + ggtitle("Zip Code Density Near Syracuse") + theme(plot.title=element_text(hjust=0.5))
map.density.zoom
