library(ggplot2)
library(raster)  
library(XML) 
library(rgdal)
library(rgeos)
library(tmap)
library(maptools)
ChangedDem <- JobData

ChangedDem$location[grep('london', ChangedDem$location, ignore.case = TRUE)] = 'London'



City= sapply(strsplit(ChangedDem$location[1:110], ', '), "[",
1)

County= sapply(strsplit(ChangedDem$location[1:110], ', '), "[",
2)
#dont fully understand how sapply split them

County[grep("England", County)] = City[grep("England", County)]

County[grep("London", City)] = City[grep("London", City)]

length(County[grep('London',County)])/
  length(County) #58% london listings

length(County)
length(Salaries$salAverage)

ChangedDem[,county:='NA']
ChangedDem$county[1:110] = County
ChangedDem[,city:='NA']
ChangedDem$city[1:110] = City

