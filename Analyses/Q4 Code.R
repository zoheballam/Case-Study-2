###Question 4###
#Set Working directory
setwd("C:/Users/Laptop/Desktop/CaseStudy_2")

##Question 1##

#download the data into R
TempData <- read.csv("TEMP.csv", header=T, na.strings=c("","NA"))
str(TempData)

#Change the original year variable so that we have a new column with just the year information
TempData$Year = as.numeric(gsub("^.*([0-9]{4}).*$", "\\1" , TempData$Date))

#Drop rows that have missing
TempDataComplete = TempData[!is.na(TempData$Monthly.AverageTemp),]

#Calculate the diff between max and min temp for each country using this function  
rangediff = function(x) {
  range = range(x)
  range[2] - range[1]
}
TempData2 = tapply(TempDataComplete$Monthly.AverageTemp, TempDataComplete$Country, rangediff)
str(TempData2) 

#Create a data subset only including data points greater than 1900
TempData1900plus = TempDataComplete[TempDataComplete$Year >= "1900",]

#The code belows identifies and sorts the top 20 countries with the maximum difference
TempDataRanges = tapply(TempData1900plus$Monthly.AverageTemp, TempData1900plus$Country, rangediff)
Rangestop20 = sort(TempDataRanges)[1:20]
Rangestop20

#Graph the relationship
plot(Rangestop20, xlab = "Country", ylab = "Temperature")

##Question 2##
TempsData1990plus = TempDataComplete[TempDataComplete$Year >= "1990",] 
USTempData = TempsData1990plus[TempsData1990plus$Country == 'United States',]

# Convert from Fahrenheit to Celsius.
library(weathermetrics)
USTempData$temp.fahr <- celsius.to.fahrenheit(USTempData$Monthly.AverageTemp, round = 2)

#plot with average land temperature by year
UStempavtemp = tapply(USTempData$temp.fahr, USTempData$Year, mean)
xaxis = 1990 + 0:(length(UStempavtemp)-1)
plot(xaxis, UStempavtemp, xlab = 'Year', ylab='Temperature')

##Question 3##
#download the data into R
CityTempData <- read.csv("CityTemp.csv", header=T, na.strings=c("","NA"))
str(CityTempData)

#Change the original year variable so that we have a new column with just the year information
CityTempData$Year = as.numeric(gsub("^.*([0-9]{4}).*$", "\\1" , CityTempData$Date))

#Drop rows that have missing
CityTempComplete = CityTempData[!is.na(CityTempData$Monthly.AverageTemp),]

#Difference in temps for each city
CityTempData2 = tapply(CityTempComplete$Monthly.AverageTemp, CityTempComplete$City, rangediff)
str(CityTempData2) 

#Create a data subset only including data points greater than 1900
CityTempData1900plus = CityTempComplete[CityTempComplete$Year >= "1900",]

#The code belows identifies and sorts the top 20 countries with the maximum difference
CityTempDataRanges = tapply(CityTempData1900plus$Monthly.AverageTemp, CityTempData1900plus$City, rangediff)
CityRangestop20 = sort(CityTempDataRanges)[1:20]
CityRangestop20

#Graph the relationship
plot(CityRangestop20, xlab = "Country", ylab = "Temperature")
