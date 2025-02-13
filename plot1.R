#Load packages

library(data.table)
library(here)
library(dplyr)
library(ggplot2)

#Load data

if(!dir.exists("./data")){dir.create(here("./data"))}#Set-up directory for work and data
#Download data
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
temp <- tempfile() #Temporary variable to save zip file
download.file(url,temp)
#Export data
unzip(temp, list = TRUE) #Explore zip folder
zip_list <- as.vector(unzip(temp, list = TRUE)[,1]) #Save structure
SCC <- readRDS(unzip(temp, zip_list[1]))
NEI <- readRDS(unzip(temp, zip_list[2]))
unlink(temp); rm(temp, url, zip_list) #Delete temporary files

#Q1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 

#Using the base plotting system, make a plot showing the 
#total PM2.5 emission from all sources for each of 
#the years 1999, 2002, 2005, and 2008.

#Total for each year
totalus <- with(NEI, tapply(Emissions, year, sum, na.rm = TRUE))
years <- unique(NEI$year)

png("plot1.png", width = 720, height = 720, units = "px", res = 150)

plot(years, totalus, 
     pch=19, 
     ylab = "Total emissions",
     xlab = "",
     type ="b", 
     main = "Total emissions in the US")

dev.off()
