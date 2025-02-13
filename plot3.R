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

#Q3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999–2008
#for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 

#Use the ggplot2 plotting system to make a plot answer this question.

btmr <- subset(NEI, fips == "24510")

typebtmr <- btmr %>% group_by(year, type) %>%
  summarize(pm25 = sum(Emissions))

png("plot3.png", width = 720, height = 720, units = "px", res = 150)

ggplot(typebtmr, aes(year, pm25, colour = type)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(type~.) +
  labs(x = "", y = "Total Emissions") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())+
  guides(color=guide_legend(nrow=2, byrow=TRUE))

dev.off()
