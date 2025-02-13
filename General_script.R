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

plot(years, totalus, 
     pch=19, 
     ylab = "Total emissions",
     xlab = "",
     type ="b", 
     main = "Total emissions in the US")


#Q2
#Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (fips == "24510") from 1999 to 2008? 

#Use the base plotting system to make a plot answering this question.

btmr <- subset(NEI, fips == "24510")

totalbtmr <- with(btmr, tapply(Emissions, year, sum, na.rm = TRUE))

plot(years, totalbtmr, 
     pch=19, 
     ylab = "Total emissions",
     xlab = "",
     type ="b", 
     main = "Total emissions in Baltimore, MD")

#Q3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999–2008
#for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 

#Use the ggplot2 plotting system to make a plot answer this question.

typebtmr <- btmr %>% group_by(year, type) %>%
  summarize(pm25 = sum(Emissions))

bltmr_plot <- ggplot(typebtmr, 
                     aes(year, pm25, colour = type)) 
                      + geom_line() 
                      + geom_point() 
                      + facet_grid(type~.)

#Q4
#Across the United States, how have emissions 
#from coal combustion-related sources changed from 1999–2008?

NEI_sector <- unique(SCC$EI.Sector)
coalcat <- as.vector(NEI_sector[grep("Coal",NEI_sector)])

coalemissions <- filter(merge(NEI, SCC[,c(1,4)], by = "SCC"), 
                        EI.Sector %in% coalcat)

yearlycoal <- coalemissions %>% group_by(EI.Sector, year) %>%
  summarise(TotalEmissions = sum(Emissions))

ggplot(yearlycoal, aes(year, TotalEmissions)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(.~EI.Sector) + 
  labs(x = "", y = "Total Emissions")

#Q5
#How have emissions from motor vehicle sources 
#changed from 1999–2008 in Baltimore City?

motor <- as.vector(NEI_sector[grep("Vehicles", NEI_sector)]) #Subset motor vehicles sections

bltmr_motors <- filter(merge(NEI, SCC[,c(1,4)], by = "SCC"), 
                        fips == "24510" & EI.Sector %in% motor)

yearlymotors <- bltmr_motors %>% group_by(EI.Sector, year) %>%
  summarise(TotalEmissions = sum(Emissions))

ggplot(yearlymotors, aes(year, TotalEmissions)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(.~EI.Sector, ncol=2) + 
  labs(x = "", y = "Total Emissions")

#Q6
#Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles 
#County, California (fips == "06037").
#Which city has seen greater changes over time in motor vehicle emissions?

motor <- as.vector(NEI_sector[grep("Vehicles", NEI_sector)])

LA_bltmr <- filter(merge(NEI, SCC[,c(1,4)], by = "SCC"),
                     fips == "24510" | 
                     fips == "06037")

LA_bltmr <- filter(LA_bltmr, EI.Sector %in% motor)

LA_bltmr <- LA_bltmr %>% mutate(city = case_when(
  (fips == "24510") ~ "Baltimore",
  (fips == "06037") ~ "Los Angeles"
))

LA_bltmr_tot <- LA_bltmr %>% group_by(city, EI.Sector, year) %>%
  summarise(TotalEmissions = sum(Emissions))


ggplot(LA_bltmr_tot, aes(year, TotalEmissions, colour = EI.Sector)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(city~.) + 
  labs(x = "", y = "Total Emissions") +
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())+
  guides(color=guide_legend(nrow=2, byrow=TRUE))

