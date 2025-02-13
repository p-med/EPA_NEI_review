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

#Q4
#Across the United States, how have emissions 
#from coal combustion-related sources changed from 1999–2008?

NEI_sector <- unique(SCC$EI.Sector)
coalcat <- as.vector(NEI_sector[grep("Coal",NEI_sector)])

coalemissions <- filter(merge(NEI, SCC[,c(1,4)], by = "SCC"), 
                        EI.Sector %in% coalcat)

yearlycoal <- coalemissions %>% group_by(EI.Sector, year) %>%
  summarise(TotalEmissions = sum(Emissions))

png("plot4.png", width = 720, height = 720, units = "px", res = 150)

ggplot(yearlycoal, aes(year, TotalEmissions)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(EI.Sector~., ncol = 1, 
             labeller = labeller(EI.Sector = c("Fuel Comb - Electric Generation - Coal" = "Electric Generation",
                                               "Fuel Comb - Industrial Boilers, ICEs - Coal" = "Industrial Boilers, ICEs", 
                                               "Fuel Comb - Comm/Institutional - Coal" = "Comm/Institutional"))) + 
  labs(x = "", y = "Total Emissions")

dev.off()
