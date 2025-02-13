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


#Q5
#How have emissions from motor vehicle sources 
#changed from 1999–2008 in Baltimore City?

NEI_sector <- unique(SCC$EI.Sector)
motor <- as.vector(NEI_sector[grep("Vehicles", NEI_sector)]) #Subset motor vehicles sections

bltmr_motors <- filter(merge(NEI, SCC[,c(1,4)], by = "SCC"), 
                       fips == "24510" & EI.Sector %in% motor)

yearlymotors <- bltmr_motors %>% group_by(EI.Sector, year) %>%
  summarise(TotalEmissions = sum(Emissions))

png("plot5.png", width = 720, height = 720, units = "px", res = 150)

ggplot(yearlymotors, aes(year, TotalEmissions)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(EI.Sector~., ncol = 2, 
             labeller = labeller(EI.Sector = c("Mobile - On-Road Gasoline Light Duty Vehicles" = "Gasoline Light Duty Vehicles",
                                               "Mobile - On-Road Gasoline Heavy Duty Vehicles" = "Gasoline Heavy Duty Vehicles", 
                                               "Mobile - On-Road Diesel Light Duty Vehicles" = "Diesel Light Duty Vehicles",
                                               "Mobile - On-Road Diesel Heavy Duty Vehicles" = "Diesel Heavy Duty Vehicles"))) +  
  labs(x = "", y = "Total Emissions")

dev.off()
