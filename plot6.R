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

png("plot6.png", width = 720, height = 720, units = "px", res = 150)

ggplot(LA_bltmr_tot, aes(year, TotalEmissions, colour = EI.Sector)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(city~.) + 
  labs(x = "", y = "Total Emissions") +
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())+
  guides(color=guide_legend(nrow=2, byrow=TRUE))

dev.off()
