## Home Value Patrol - A study to understand the effect of crime on home values in Seattle

# Set Working Directory
setwd("C:/Users/UW_STUDENT_Virtual/Desktop/572/572 - Group Project")

# Required Packages for Project
# 1. gpclib: General Polygon Clipping Library for R
# 2. rgeos: Interface to Geometry Engine - Open Source (GEOS)
# 3. maptools: Tools for Reading and Handling Spatial Objects
# 4. rgdal: Bindings for the Geospatial Data Abstraction Library
# 5. ggplot2: graphing library for R
# 6. spatialEco: Spatial Analysis and Modelling
# 7. tidyr: Data cleaning operations
# 8. dplyr: Data Manipulation

install.packages("gpclib")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("spatialEco")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggmap")

# Loading the packages

library("tidyr")
library("dplyr")
library("gpclib")
library("maptools")
library("rgdal")
library("ggplot2")
library("rgeos")
require("maptools")
library("spatialEco")
library("ggmap")
library("plyr")
library("reshape2")

# Importing Datasets

# Seattle_Police_Department_911_Incident_Response.csv -- Source: data.seattle.gov
# URL: https://data.seattle.gov/Public-Safety/Seattle-Police-Department-911-Incident-Response/3k2p-39jp

raw.data_911 <- read.csv("Seattle_Police_Department_911_Incident_Response.csv")
View(raw.data_911)

# Zhvi_YearlyTrends.csv -- Source: Zillow Data Research
# URL: http://files.zillowstatic.com/research/public/Neighborhood/Neighborhood_Zhvi_AllHomes.csv

raw.data_zillow <- read.csv("Zhvi_YearlyTrends.csv")
View(raw.data_zillow)

# Data Cleaning: Transforming raw data and eliminating redundancies

reqd_cols <- c("Event.Clearance.SubGroup","Event.Clearance.Group","Event.Clearance.Date","Hundred.Block.Location",
               "District.Sector","Zone.Beat","Longitude","Latitude", "Incident.Location")

reqd_events <- c("ARREST", "ASSAULTS","BURGLARY","DISTURBANCES","ROBBERY","TRESSPASS")

clean.data_911 <- subset(raw.data_911, 
                         raw.data_911$Event.Clearance.Group !="NULL" & 
                           raw.data_911$Event.Clearance.Group %in% reqd_events & 
                           raw.data_911$Incident.Location != "NULL"& 
                           raw.data_911$Event.Clearance.Date != "NULL" &
                           raw.data_911$Event.Clearance.Date != "")[reqd_cols]

clean.data_911["Year"]<-substr(as.character(clean.data_911$Event.Clearance.Date),7,10)
clean.data_911["Month"]<-substr(as.character(clean.data_911$Event.Clearance.Date),1,2)
clean.data_911["POSIXct_date"]<-as.POSIXct(substr(as.character(clean.data_911$Event.Clearance.Date),1,10),format="%m/%d/%Y")
clean.data_911 <- subset(clean.data_911, clean.data_911$Year >= 2010)

# Exploratory Analysis on 911 data

# Exploring the number of instances of individual crimes since 2010
clean.data_911$Event.Clearance.Group <- factor(clean.data_911$Event.Clearance.Group)
crime.frequency <- as.data.frame(table(clean.data_911$Event.Clearance.Group))

View(crime.frequency)
colnames(crime.frequency) = c("crime","count")

ggplot(data = crime.frequency, aes(x = crime, y=count,fill=crime)) +
  geom_bar(stat = "identity", color = "white", width = 0.55) +
  ggtitle("Crime Frequency since 2010") +
  theme(legend.text=element_text(size=18),axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"), plot.title = element_text(size=22)) 


# Exploring the crime rates by year

clean.data_911$Year <- factor(clean.data_911$Year)
crime.frequency.year <- as.data.frame(table(clean.data_911$Year))
View(crime.frequency.year)
colnames(crime.frequency.year) = c("year","count")

ggplot(data = crime.frequency.year, aes(x = year, y=count, fill=count)) +
  geom_bar(stat = "identity", color = "white", width = 0.65) +
  ggtitle("Crime statistics by year") +
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), plot.title = element_text(size=22))

# Visualizing monthly trends in crimes
clean.data_911$Month = as.factor(clean.data_911$Month)
monthly.trends <- as.data.frame(table(clean.data_911$Month, clean.data_911$Event.Clearance.Group))
monthly.trends <- subset(monthly.trends,monthly.trends$Var2 %in% reqd_events)
colnames(monthly.trends) <- c("month","crime","value")

ggplot(data=monthly.trends, aes(x=month, y=value, group=crime, colour=crime)) +
  geom_line(size=1.5) +
  geom_point(size=3, fill="white") +
  ggtitle("Crime trends by type by month") +
  theme(legend.text=element_text(size=18),axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), plot.title = element_text(size=22))


# Exploring Seattle Map and plotting our crime data

seattle.map <- qmap('Seattle',zoom=11, maptype = "hybrid")
seattle.map + geom_point(data = clean.data_911, aes(x = clean.data_911$Longitude, y = clean.data_911$Latitude), color = "coral", alpha = 0.02, na.rm = TRUE, size = 4) +
  ggtitle("Crime in Seattle") + 
  xlab("Longitude") +
  ylab("Latitude") 

# Cleaning 911 data further by mapping longitude + latitude to neighborhoods

# Using shape files to draw neighborhoods
map <- readOGR("Neighborhoods.shp", layer="Neighborhoods")

clean.data_911 <- clean.data_911[complete.cases(clean.data_911),]
coordinates(clean.data_911)<-~Longitude+Latitude # Adding coordinates column

# projecting the coordinates to a format that can be read by the shapefile  
# creates a special dataframe called : SpatialPointsDataFrame
proj4string(clean.data_911)<-CRS("+proj=longlat +datum=NAD83")
clean.data_911<-spTransform(clean.data_911, CRS(proj4string(map)))

# Joining the shape file and data_911. 
# The coordinate of each 911 call is mapped to the neighborhood from the shapefile
pts <- point.in.poly(clean.data_911,map)
neighborhood.911_data <- data.frame(pts)
neighborhood.911_data<-neighborhood.911_data[c("Year","Month","Event.Clearance.Group","Event.Clearance.SubGroup","S_HOOD","L_HOOD")] 

# Some more manual cleaning in neighborhoods to refine zones

unique(subset(neighborhood.911_data, neighborhood.911_data$L_HOOD=="CENTRAL AREA")$S_HOOD)
neighborhood.911_data$S_HOOD <-  as.character(neighborhood.911_data$S_HOOD)
neighborhood.911_data$S_HOOD[which(neighborhood.911_data$S_HOOD %in% "Adams")]<-"Ballard"
neighborhood.911_data$S_HOOD[which(neighborhood.911_data$S_HOOD %in% "Mid-Beacon Hill")]<-"Beacon Hill"
neighborhood.911_data$S_HOOD[which(neighborhood.911_data$S_HOOD %in% c("Broadway","Stevens"))] <- "Capitol Hill"
neighborhood.911_data$S_HOOD[which(neighborhood.911_data$S_HOOD %in% c("Central Business District","International District","Pike-Market","Pioneer Square","Yesler Terrace"))]<-"Downtown"
neighborhood.911_data$S_HOOD[which(neighborhood.911_data$S_HOOD %in% c("Briancliff","Lawton Park","Southeast Magnolia"))] <- "Magnolia"
neighborhood.911_data$S_HOOD[which(neighborhood.911_data$S_HOOD %in% c("Atlantic","Harrison/Denny-Blaine","Mann"))] <- "Central"

neighborhood.911_data$Event.Clearance.Group <-  as.factor(neighborhood.911_data$Event.Clearance.Group)

# Initializing new columns in 911 data for each event type for classification with 0/1 flag

neighborhood.911_data$Burglary <- 0
neighborhood.911_data$Arrests <- 0
neighborhood.911_data$Assaults <- 0
neighborhood.911_data$Disturbances <- 0
neighborhood.911_data$Robbery <- 0

neighborhood.911_data$Burglary[which(neighborhood.911_data$Event.Clearance.Group %in% "BURGLARY")]<-1
neighborhood.911_data$Arrests[which(neighborhood.911_data$Event.Clearance.Group %in% "ARREST")]<-1
neighborhood.911_data$Assaults[which(neighborhood.911_data$Event.Clearance.Group %in% "ASSAULTS")]<-1
neighborhood.911_data$Disturbances[which(neighborhood.911_data$Event.Clearance.Group %in% "DISTURBANCES")]<-1
neighborhood.911_data$Robbery[which(neighborhood.911_data$Event.Clearance.Group %in% "ROBBERY")]<-1


## Cleaning up Zillow Dataset
# Eliminating all cities except City == Seattle

raw.data_zillow <- subset(raw.data_zillow,raw.data_zillow$City=="Seattle")

raw.data_zillow$Metro <- NULL
raw.data_zillow$City <- NULL
raw.data_zillow$State <- NULL
raw.data_zillow$CountyName <- NULL

rownames(raw.data_zillow) <- raw.data_zillow[,"RegionName"]
raw.data_zillow["RegionName"] <- NULL
clean.data_zillow <- data.frame(t(raw.data_zillow))
clean.data_zillow["ym"] <- rownames(clean.data_zillow)
clean.data_zillow["Year"] <- substr(clean.data_zillow$ym,2,5)
clean.data_zillow["Month"] <- substr(clean.data_zillow$ym,7,8)
clean.data_zillow["ym"] <- NULL
rownames(clean.data_zillow) <- NULL
clean.data_zillow <- clean.data_zillow %>% gather("Neighborhood",zvhi_score, Capitol.Hill:Jackson.Place)
clean.data_zillow$Neighborhood <- gsub("[.]"," ",clean.data_zillow$Neighborhood)
colnames(clean.data_zillow) <- c("Year","Month","Neighborhood","home.value")

# Manual neighborhood cleanup for Zillow - 911 intersection

clean.data_zillow$neighborhood[which(clean.data_zillow$neighborhood %in% "Mt  Baker")] <- "Mount Baker"

## Moving to the final section -- Merging 911 and Zillow Datasets inner join

neighborhood.911_data$Neighborhood <- neighborhood.911_data$S_HOOD

## Creating new dataframe with aggregated values for each crime type

crime.values <- data.frame(table(neighborhood.911_data$Year, neighborhood.911_data$Month,neighborhood.911_data$Neighborhood,neighborhood.911_data$Event.Clearance.Group))
colnames(crime.values) <- c("Year","Month","Neighborhood","Type","Frequency")

crime.values$Burglary <- 0
crime.values$Arrests <- 0
crime.values$Assaults <- 0
crime.values$Disturbances <- 0
crime.values$Robbery <- 0

crime.values$Burglary[which(crime.values$Type %in% "BURGLARY")] <- crime.values$Frequency[which(crime.values$Type %in% "BURGLARY")]
crime.values$Arrests[which(crime.values$Type %in% "ARREST")] <- crime.values$Frequency[which(crime.values$Type %in% "ARREST")]
crime.values$Assaults[which(crime.values$Type %in% "ASSAULTS")] <- crime.values$Frequency[which(crime.values$Type %in% "ASSAULTS")]
crime.values$Disturbances[which(crime.values$Type %in% "DISTURBANCES")] <- crime.values$Frequency[which(crime.values$Type %in% "DISTURBANCES")]
crime.values$Robbery[which(crime.values$Type %in% "ROBBERY")] <- crime.values$Frequency[which(crime.values$Type %in% "ROBBERY")]

join_cols_reqd <- c("Year","Month","Neighborhood","Burglary","Assaults","Disturbances","Robbery","Arrests")

crime.values <- subset(crime.values)[join_cols_reqd]

# write.csv(neighborhood.911_data,"911 Neighborhoods data.csv")
# write.csv(clean.data_zillow,"Zillow clean data.csv")
# write.csv(crime.values,"crime values.csv")

crime.values$Year <-  as.character(crime.values$Year)
crime.values$Month <- as.character(crime.values$Month)
crime.values$Neighborhood <- as.character(crime.values$Neighborhood)
crime.values$Burglary <- as.numeric(crime.values$Burglary)
crime.values$Assaults <- as.numeric(crime.values$Assaults)
crime.values$Disturbances <- as.numeric(crime.values$Disturbances)
crime.values$Robbery <- as.numeric(crime.values$Robbery)
crime.values$Arrests <-  as.numeric(crime.values$Arrests)

# Grouping data by neighborhood, year and month

Assaults_data <- as.data.frame(xtabs(crime.values$Assaults ~ crime.values$Year + crime.values$Month + crime.values$Neighborhood))
Burglaries_data <- as.data.frame(xtabs(crime.values$Burglary ~ crime.values$Year + crime.values$Month + crime.values$Neighborhood))
Arrests_data <- as.data.frame(xtabs(crime.values$Arrests ~ crime.values$Year + crime.values$Month + crime.values$Neighborhood))
Disturbances_data <- as.data.frame(xtabs(crime.values$Disturbances ~ crime.values$Year + crime.values$Month + crime.values$Neighborhood))
Robbery_data <- as.data.frame(xtabs(crime.values$Robbery ~ crime.values$Year + crime.values$Month + crime.values$Neighborhood))

colnames(Assaults_data) <- c("Year","Month","Neighborhood","Assault.Count")
colnames(Burglaries_data) <- c("Year","Month","Neighborhood","Burglary.Count")
colnames(Arrests_data) <- c("Year","Month","Neighborhood","Arrest.Count")
colnames(Disturbances_data) <- c("Year","Month","Neighborhood","Disturbance.Count")
colnames(Robbery_data) <- c("Year","Month","Neighborhood","Robbery.Count")

aggregate.911.neighborhood <- as.data.frame(cbind(as.character(Assaults_data$Year), as.character(Assaults_data$Month), as.character(Assaults_data$Neighborhood), as.numeric(Assaults_data$Assault.Count), as.numeric(Burglaries_data$Burglary.Count), as.numeric(Arrests_data$Arrest.Count), as.numeric(Disturbances_data$Disturbance.Count), as.numeric(Robbery_data$Robbery.Count)))
colnames(aggregate.911.neighborhood) <- c("Year","Month","Neighborhood","Assault.Count","Burglary.Count","Arrest.Count","Disturbance.Count","Robbery.Count")

write.csv(aggregate.911.neighborhood, "Aggregate 911 data.csv")

zillow.911.merged <- as.data.frame(merge(x=aggregate.911.neighborhood,y=clean.data_zillow,by=c("Neighborhood","Year","Month")))

## Time for action - Multiple Regression between home value and crime data

# Plotting Data
x <- c("home.value","Arrest.Count","Burglary.Count","Robbery.Count")
plot.merged.data <- subset(zillow.911.merged,)[x]
plot.merged.data <- plot.merged.data[order(plot.merged.data$Arrest.Count, plot.merged.data$Burglary.Count, plot.merged.data$Robbery.Count),] 

plot.merged.data$Arrest.Count <- as.numeric(plot.merged.data$Arrest.Count)
plot.merged.data$Burglary.Count <- as.numeric(plot.merged.data$Burglary.Count)
plot.merged.data$Robbery.Count <- as.numeric(plot.merged.data$Robbery.Count)

plot(plot.merged.data)

# Multiple regression model with all 5 parameters: Assaults, arrests, burglaries, disturbances, robberies
multiple.lm <- lm(home.value ~ as.numeric(Assault.Count) + as.numeric(Arrest.Count) + as.numeric(Burglary.Count) + as.numeric(Disturbance.Count) + as.numeric(Robbery.Count), data = zillow.911.merged)
summary(multiple.lm)

# Multiple regression model without disturbances
multiple.lm.2 <- lm(home.value ~ as.numeric(Assault.Count) + as.numeric(Arrest.Count) + as.numeric(Burglary.Count) + as.numeric(Robbery.Count), data = zillow.911.merged)
summary(multiple.lm.2)

# Linear regression with crime = disturbances
dist.lm <- lm(home.value ~ as.numeric(Disturbance.Count), data = zillow.911.merged)
summary(dist.lm)

# Linear regression with Assaults
assault.lm <- lm(home.value ~ as.numeric(Assault.Count), data = zillow.911.merged)
summary(assault.lm)

# Multiple regression without burglaries
multiple.lm.3 <- lm(home.value ~ as.numeric(Assault.Count) + as.numeric(Arrest.Count) + as.numeric(Disturbance.Count) + as.numeric(Robbery.Count), data = zillow.911.merged)
summary(multiple.lm.3)

# Multiple regression model without the two confounding variables disturbances and assaults
multiple.lm.4 <- lm(home.value ~  as.numeric(Arrest.Count) + as.numeric(Burglary.Count) + as.numeric(Robbery.Count), data = zillow.911.merged)
summary(multiple.lm.4)

# Main Plot

ggplot(data = plot.merged.data, aes(x = crime, y = home.value))  +
  geom_point(aes(x = Arrest.Count, y = home.value), stat = "identity", colour = 'red',alpha = "0.5", size = 3.5) +
  geom_point(aes(x = Burglary.Count, y = home.value), stat = "identity", colour = 'blue',alpha = "0.5", size = 3.5) +
  geom_point(aes(x = Robbery.Count, y = home.value), stat = "identity", colour = 'green',alpha = "0.5", size = 3.5) 
