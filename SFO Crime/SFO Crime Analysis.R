library(chron)
library(ggplot2)
library(dplyr)

## 1 - Import the data : San Francisco Incidents derived from SFPD (San Francisco Police Department) Crime Incident Reporting system for calendar year 2018 -----

Police_Department_Incident_Reports_2018_to_Present <- read_csv("Data/Police_Department_Incident_Reports__2018_to_Present.csv")
View(Police_Department_Incident_Reports_2018_to_Present)

## Save the data into another file
save(Police_Department_Incident_Reports_2018_to_Present, file = "SFO incident reports.rdata")

## Load it into memory 
load("SFO incident reports.rdata")

## 2- Explore the Data ----

## Find the number of rows : 290 384 rows
nrow(Police_Department_Incident_Reports_2018_to_Present)

## Find the number of columns : 35 variables
ncol(Police_Department_Incident_Reports_2018_to_Present)

## Structure of the dataset
str(Police_Department_Incident_Reports_2018_to_Present)

## Summary of the dataset

summary(Police_Department_Incident_Reports_2018_to_Present)

## 3 - Data Manipulation ----

## 3-1 Order the data by number of incident to find duplicates ----

Police_Department_Incident_Reports_2018_to_Present <- Police_Department_Incident_Reports_2018_to_Present[order(Police_Department_Incident_Reports_2018_to_Present$`Incident Number`),]

SFO_incident_num <- Police_Department_Incident_Reports_2018_to_Present[duplicated(Police_Department_Incident_Reports_2018_to_Present$`Incident Number`),]

## Let's removethe duplicates by number of incident

SFO_incident_report <- subset(Police_Department_Incident_Reports_2018_to_Present, !duplicated(Police_Department_Incident_Reports_2018_to_Present$`Incident Number`))

nrow(SFO_incident_report) # 210 139

## 3-2 Combine Incident Date and incident time ----

SFO_incident_report$Datetime <- paste(SFO_incident_report$`Incident Date`, paste(SFO_incident_report$`Incident Time`))

# Create a new column Incident Date by converting Datetime using POSIXIt function

SFO_incident_report$Incid_Date <- as.POSIXct(SFO_incident_report$`Incident Datetime`, format = "%Y/%m/%d %H:%M:%S")

#Split time from the Incident_Date into Incident_Time to perform analysis on the data by time of the day

SFO_incident_report$Time <- times(format(SFO_incident_report$Incid_Date , "%H:%M:%S"))

#Split Date from the Incident_Date field to perform analysis on day of the month
# strptime :: to ocnvert from and to character

SFO_incident_report$Incid_Date <- as.POSIXlt(strptime(SFO_incident_report$Incid_Date, format="%Y-%m-%d"))

## 3-3 Drop columns that are not needed for this analysis to keep it clean. The chosen columns are stored in a Vector ----

drops <- c("Time", "Incid_Date", "point", "Police District", "SF Find Neighborhoods", "Current Supervisor Districts", "OWED Public Spaces", "Intersection", "CNN", "HSOC Zones as of 2018-06-05", "Parks Alliance CPSI (27+TL sites)", "ESNCAG - Boundary File", "CAD Number", "Current Police Districts", "Central Market/Tenderloin Boundary Polygon - Updated", "Analysis Neighborhoods", "Analysis Neighborhood", "Supervisor District")

SFO_incident_report <- SFO_incident_report[, !(names(SFO_incident_report) %in% drops)]
head(SFO_incident_report)


#The frequency of crimes need not be consistent throughout the day as certain crimes happen more in the night than the rest of the #hour. 
##To check this, we can bucket the timestamps into few categories and then analyze the distribution of the crimes across different #time intervals. 
#Let’s create 4 buckets: Early Morning, Morning, Evening, and Night by grouping certain hours together.

t <- as.data.frame(table(as.character(SFO_incident_report$`Incident Time`)))

time.tag <- chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00"))

## 3-4 Create a new column Incident_Time_Tag and use Chron’s cut function to put different hours into the time breaks ----

SFO_incident_report$Incident_Time_Tag <- cut(as.times(SFO_incident_report$`Incident Time`) , breaks=time.tag, labels=c("Early Morning","Morning", "Evening", "Night"), include.lowest = TRUE)


## 3-5- Identify which month and day of the year a particular crime incident has happened ----

SFO_incident_report$Incident_Month <- months(SFO_incident_report$`Incident Date`, abbreviate = TRUE)
SFO_incident_report$Incident_day <-  weekdays(SFO_incident_report$`Incident Date`, abbreviate = FALSE)
SFO_incident_report$Incident_quarter <-  quarters(SFO_incident_report$`Incident Date`, abbreviate = FALSE)


## Explore different category of crime 

table(SFO_incident_report$`Incident Category`)
length(unique(SFO_incident_report$`Incident Category`))


# There are 51 category of crimes, let's try to gather some under one crime

## Create a new column 

SFO_incident_report$Crime_Category <- as.character(SFO_incident_report$`Incident Category`)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Human Trafficking (A), Commercial Sex Acts", "Human Trafficking (B), Involuntary Servitude", "Human Trafficking, Commercial Sex Acts", "Sex Offense", "Prostitution", "Rape"), 'SEX CRIMES' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Drug Offense", "Drug Violation", "Liquor Laws"), 'DRUGS' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Burglary", "Lost Property", "Motor Vehicle Theft", "Motor Vehicle Theft?", "Larceny Theft", "Embezzlement", "Stolen Property", "Vehicle Misplaced", "Vehicle Impounded", "Robbery", "Recovered Vehicle"), 'THEFT & ROBERRY' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Forgery And Counterfeiting", "Fraud", "Gambling"), 'FRAUD' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Vandalism","Arson" ), 'ARSON' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Assault", "Homicide", "Fire Report", "Weapons Carrying Etc", "Weapons Offence", "Weapons Offense" , "Assault & weapons"), 'ASSAULT & WEAPONS' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Case closure", "Civil Sidewalks", "Courtesy Report", "Disorderly Conduct", "Traffic Collision" , "Traffic Violation Arrest", "Malicious Mischief", "Miscellaneous Investigation", "Other Miscellaneous", "Suspicious", "Suspicious Occ", "Other Offenses", "Other", "Warrant"), 'OTHER OFFENSES' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Family Offense", "Offences Against The Family And Children"), 'FAMILY OFFENSES' , SFO_incident_report$Crime_Category)

SFO_incident_report$Crime_Category <- ifelse(SFO_incident_report$Crime_Category %in% c("Suicide", "Non-Criminal", "Case Closure"), 'NON CRIMINAL' , SFO_incident_report$Crime_Category)

table(SFO_incident_report$Crime_Category) #now we have 10 Major Crime categories instead of 51





## 4- Data Visualisation -----

## 4-1- Crimes by category ----
# Theft is the most crime to be reported in the year 2018 et 2019

qplot(SFO_incident_report$Crime_Category, xlab = "Category of crimes",ylab = "Number of crimes reported",  main = "Crimes reported in San Francisco")

length(which(is.na(SFO_incident_report$Crime_Category)))

## 4-2- Crimes by time of the Day ----
# Crimes are more likely to happen in the evening and in the night than in the morning

qplot(SFO_incident_report$Incident_Time_Tag, xlab = "Time of the Day", ylab = "Number of crimes reported", main = "Crimes reported in SFO")

## 4-3- Crimes by the day of the week ----
# Crimes are more likely to happen Friday  

qplot(SFO_incident_report$Incident_day, xlab = "Day of the week", ylab = "Number of crimes reported", main = "Crimes reported in SFO")

## 4-4- Crimes by the month ----
# Crimes are more likely to happen at august and october  

qplot(SFO_incident_report$Incident_Month, xlab = "Month", ylab = "Number of crimes reported", main = "Crimes reported in SFO")
qplot(SFO_incident_report$Incident_quarter, xlab = "quarter of the year", ylab = "Number of crimes reported", main = "Crimes reported in SFO")


## Heat Map of Crime Incidents by Time of the Day ----

#Aggregate the Crime Category by different time buckets

incidents_by_time <- aggregate(SFO_incident_report$Crime_Category, by = list(SFO_incident_report$Crime_Category, SFO_incident_report$Incident_Time_Tag), FUN = length)

#Name the columns of the new data frame
names(incidents_by_time) <- c("Crime_Category", "Incident_Time_Tag", "Count")

ggplot(incidents_by_time, aes(x= Crime_Category, y= factor(Incident_Time_Tag))) +
  geom_tile(aes(fill= Count)) + scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Time of day", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
  theme_bw() + ggtitle("Crimes by time of day") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
        (colour = NA))

## Heat Map of Crime Incidents by Day of the Week ----

incidents_by_day <- aggregate(SFO_incident_report$Crime_Category, by = list(SFO_incident_report$Crime_Category, SFO_incident_report$Incident_day), FUN = length)

names(incidents_by_day) <- c("Crime_Category", "Incident_Day", "Count")

ggplot(incidents_by_day, aes(x = Crime_Category, y = factor(Incident_Day))) +
  geom_tile(aes(fill = Count)) + scale_x_discrete("Category of Crime", expand = c(0,0)) +
  scale_y_discrete("Day of the Week", expand = c(0, -4)) +
  scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
  theme_bw() + ggtitle("Crimes reported by day of the week") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))


## Heat Map of Crime Incidents by the month ----

incidents_by_month <- aggregate(SFO_incident_report$Crime_Category, by = list(SFO_incident_report$Crime_Category, SFO_incident_report$Incident_Month), FUN = length)

names(incidents_by_month) <- c("Crime_Category", "Incident_month", "Count")

ggplot(incidents_by_month, aes(x = Crime_Category, y = factor(Incident_month))) +
  geom_tile(aes(fill = Count)) + xlab("Category of Crime") +
  ylab("Month")+
  scale_fill_gradient("Number of crimes", low = "green", high = "red") +
  theme_bw() + ggtitle("Crimes reported by month")

## Heat Map of Crime Incidents by quarter ----

incidents_by_quarter <- aggregate(SFO_incident_report$Crime_Category, by = list(SFO_incident_report$Crime_Category, SFO_incident_report$Incident_quarter), FUN = length)

names(incidents_by_quarter) <- c("Crime_Category", "Incident_quarter", "Count")


ggplot(incidents_by_quarter, aes(x = Crime_Category, y = factor(Incident_quarter))) +
  geom_tile(aes(fill = Count)) + xlab("Category of Crime") +
  ylab("quarter")+
  scale_fill_gradient("Number of crimes", low = "green", high = "red") +
  theme_bw() + ggtitle("Crimes reported by quarter")