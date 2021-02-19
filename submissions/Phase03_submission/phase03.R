#Author: Akash Ghose
#Student ID: 50128850
#Phase: 03; Data cleaning and reprocessing 

#Loading libraries
library(readxl)
library(dplyr)

#First, load all the data into environment 
X2019_FBI_arrests_by_race_total <- read_excel("data/2019_FBI_arrests_by_race_total.xls")
X2019_FBI_arrests_by_race_under18 <- read_excel("data/2019_FBI_arrests_by_race_under18.xls")
X2019_FBI_arrests_by_race_18_and_over <- read_excel("data/2019_FBI_arrests_by_race_18_and_over.xls")
#NFWBS_PUF_2016_data_readable <- read.csv("data/NFWBS_PUF_2016_data_readable.csv")

#Getting rid of unnecessary rows/rows without data from the FBI arrest datasets
intermediate_FBI_arrest_by_race_under18 <- X2019_FBI_arrests_by_race_under18[-c(1:6,39:43),]
intermediate_FBI_arrest_by_race_total <- X2019_FBI_arrests_by_race_total %>% slice(-c(1:6,39:42),)
intermediate_FBI_arrest_by_race_18_and_over <- na.omit(X2019_FBI_arrests_by_race_18_and_over) %>% slice(-c(32),)

#Removing irrelevant columns (features)
intermediate_FBI_arrest_by_race_total <- select(intermediate_FBI_arrest_by_race_total, -c(14:19))
intermediate_FBI_arrest_by_race_under18 <- select(intermediate_FBI_arrest_by_race_under18, -c(14:19))
intermediate_FBI_arrest_by_race_18_and_over <- select(intermediate_FBI_arrest_by_race_18_and_over, -c(14:19))

#Changing column values so I can use them as column names later
indecies <- seq(8,13)
for(i in indecies){
  "For each of the columns 8:13, 
  add a '%' sign in front of the values of the first row"
  val <- intermediate_FBI_arrest_by_race_total[1,i]
  intermediate_FBI_arrest_by_race_total[1,i] = paste("%",val)
  
  val <- intermediate_FBI_arrest_by_race_under18[1,i]
  intermediate_FBI_arrest_by_race_under18[1,i] = paste("%",val)
  
  val <- intermediate_FBI_arrest_by_race_18_and_over[1,i]
  intermediate_FBI_arrest_by_race_18_and_over[1,i] = paste("%",val)
}

#Assigning appropriate column names for ease of readability 
names(intermediate_FBI_arrest_by_race_total) <- intermediate_FBI_arrest_by_race_total[1,]
names(intermediate_FBI_arrest_by_race_under18) <- intermediate_FBI_arrest_by_race_under18[1,]
names(intermediate_FBI_arrest_by_race_18_and_over) <- intermediate_FBI_arrest_by_race_18_and_over[1,]

#Dropping the first rows as they are no longer needed
intermediate_FBI_arrest_by_race_total <- intermediate_FBI_arrest_by_race_total[-c(1),]
intermediate_FBI_arrest_by_race_under18 <- intermediate_FBI_arrest_by_race_under18[-c(1),]
intermediate_FBI_arrest_by_race_18_and_over <- intermediate_FBI_arrest_by_race_18_and_over[-c(1),]

#Changing the data values from character to numeric
intermediate_FBI_arrest_by_race_total[,2:13] <- lapply(2:13, function(x) as.numeric(intermediate_FBI_arrest_by_race_total[[x]]))
intermediate_FBI_arrest_by_race_under18[,2:13] <- lapply(2:13, function(x) as.numeric(intermediate_FBI_arrest_by_race_under18[[x]]))
intermediate_FBI_arrest_by_race_18_and_over[,2:13] <- lapply(2:13, function(x) as.numeric(intermediate_FBI_arrest_by_race_18_and_over[[x]]))

#Changing the first column into factors
intermediate_FBI_arrest_by_race_total[,1] <- lapply(1, function(x) as.factor(intermediate_FBI_arrest_by_race_total[[x]]))
intermediate_FBI_arrest_by_race_under18[,1] <- lapply(1, function(x) as.factor(intermediate_FBI_arrest_by_race_under18[[x]]))
intermediate_FBI_arrest_by_race_18_and_over[,1] <- lapply(1, function(x) as.factor(intermediate_FBI_arrest_by_race_18_and_over[[x]]))