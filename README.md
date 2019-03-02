rm(list=ls())

setwd("C:/Users/remed/Google Drive/Columbia/Spring 2019/5205-004 Frameworks and Methods II/Project")
dir()

data_raw <- read.csv("winemag-data-130k-v2.csv", stringsAsFactors = F)
str(data_raw)

##### Exploratory Data Analysis
head(data)
as.data.frame(table(data$price))
summary(data$price)
sum(is.na(data$price))
as.data.frame(table(data$country)) 
as.data.frame(table(data$designation)) ##weird names
as.data.frame(table(data$province))
as.data.frame(table(data$region_1))
as.data.frame(table(data$region_2))
as.data.frame(table(data$taster_name))
as.data.frame(table(data$taster_twitter_handle))
as.data.frame(table(data$variety))
as.data.frame(table(data$winery))
#check
sum(is.na(data)) 

data <- data_raw[!is.na(data_raw$price),]   ### Remove all records with missing price values
data <- data[!data$country==" ",]           ### Remove all records with missing country values
data <- data[,2:ncol(data)]                 ### Remove counter column
data <- data[!data$variety=="",]            ### Remove 1 record that has no variety information
                                            ####double check to make sure no twitter, no taster_name

table(data$taster_name)
data$taster_name <- as.factor(ifelse(data$taster_name=="","Other",data$taster_name))
#Putting "Other" in the No Name label for taster_name
str(data)

table(data$region_1)
table(data$region_2)
data$region_collapse <- ifelse(data$region_2=="",data$region_1,data$region_2)
data$region_collapse <- ifelse(data$region_collapse=="","No Information",data$region_collapse)
table(data$region_collapse=="No Information")

data$has_twitter <- ifelse(data$taster_twitter_handle=="",0,1)  ### Variable to capture whether taster has a twitter handle

# This extracts the year from the title without necessitating further cleaning outside of NA's in cases where no year is present
library(stringr)
data$year <- as.numeric(stri_extract_first_regex(data$title, paste(c(1900:2019), collapse ="|")))
table(data$year)
sum(is.na(data$year))
### It seems that as wines get more expensive, expectations of quality increase disproportionaly,
### as demonstrated by the in reduction in quality
### This suggests that "points" is more of a subjective value judgement based on both quality and price

data_pricebelow200 = data[data$price<200,] #still 96.7% of dataset (if want 99.6% <250)
summary(data$points)

library(ggplot2)
ggplot(data, aes(price, points))+
  geom_point()+
  geom_smooth()

ggplot(data_pricebelow1000, aes(price, points))+
  geom_point()

ggplot(data_pricebelow1000, aes(price))+
  geom_histogram(aes(y = stat(count)))

ggplot(data_pricebelow1000, aes(points))+
  geom_histogram(aes(y = stat(count)))

ggplot(data_pricebelow200, aes(price))+
  geom_histogram(aes(y = stat(count)))

ggplot(data1, aes(price, points))+
  geom_point()+
  geom_smooth()

#duplicated row
library(dplyr)
duplicated = data[duplicated(data),]
head(duplicated)
dim(duplicated)
data1=data_pricebelow200[!duplicated(data), ] #92% of original dataset 

write.csv(data1, file = "wine_clean.csv")
