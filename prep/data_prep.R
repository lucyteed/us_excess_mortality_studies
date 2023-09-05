library(tidyverse)
library(lubridate)

## TO DO: download datasets into home directory
## https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr
## https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab

## The work in this research note accessed the second dataset on August 10, 2023.

## read dataset for counts from January 2014-December 2019
dt2014_2019 <- read.csv("~/Weekly-Counts-of-Deaths-by-State-and-Select-Causes.csv", 
                        header=TRUE, sep=',')

## read dataset for counts from January 2020-present
dt2020_present <- read.csv("~/Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv", 
                           header=TRUE, sep=',')

## change column names for consistency
colnames(dt2014_2019)[1] <- "location"
colnames(dt2014_2019)[2] <- "year"
colnames(dt2014_2019)[3] <- "week"
colnames(dt2014_2019)[4] <- "week_end"
colnames(dt2014_2019)[5] <- "total_observed"

colnames(dt2020_present)[3] <- "week_end"
colnames(dt2020_present)[5] <- "year"
colnames(dt2020_present)[7] <- "week"
colnames(dt2020_present)[9] <- "location"
colnames(dt2020_present)[11] <- "total_observed"

## subset to weekly counts
dt2020_present <- subset(dt2020_present, Group=="By Week")

## only keep the necessary columns for all-cause excess mortality estimates
dt2014_2019 <- dt2014_2019[-c(6:30)]
dt2020_present <- dt2020_present[-c(1:2,4,6,8,10,12:17)]

## subset to United States counts
dt2014_2019 <- subset(dt2014_2019, location == "United States")
dt2020_present <- subset(dt2020_present, location == "United States")

## combine two datasets to gain data for January 2014-present
data <- rbind(dt2014_2019, dt2020_present)

## we only used data after 2015 until June 30
data <- subset(data, year >= 2015)
data$year <- format(as.Date(data$week_end, format="%m/%d/%Y"),"%Y")
data$month <- format(as.Date(data$week_end, format="%m/%d/%Y"),"%m")
data$day <- format(as.Date(data$week_end, format="%m/%d/%Y"),"%d")
data$week_end <- make_date(year = data$year, month = data$month, day = data$day)
data <- subset(data, week_end < "2023-07-01")

## save counts
write.csv(data, "~/cdc-weekly_counts.csv", row.names=FALSE)
