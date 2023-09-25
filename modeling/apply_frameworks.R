## This script includes the information regarding the code we used to calculate
## the excess mortality estimates in this research note.

library(tidyverse)
library(lubridate)

# 1. Msemburi et. al ---------------------------------------------------
## We utilized the spline_model_monthly.R script included in the group's source 
##  code linked here: https://github.com/WHOexcessc19/Codebase/tree/main

library(mcgv)

## Before altering the code provided, we have to prep our data.
# Read in the dataset
exp_obs_bymonthyear <- read.csv("~/cdc-weekly_counts.csv", header = TRUE, sep = ',')

# Add in necessary columns and change current column names
colnames(exp_obs_bymonthyear)[1] <- "iso3"
colnames(exp_obs_bymonthyear)[5] <- "observed"
exp_obs_bymonthyear$month <- format(as.Date(exp_obs_bymonthyear$week_end, format="%Y-%m-%d"),"%m")
exp_obs_bymonthyear$year <- format(as.Date(exp_obs_bymonthyear$week_end, format="%Y-%m-%d"),"%Y")
exp_obs_bymonthyear$month <- as.numeric(exp_obs_bymonthyear$month)

## Run the code included in the R script starting from line 16 
##    and change the lines listed below:
# Change lines 28-42 to the following:
acm_predictions <- data.frame(iso3 = rep(countries, each = 24),
                              year = rep(c(rep(2020, 12), 
                                           rep(2021, 12),
                                           rep(2022, 12),
                                           rep(2023, 12)), 
                                         times = length(countries)),
                              month = rep(c(1:12, 1:12, 1:12, 1:12), 
                                          times = length(countries)),
                              expected_acm = NA,
                              expected_acm_se = NA,
                              expected_log_acm = NA,
                              expected_log_acm_se = NA,
                              gamma_E = NA,
                              gamma_delta = NA,
                              gamma_E_nb = NA,
                              gamma_delta_nb = NA) %>%
  mutate(country_num = as.numeric(as.factor(iso3)))

# Change lines 74-79 to the following:
pred <- predict(annual_model,
                se.fit = TRUE,
                type = "response",
                newdata = data.frame(year = c(rep(2020, 12),
                                              rep(2021, 12),
                                              rep(2022, 12),
                                              rep(2023, 12)),
                                     month = c(1:12, 1:12, 1:12, 1:12)))

# Change lines 83-87 to the following:
pred_log <- predict(annual_model,
                    se.fit = TRUE,
                    newdata = data.frame(year = c(rep(2020, 12),
                                                  rep(2021, 12),
                                                  rep(2022, 12),
                                                  rep(2023, 12)),
                                         month = c(1:12, 1:12, 1:12, 1:12)))

## Once finished running the script, we extract expected counts.
expected_estimates <- data.frame(date = make_date(year = acm_predictions$year, 
                                                  month = acm_predictions$month),
                                 who_expected = acm_predictions$expected_acm,
                                 who_expected_lower95 = 
                                   acm_predictions$expected_acm-2*acm_predictions$expected_acm_se,
                                 who_expected_upper95 =
                                   acm_predictions$expected_acm+2*acm_predictions$expected_acm_se
)

## Subset to gain expected counts for January 2022-June 2023.
expected_estimates <- subset(expected_estimates, date >= "2022-01-01")
expected_estimates <- subset(expected_estimates, date <= "2023-06-01")

## Create a data frame for excess counts. Add results from script to excess_estimates
monthly_observed <- subset(exp_obs_bymonthyear, year > 2019)
monthly_observed <- subset(monthly_observed, year > 2019)
excess_estimates <- aggregate(cbind(observed)~date,
                              data=monthly_observed,FUN=sum)
excess_estimates <- subset(excess_estimates, date >= "2022-01-01")
excess_estimates$who_excess <- excess_estimates$observed-expected_estimates$who_expected

intervals <- data.frame(date = make_date(year = acm_predictions$year, 
                                month = acm_predictions$month))
intervals$se <- acm_predictions$expected_acm_se
intervals <- subset(intervals, date >= "2022-01-01")
intervals <- subset(intervals, date <= "2023-06-01")
excess_estimates$who_excess_lower95 <- excess_estimates$who_excess-2*intervals$se
excess_estimates$who_excess_lower95 <- excess_estimates$who_excess+2*intervals$se


# 2. Acosta & Irizarry --------------------------------------------------
## We utilized the excessmort package linked here: 
##  https://cran.r-project.org/web/packages/excessmort/index.html

## We then followed the vignette linked here: 
##  https://cran.r-project.org/web/packages/excessmort/vignettes/excessmort.html

## We used the following code. Any code that was altered or added will be 
##  indicated with a comment followed with an asterisk.

# Libraries needed
library(knitr)
library(dplyr)
library(excessmort)

# Read in dataset*
data <- read.csv("~/cdc-weekly_counts.csv", header = TRUE, sep = ',')

# Change column names and add in necessary columns*
colnames(data)[4] <- "date"
colnames(data)[5] <- "outcome"
data$population <- 338289856  # population number comes from The Economist's 
                              #   dataset linked here: https://github.com/TheEconomist/covid-19-excess-deaths-tracker/blob/master/output-data/historical-deaths/united_states_weekly_deaths.csv

# Format date column correctly*
data$month <- format(as.Date(data$date, format="%Y-%m-%d"),"%m")
data$year <- format(as.Date(data$date, format="%Y-%m-%d"),"%Y")
data$day <- format(as.Date(data$date, format="%Y-%m-%d"),"%d")
data$month <- as.numeric(data$month)
data$date <- make_date(year = data$year, month = data$month, day = data$day)

# Dates to exclude when fitting the mean model
exclude_dates <- c(seq(make_date(2020, 1, 1), make_date(2023, 6, 1), by = "day"))

# Fitting mean model to data
counts <- data %>% 
  compute_expected(exclude = exclude_dates)

# Calculate confidence intervals and add results to expected_estimates.csv*
geo_mean <- exp(mean(log(counts$expected)))
counts$expected_se <- geo_mean*counts$log_expected_se

counts_monthly <- counts
counts_monthly$date <- make_date(year = counts_monthly$year, month = counts_monthly$month)
counts_monthly <- aggregate(cbind(outcome, expected, expected_se)~date,
                data=counts_monthly,FUN=sum)
counts_monthly <- subset(counts_monthly, date >= "2022-01-01")

expected_estimates$observed <- counts_monthly$outcome
expected_estimates$harvard_expected <- counts_monthly$expected
expected_estimates$harvard_expected_lower95 <- counts_monthly$expected-2*counts_monthly$expected_se
expected_estimates$harvard_expected_upper95 <- counts_monthly$expected+2*counts_monthly$expected_se

## Add results from script to excess_estimates.csv
excess_estimates$harvard_excess <- excess_estimates$observed-counts_monthly$expected
excess_estimates$harvard_excess_lower95 <- excess_estimates$harvard_excess-2*counts_monthly$expected_se
excess_estimates$harvard_excess_upper95 <- excess_estimates$harvard_excess+2*counts_monthly$expected_se

# 3. The Economist --------------------------------------------------------
## We utilized the R script excess_deaths_script.R from the following 
##  GitHub repository: https://github.com/TheEconomist/covid-19-excess-deaths-tracker

## Before running the scripts, we need to prep our data
data <- read.csv("~/cdc-weekly_counts.csv", header = TRUE, sep = ',')
data$country <- "United States"
data$region <- "United States"
data$region_code <- 0
data$days <- 7
data$population <- 338289856  # population number comes from The Economist's 
                              #   dataset linked here: https://github.com/TheEconomist/covid-19-excess-deaths-tracker/blob/master/output-data/historical-deaths/united_states_weekly_deaths.csv
data$expected_deaths <- "TBC"
colnames(data)[4] <- "end_date"
colnames(data)[5] <- "total_deaths"
data$start_date <- c(seq(make_date(2015, 1, 4), make_date(2023, 6, 18), by = "week"))
data$week <- c(2:52,1:53,1:52,1:52,1:52,1:52,1:52,1:53,1:25)

# extract covid deaths from CDC counts
dt_covid <- read.csv("~/Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv", 
                     header=TRUE, sep=',')
dt_covid <- subset(dt_covid, Group=="By Week")
dt_covid <- subset(dt_covid, State == "United States")
dt_covid$End.Date <- format(as.Date(dt_covid$End.Date, format="%m/%d/%Y"),"%Y-%m-%d")
dt_covid <- subset(dt_covid, End.Date <= "2023-06-24")
data$covid_deaths <- c(rep(0,260),dt_covid$COVID.19.Deaths)

data <- data[c(8:10,14,4,11,2:3,12,5,15,13)]

## Run excess_deaths_script.R

## Add results from script to expected_estimates.csv
expected_estimates$economist_expected <- "TBC"         # calculate from R script
expected_estimates$economist_expected_lower95 <- "TBC" # calculate from R script
expected_estimates$economist_expected_upper95 <- "TBC" # calculate from R script

## Add results from script to excess_estimates.csv
excess_estimates$economist_excess <- "TBC"         # calculate from R script
excess_estimates$economist_excess_lower95 <- "TBC" # calculate from R script
excess_estimates$economist_excess_upper95 <- "TBC" # calculate from R script

# 4. IHME -----------------------------------------------------------------
## We utilized the R scripts from 01_data_prep, 02_data_processing, and 
##  03_ensemble_excess_model from the following GitHub repository: 
##  https://github.com/ihmeuw-demographics/publication_covid_em

## We applied the prepared dataset in the 01_data_prep and 02_data_processing.

## After running the scripts, extract expected counts and add them to 
##  expected_estimates.csv
expected_estimates$ihme_expected <- "TBC"           # add in results from script
expected_estimates$ihme_expected_lower95 <- "TBC"   # add in results from script
expected_estimates$ihme_expected_upper95 <- "TBC"   # add in results from script

## After running the scripts, compute excess counts and append them to 
##  excess_estimates.csv
excess_estimates$ihme_excess <- "TBC"           # add in results from script
excess_estimates$ihme_excess_lower95 <- "TBC"   # add in results from script
excess_estimates$ihme_excess_upper95 <- "TBC"   # add in results from script

## Save expected estimates
write.csv(expected_estimates, "~/expected_estimates.csv", row.names = FALSE)

## Save excess estimates
write.csv(excess_estimates, "~/excess_estimates.csv", row.names = FALSE)
