# Program to calculate average interest rate spread for US census tracts in 2019 
# Alexa Freedman
# May 18, 2023



# Codebook for 2019 one year data:
## https://ffiec.cfpb.gov/documentation/2019/lar-data-fields/

# Data download: 
## https://ffiec.cfpb.gov/data-publication/one-year-national-loan-level-dataset/2019



# Rate spread description: The difference between the covered loan’s 
# annual percentage rate (APR) and the average prime offer rate (APOR) for a 
# comparable transaction as of the date the interest rate is set




# Load packages
library(dplyr)
library(tidyr)


# Read in HMDA data

one <- read.csv("C:/HMDA/2019_public_lar_one_year.csv")


# Filter 2019 one year loan-level data to loans of interest
one_res <- one %>% 
  # Restrict to loans for home purchase
  filter(loan_purpose == 1) %>% 
  # Restrict to loans for single family homes
  filter(derived_dwelling_category %in% c("Single Family (1-4 Units):Site-Built","Single Family (1-4 Units):Manufactured")) %>% 
  # Restrict to loans for principal residence
  filter(occupancy_type == 1) %>% 
  # Restrict to application originated 
  filter(action_taken == 1) %>% 
  # Convert exempt rate spread to missing and convert to numeric
  mutate(rate_spread_use = ifelse(rate_spread == "Exempt", NA, as.numeric(rate_spread))) 


# Quick descriptives
summary(one_res$rate_spread_use)
quantile(one_res$rate_spread_use, probs = c(0.00001, 0.99999), na.rm=TRUE) # looking at extremes


# Group by census tract and summarize average interest rate spread
one_sum <- one_res %>% 
  group_by(census_tract) %>% 
  # Filter NA census tract
  filter(is.na(census_tract)==FALSE) %>% 
  # Set extreme rate spread observations to missing
  ### Extreme based on <0.001%ile or >99.999%ile
  mutate(rate_spread_use = ifelse(rate_spread_use < -6.5, NA,
                                  ifelse(rate_spread_use >24.6, NA, rate_spread_use))) %>% 
  summarise(n_loans = n(),
            # Average interest rate spread
            avg_rate_spread = mean(rate_spread_use, na.rm=TRUE))


summary(one_sum$avg_rate_spread)


## Export file and merge to datasets of interest by census tract 
