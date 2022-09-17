# Assignment 1 


# SETUP ------------------------------------------------------------------------
setwd('C:/Users/steef/Documents/NHH/FIE401 Financial Econometrics/Assignment 1')
load('CAR_M&A.RData')

# Libraries
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

# Functions, colors and themes
mytheme <- 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# STEP 1 -----------------------------------------------------------------------
# Familiarize yourself with the data
summary(CAR_MA)

# Variables public and private contain the same information => delete one ***********************************


# Binary variables are stored as numerical => change to factor
factor_var <- c('public', 'private', 'tender_offer', 'all_stock', 'hostile',
                'horz') 
CAR_MA[factor_var] <- lapply(CAR_MA[factor_var], as.factor)

# Are there outliers? Evaluate if you should winsorize the data.
ggplot(CAR_MA, aes(deal_value)) +
  geom_histogram(bins = 30, fill = 'yellow') +
  mytheme # MAKE GRADIENT ****************************************************************************************
ggplot(CAR_MA, aes(y = deal_value)) +
  geom_boxplot()

# Are there some mistakes in the data? Evaluate to make adjustments to the data.


# STEP 2 -----------------------------------------------------------------------
# For each year in the sample, report the average deal size, bidder CAR, share 
# of deals with private targets, and share of deals fully paid in stock.
CAR_MA %>%
  group_by(yyyy) %>%
  summarise(Average_deal_size = mean(deal_value),
            Bidder_CAR = mean(carbidder),
            Share_of_deals_with_private = mean(as.numeric(
              levels(private)[private])), # Transform back to numeric
            Share_of_deals_fully_stock = mean(as.numeric(
              levels(all_stock)[all_stock]))) # Transform back to numeric

# STEP 3 -----------------------------------------------------------------------


