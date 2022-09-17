# Assignment 1 


# SETUP ------------------------------------------------------------------------
setwd('C:/Users/steef/Documents/NHH/FIE401 Financial Econometrics/Fin-Econ-Assignment-1')
load('CAR_M&A.RData')

# Libraries
library(ggplot2)
# library(RColorBrewer)
library(tidyverse)
library(stargazer)

# Functions, colors and themes
mytheme <- 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# STEP 1 -----------------------------------------------------------------------
# Familiarize yourself with the data

# Select the variables that we need



summary(CAR_MA)
stargazer(CAR_MA, type = 'latex', header = FALSE)

# Variables public and private contain the same information
# Date variables are stored as numbers; factor variables are stored as numeric
CAR_MA$yyyymmdd <- as.Date(as.character(CAR_MA$yyyymmdd), format = '%Y%m%d')

# Are there outliers? Evaluate if you should winsorize the data.
ggplot(CAR_MA, aes(deal_value)) +
  geom_histogram(bins = 30) +
  mytheme # MAKE GRADIENT ****************************************************************************************

ggplot(CAR_MA, aes(y = deal_value)) +
  geom_boxplot()



# Are there some mistakes in the data? Evaluate to make adjustments to the data.


# STEP 2 -----------------------------------------------------------------------
# For each year in the sample, report the average deal size, bidder CAR, share 
# of deals with private targets, and share of deals fully paid in stock.
CAR_MA %>%
  group_by(yyyy) %>% # group by year
  summarise(Avg_deal_size = mean(deal_value),
            Acg_bidder_CAR = mean(carbidder),
            Share_of_deals_with_private = mean(private), 
            Share_of_deals_fully_stock = mean(all_stock))

# STEP 3 -----------------------------------------------------------------------
# By methods of payment (fully paid by stock or not), report the average of 
# all variables you include in your regression model. Report the difference 
# in means for both sub-samples and the associated p-value 
# (t-test; text book page 119-120).

CAR_MA %>%
  group_by(all_stock) %>%
  summarise(Avg_CAR_bidder = mean(carbidder),
            Share_of_deals_with_private = mean(private))

# STEP 4 -----------------------------------------------------------------------
#       (Regression table)



# Binary variables are stored as numerical => change to factor
factor_var <- c('public', 'private', 'tender_offer', 'all_stock', 'hostile',
                'horz') 
CAR_MA[factor_var] <- lapply(CAR_MA[factor_var], as.factor)