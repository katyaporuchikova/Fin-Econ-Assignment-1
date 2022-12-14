## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- results='asis', message = FALSE---------------------------------------------------------------------------------
# SETUP ------------------------------------------------------------------------
setwd('C:/Users/steef/Documents/NHH/FIE401 Financial Econometrics/Fin-Econ-Assignment-1')
load('CAR_M&A.RData')

# Libraries
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(stargazer)
library(corrplot)
library(gridExtra) # for combining graphs
library(lubridate) # for ymd function 
library(knitr)

mytheme <- 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Extract code from Rmd
# knitr::purl(input = 'Assignment1_group_08.Rmd', output = 'Assignment1_group_08.R')


## ----summary-stats, echo=FALSE, results='asis'------------------------------------------------------------------------
# STEP 1 -----------------------------------------------------------------------
#       (Summary statistics & Exploratory analysis)

# Variables public and private contain the same information
# change to date variable
CAR_MA$yyyymmdd <- ymd(CAR_MA$yyyymmdd)

# Binary variables are stored as numerical
binary.vars <- c('public', 'private', 'tender_offer', 'all_stock', 'hostile',
                 'horz')

stargazer(CAR_MA[!names(CAR_MA) %in% c('yyyymmdd','yyyy',binary.vars)], 
          type = 'latex', header = FALSE, 
          title = 'Summary statistics',
          summary.stat = c('min','p25','mean','p75', 'max','sd'))


## ---------------------------------------------------------------------------------------------------------------------
# Are there outliers? Evaluate if you should winsorize the data:
# Deal_value:
plot1 <- 
  CAR_MA %>% 
  ggplot(aes(x = yyyy,y = deal_value))+
  geom_point(color = brewer.pal(11,'PiYG')[10]) +
  geom_point(data = CAR_MA[2558,], color = brewer.pal(11,'PiYG')[2]) +
  mytheme +
  labs(x = 'Year',
       y = 'Deal value')

plot2 <- 
  CAR_MA %>%   
  ggplot(aes(deal_value))+
  geom_histogram(bins = 30,
                 fill = brewer.pal(11,'PiYG')[2]) +
  mytheme +
  labs(x = 'Deal value',
       y = 'Frequency')


## ----fig-dv, fig.width = 9, fig.height = 5,fig.cap="\\label{fig:figs}Scatterplot and histogram of deal value"---------
grid.arrange(plot1, plot2, ncol=2)


## ---------------------------------------------------------------------------------------------------------------------
# bidder_size
plot3 <- 
  CAR_MA %>% 
  ggplot(aes(x = yyyy, y = bidder_size))+
  geom_point(color = brewer.pal(11,'PiYG')[10]) +
  mytheme +
  labs(x = 'Year',
       y = 'Bidder size')

plot4 <- 
  CAR_MA %>%   
  ggplot(aes(x = factor(0), bidder_size))+
  geom_boxplot(alpha = 0.5, color = brewer.pal(11,'PiYG')[2]) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  mytheme +
  labs(x = '',
       y = 'Bidder size')


## ----fig-bs, fig.width = 9, fig.height = 5,fig.cap="\\label{fig:figs}Scatterplot and boxplot of bidder size"----------
grid.arrange(plot3,plot4,ncol=2)


## ---------------------------------------------------------------------------------------------------------------------
plot5 <- 
  CAR_MA %>% 
  ggplot(aes(x=yyyy, y = bidder_mtb))+
  geom_point(color = brewer.pal(11,'PiYG')[10]) +
  mytheme +
  labs(x = 'Year',
       y = 'Bidder market-to-book ratio')

# bidder book to market 
plot6 <- 
  CAR_MA %>%   
  ggplot(aes(x=factor(0),bidder_mtb))+
  geom_boxplot(alpha = 0.5, color = brewer.pal(11,'PiYG')[2]) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  mytheme +
  labs(x = '',
       y = 'Bidder market-to-book ratio')


## ----fig-bm, fig.width = 9, fig.height = 5,fig.cap="\\label{fig:figs}Scatterplot and boxplot of bidder book-to-market ratio"----
grid.arrange(plot5,plot6,ncol=2)


## ----fig-distr, fig.width = 9, fig.height = 5,fig.cap="\\label{fig:figs}Deal size histogram before and after log-transformation"----
# Deal with outliers
CAR_MA <- 
  CAR_MA %>%
  rename(year = yyyy) %>% 
  slice(-(2558)) %>%  # to get rid of extreme outlier
  mutate(deal_value = log(deal_value),
         bidder_size = log(bidder_size))

plot2.0 <- 
  CAR_MA %>%   
  ggplot(aes(deal_value))+
  geom_histogram(bins = 30,
                 fill = brewer.pal(11,'PiYG')[10]) +
  mytheme +
  labs(x = 'Deal value',
       y = 'Frequency')

grid.arrange(plot2, plot2.0, ncol=2)


## ----results='asis'---------------------------------------------------------------------------------------------------
# STEP 2 -----------------------------------------------------------------------
#       (Average values by year)

# descriptive table 1:
summary1 <- 
  CAR_MA %>% 
  arrange(yyyymmdd) %>% 
  group_by(year) %>%
  mutate(share_private = mean(private),
         share_stock = mean(all_stock)) %>% 
  summarise(avg_deal_size = round(mean(deal_value), digits= 3),
            avg_bidCAR = round(mean(carbidder), digits=3),
            avg_share_private = round(mean(share_private), digits=3),
            avg_share_stock = round(mean(share_stock), digits = 3))

stargazer(summary1, summary = FALSE, type = 'latex', header = FALSE,
          title = 'Mean values: time effects')


## ---------------------------------------------------------------------------------------------------------------------
# STEP 3 -----------------------------------------------------------------------
#       (Average values by method of payment & t-tests)

vars.for.model <- names(CAR_MA[!names(CAR_MA) %in% c('yyyymmdd','yyyy', 
                                                     'private', 'all_stock')])

difference.means <- data.frame(matrix(NA,    # Create empty data frame
                                      nrow = 4,
                                      ncol = length(vars.for.model)))

row.names(difference.means) <- c('all.stock', 'not.all.stock', 'differece', 
                                 'p-value, t-test')
names(difference.means) <- vars.for.model

difference.means[1,] <- lapply(CAR_MA[CAR_MA$all_stock == 1,
                                      names(CAR_MA) %in% vars.for.model], mean)

difference.means[2,] <- lapply(CAR_MA[CAR_MA$all_stock == 0,
                                      names(CAR_MA) %in% vars.for.model], mean)

difference.means[3,] <- difference.means[1,] - difference.means[2,]

for (v in vars.for.model){
  pval <- t.test(CAR_MA[CAR_MA$all_stock == 1,v],
                 CAR_MA[CAR_MA$all_stock == 0,v])$p.value
  difference.means[4,v] <- pval
}


## ----results='asis'---------------------------------------------------------------------------------------------------
stargazer(difference.means[,c(1:7)], summary = FALSE, type = 'latex', header = FALSE,
          title = 'Difference in means between all stock and not all stock')

stargazer(difference.means[,c(8:14)], summary = FALSE, type = 'latex', header = FALSE, 
          title = 'Difference in means between all stock and not all stock - continued')


## ----fig-cor, fig.width = 7, fig.height = 7,fig.cap="\\label{fig:figs}Correlation plot"-------------------------------



# STEP 4 -----------------------------------------------------------------------
#       (Regression table)

# Check correlations for potential multicolliniarity
correlations <- cor(CAR_MA[!names(CAR_MA) %in% 'yyyymmdd'])
corrplot(correlations, col = COL2('PiYG'), tl.col = 'black')


## ----results='asis'---------------------------------------------------------------------------------------------------
# run regressions 

# Only public - no controls
model1 <- 
  lm(carbidder ~ all_stock, CAR_MA[CAR_MA$public == 0,]) 
# summary(model1)

# Only private - no controls
model2 <- 
  lm(carbidder ~ all_stock, CAR_MA[CAR_MA$public == 1,]) 
# summary(model2)


# Full sample - no controls
model3 <- 
  lm(carbidder ~ all_stock +
                 public +
                 I(all_stock*public),
                 CAR_MA) 
# summary(model3)


# Only public - controls
model4 <- 
  lm(carbidder ~ all_stock +
       deal_value +
       bidder_size +
       bidder_mtb + 
       run_up_bidder +
       bidder_fcf +
       bidder_lev +
       sigma_bidder + 
       relsize +
       horz +
       tender_offer +
       year + # ********************************************************** Add as FE
       hostile,
     CAR_MA[CAR_MA$public == 0,])
# summary(model4)


# Only private - controls
model5 <- 
  lm(carbidder ~ all_stock +
       deal_value +
       bidder_size +
       bidder_mtb + 
       run_up_bidder +
       bidder_fcf +
       bidder_lev +
       sigma_bidder + 
       relsize +
       horz +
       tender_offer +
       year + # ********************************************************** Add as FE
       hostile,
     CAR_MA[CAR_MA$public == 1,])
# summary(model5)


# Full sample - controls
model6 <-
  lm(carbidder ~ all_stock +
                 public +
                 I(all_stock*public) +
                 deal_value +
                 bidder_size +
                 bidder_mtb +
                 run_up_bidder + 
                 bidder_fcf +
                 bidder_lev +
                 sigma_bidder +
                 relsize +
                 horz + 
                 tender_offer +
                 year + # ********************************************************** Add as FE
                 hostile,
                 CAR_MA)
# summary(model6)


stargazer(model1, model2, model3, model4, model5, model6,
          keep.stat = c('n','adj.rsq'),
          type = 'latex', header = FALSE, digits = 3,
          title = 'Regressions output',
          notes.append = TRUE, notes.align = 'l', font.size = 'small',
          notes = c('Models (1) and (4) contain subsample consisting of only private;', 
          'models (2) and (5) contain subsample consisting of only public.'))

