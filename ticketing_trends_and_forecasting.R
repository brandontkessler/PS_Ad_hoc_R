#####################
# Ticketing Trends  #
#####################

#############################################################################
# SETUP

# Clear all variables in workspace
rm(list=ls())

# USER INPUTS
# choose classics or pops

series <- 'classics'

#############################################################################


# libs
library(tidyverse)
library(lubridate)
library(fpp2)
library(plotly)

# funs
source('functions/data_import.R')
source('functions/ticketing_functions.R')

# today
today <- Sys.Date()

# List of ticketing files to load
if(series == 'classics') {
  data_list <- c('Clx09','Clx10','Clx11','Clx12','Clx13',
                 'Clx14','Clx15','Clx16','Clx17','Clx18','Clx19')
} else if(series == 'pops') {
  data_list <- c('Pops09','Pops10','Pops11','Pops12','Pops13',
                 'Pops14','Pops15','Pops16','Pops17','Pops18','Pops19')
} else {
  stop("Choose either classics or pops only")
}

# load ticketing data
df <- ticketing_data(data_list)

# remove any performance later than today
df <- df %>%
  filter(perf_dt < today)

# parse into singles and subs, paid only
df.singles <- single_conversion_and_return(df)
df.subs <- sub_conversion_and_return(df)

# convert to look at revenue per performance
df.singles <- revenue_per_performance(df.singles)
df.subs <- revenue_per_performance(df.subs)

df.final <- merge(df.singles,df.subs,by='perf_dt') %>%
  rename('total_rev_singles' = 'total_rev.x',
         'total_rev_subs' = 'total_rev.y') %>%
  mutate(total_rev_singles = ifelse(total_rev_singles > 200000, 200000, total_rev_singles))

# Time series converted to monthly by dividing total revenue per month by total performances per month
df.monthly <- df.final %>%
  group_by(month=floor_date(perf_dt,'month')) %>%
  summarise(singles = sum(total_rev_singles/n()),
            subs = sum(total_rev_subs)/n()) %>%
  mutate(month = as.Date(month))

# Create a monthly factor to merge with df.monthly to replace non-existent months with a 0
tt <- as.data.frame(seq(as.Date('2008-9-1'),as.Date('2019-2-1'),'month')) %>%
  rename(month = 1)

df.monthly <- merge(df.monthly, tt, by = 'month', all = TRUE) %>%
  mutate(singles = replace(singles, is.na(singles), 0),
         subs = replace(subs, is.na(subs), 0))

ts.tix <- ts(df.monthly[,c('singles','subs')],
             start = c(2008, 9),
             end = c(2019, 2),
             frequency = 12)

ts.tix.singles <- ts(df.monthly[,c('singles')],
                     start = c(2008, 9),
                     end = c(2019, 2),
                     frequency = 12)

ts.tix.subs <- ts(df.monthly[,c('subs')],
                  start = c(2008, 9),
                  end = c(2019, 2),
                  frequency = 12)

##########################################
# Combining subs/singles into monthly rev
##########################################
df.monthly.combo <- df.monthly %>%
  mutate(revenue = singles + subs) %>%
  select(-singles,
         -subs)

ts.combo <- ts(df.monthly.combo[,c('revenue')],
               start = c(2008, 9),
               end = c(2019, 2),
               frequency = 12)

#############################################################################
# Preliminary Analysis
#############################################################################

#############################################################################
# Combined Revenue Analysis
#############################################################################
# Plot time series
autoplot(ts.combo) + 
  ggtitle('Rev /Series /Month') + 
  ylab('Revenue')

# Data by polynomial smoothing and 95% CI
ggplot(df.monthly.combo, aes(x = month)) + 
  stat_smooth(aes(y = revenue),
              method = 'lm', 
              formula = y ~ poly(x, degree = 3)) + 
  ggtitle('Revenue per Performance (polynomial smoothing)') +
  theme_bw()

##################################################################################
# Preliminary analysis of split data (singles/subs separate)
##################################################################################
# Data by performance
ggplot(df.final, aes(x = perf_dt)) + 
  geom_line(aes(y = total_rev_singles, color = 'singles')) + 
  geom_line(aes(y = total_rev_subs, color = 'subs')) +
  ggtitle('Revenue per Performance') +
  ylab('Revenue') + 
  theme_bw()

# Plot time series
autoplot(ts.tix) + 
  ggtitle('Tix Rev /Series /Month') + 
  ylab('Revenue')

# Data by polynomial smoothing and 95% CI
ggplot(df.monthly, aes(x = month)) + 
  stat_smooth(aes(y = singles, color = 'singles'),
              method = 'lm', 
              formula = y ~ poly(x, degree = 3)) + 
  stat_smooth(aes(y = subs, color = 'subs'),
              method = 'lm', 
              formula = y ~ poly(x, degree = 3)) +
  ggtitle('Revenue per Performance (polynomial smoothing)') +
  theme_bw()

# Check change in revenue per month
# This will remove the trend
ts.tix.difference <- diff(ts.tix)

# Plot time series of difference data
autoplot(ts.tix.difference) + 
  ggtitle('Change in Tix Rev /Series /Month') + 
  ylab('Revenue')

# Appears to be stationary, check for seasonality
ggseasonplot(ts.tix.difference[,'subs']) + 
  ggtitle('Seasonal: Subs: Change in Tix Rev /Series /Month') + 
  ylab('Revenue')

ggseasonplot(ts.tix.difference[,'singles']) + 
  ggtitle('Seasonal: Singles: Change in Tix Rev /Series /Month') + 
  ylab('Revenue')

# Decompose time series data
autoplot(stl(ts.tix.singles, s.window = 'periodic')) +
  ggtitle('Decomposed Time Series - Singles')

autoplot(stl(ts.tix.subs, s.window = 'periodic')) + 
  ggtitle('Decomposed Time Series - Subs')

# Check for lags
gglagplot(ts.tix.singles, lags = 4, do.lines = FALSE) + 
  ggtitle('Lag Plot - Singles')

gglagplot(ts.tix.subs, lags = 4, do.lines = FALSE) + 
  ggtitle('Lag Plot - Subs')

# A lag plot is used to help evaluate whether the 
#   values in a dataset or time series are random. 
# If the data is random, the lag plot will exhibit 
#   no identifiable pattern.

####################################################
# Data appears to have seasonality, but may be due to programming
# 
# Data has random relationship between lags
# Potentially a bad candidate for time series
####################################################

######
# Start with a benchmark forecast
# Seasonal naive method as benchmark
#   y_t = y_{t-s} + e_t
#   value in January 2019 = value in January 2018 + error
# Good for benchmark because of seasonality
# Use de-trended data for consistency
######
# Subscribers
# Naive - must use difference data for naive - Residual = 11085
fit.subs <- snaive(ts.tix.difference[, 'subs'])
summary(fit.subs)
checkresiduals(fit.subs)
# ETS - Residual = 5347
fit_ets.subs <- ets(ts.tix[,'subs'])
summary(fit_ets.subs)
checkresiduals(fit_ets.subs)
# Arima - 
fit_arima.subs <- auto.arima(ts.tix[,'subs'],
                                #d=1, # differencing in data - same as ts.tix.difference
                                #D=1, # seasonality
                                stepwise = FALSE, # Saves time by not trying every model
                                approximation = FALSE, # True for long time series to avoid excessive time
                                trace = TRUE) # Shows which arima models were considered
summary(fit_arima.subs)
checkresiduals(fit_arima.subs)

################
# Single Tickets
################
# Naive - must use difference data for naive - Residual = 30397
fit.singles <- snaive(ts.tix.difference[, 'singles'])
summary(fit.singles)
checkresiduals(fit.singles)
# ETS - Residual = 15831
fit_ets.singles <- ets(ts.tix[, 'singles'])
summary(fit_ets.singles)
checkresiduals(fit_ets.singles)
# ARIMA model - Residual = 16958
fit_arima.singles <- auto.arima(ts.tix[,'singles'],
                                #d=1, # differencing in data - same as ts.tix.difference
                                #D=1, # seasonality
                                stepwise = FALSE, # Saves time by not trying every model
                                approximation = FALSE, # True for long time series to avoid excessive time
                                trace = TRUE) # Shows which arima models were considered
summary(fit_arima.singles)
checkresiduals(fit_arima.singles)

#####################################
# Forecast Singles with ETS
#####################################
fcst.singles <- forecast(fit_ets.singles, h = 12)
autoplot(fcst.singles, include = 24) + 
  ylab('revenue') + 
  xlab('time')
summary(fcst.singles)
