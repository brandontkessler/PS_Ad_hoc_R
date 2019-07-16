# --- NOTES ---
# Make sure to update the imported data with new file names
# --------------------------------- USER INPUTS -------------------------

concert_number_lookup <- 33
# If unsure which number to lookup for specific concert, run entire code, then open
#   perf_dt_unique df to view a table showing concert number by performance date

library(lubridate)
library(tidyverse)


#######################################################
###################### FUNCTIONS ######################
#######################################################

LoadData <- function(){
  rbind(read.csv('data/Clx19.csv', header = TRUE, skip = 3),
        read.csv('data/Pops19.csv', header = TRUE, skip = 3),
        read.csv('data/Summer19.csv', header = TRUE, skip = 3)) %>%
    mutate(perf_dt = mdy_hms(perf_dt))
}

UniquePerfDataframe <- function(dt){
  dt %>% 
    distinct(perf_dt) %>%
    arrange(perf_dt) %>%
    mutate(concert_number_lookup = c(1:53))
}

LookupValue <- function(dt,con_num=concert_number_lookup){
  df <- data.frame(con_num)
  names(df) <- c('concert_number_lookup')
  merge(df,dt)
}

SubDf <- function(dt){
  dt %>%
    filter(summary_cust_id > 0,
           price_type_group == 'Flex' |
             price_type_group == 'Subscription')
}

NewDf <- function(dt,value,dt_sub){
  dt %>%
    mutate(subscriber = summary_cust_id %in% dt_sub$summary_cust_id) %>%
    filter(summary_cust_id > 0, 
           perf_dt <= value$perf_dt[1],
           price_type_group != 'Comp',
           summary_cust_id != 2010347,
           subscriber == FALSE)
}

CreateUniquePerfSpread <- function(dt){
  dt %>%
    select(perf_dt, summary_cust_id) %>%
    distinct() %>%
    mutate(perf_counter = 1) %>%
    spread(key=perf_dt,
           value=perf_counter,
           fill=0)
}

AddTotalPerfs <- function(dt){
  dt %>%
    mutate(total_perfs = if(ncol(dt) > 2){
      rowSums(dt[,-1])
    } else {
      1
    })
}

AggTotalPerformances <- function(dt){
  dt %>%
    group_by(total_perfs) %>%
    tally() %>%
    rename(total_customers = n)
}

GeneratePlot <- function(dt){
  ggplot(data = dt, aes(x=total_perfs, y=total_customers, colors = 'black')) + 
    guides(fill=FALSE) + 
    geom_bar(stat='identity', fill='#166713', color = 'black') + 
    xlab('Concerts Attended') + 
    ylab('Total Customers') + 
    ggtitle('Concerts Attended by Unique Customers') +
    scale_x_continuous(breaks = seq(1, max(dt$total_perfs), by = 1),
                       labels = seq(1, max(dt$total_perfs), by = 1),
                       limits = if(max(dt$total_perfs) >= 10){
                         c(0.5,max(dt$total_perfs)+0.5)
                       } else {
                         c(0.5,10)
                       }) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 20, face='bold'),
          axis.title = element_text(size = 16),
          panel.grid.minor = element_blank())
}


#######################################################
#################### RUN THE CODE #####################
#######################################################
df_og <- LoadData()
perf_dts_unique <- UniquePerfDataframe(df_og)
lookup_value <- LookupValue(perf_dts_unique)
df_sub <- SubDf(df_og)
df <- NewDf(df_og,lookup_value,df_sub)
df_unique_perf_spread <- CreateUniquePerfSpread(df)
df_unique_perf_spread <- AddTotalPerfs(df_unique_perf_spread)
total_performances <- AggTotalPerformances(df_unique_perf_spread)
GeneratePlot(total_performances)


