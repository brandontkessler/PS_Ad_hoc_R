library(lubridate)
library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)

#######################################################
###################### FUNCTIONS ######################
#######################################################

LoadData <- function(){
  rbind(read.csv('data/Clx19.csv', header = TRUE, skip = 3),
        read.csv('data/Pops19.csv', header = TRUE, skip = 3),
        read.csv('data/Summer19.csv', header = TRUE, skip = 3)) %>%
    mutate(perf_dt = mdy_hms(perf_dt))
}

NewDf <- function(dt){
  dt %>%
    filter(perf_dt > date_of_concert &
             perf_dt < date_of_concert + 1) %>%
    mutate(price_zone = substr(zone_desc, 1, 7)) %>%
    mutate(price_type_group = replace(price_type_group, price_type_group == "Flex", "Subscription")) %>%
    mutate(price_type_group = replace(price_type_group, price_type_group == "Discount", "Single ")) %>%
    mutate(price_type_group = str_trim(price_type_group)) %>%
    select(price_zone,price_type_group,paid_amt,attended) %>%
    mutate(row_count = 1)
}


SummarizeDf <- function(dt){
  dt %>%
    group_by(price_zone) %>%
    summarize(Seats = n(),
              SubSold = sum(row_count[price_type_group == 'Subscription']),
              SubAvg = mean(paid_amt[price_type_group == 'Subscription']),
              SubRev = sum(paid_amt[price_type_group == 'Subscription']),
              SglSold = sum(row_count[price_type_group == 'Single']),
              SglAvg = mean(paid_amt[price_type_group == 'Single']),
              SglRev = sum(paid_amt[price_type_group == 'Single']),
              TotSold = sum(row_count[price_type_group == 'Subscription' | 
                                        price_type_group == 'Single']),
              PrcntSold = TotSold/Seats,
              TotRev = sum(SubRev,SglRev),
              TotComp = sum(row_count[price_type_group == 'Comp']),
              PrcntComp = TotComp/Seats,
              Att = sum(row_count[attended == 'Attended']),
              CompAtt = sum(row_count[attended == 'Attended' & 
                                        price_type_group == 'Comp'])/TotComp,
              SubAtt = sum(row_count[attended == 'Attended' & 
                                       price_type_group == 'Subscription'])/SubSold,
              SglAtt = sum(row_count[attended == 'Attended' & 
                                       price_type_group == 'Single'])/SglSold,
              TotAtt = Att/sum(TotSold,TotComp)) %>%
    rename(Zone = price_zone)
}

BindFinalwTotalRow <- function(dt){
  x <- data.frame('Total',
                  sum(dt$Seats),
                  sum(dt$SubSold),
                  sum(dt$SubRev)/sum(dt$SubSold),
                  sum(dt$SubRev),
                  sum(dt$SglSold),
                  sum(dt$SglRev)/sum(dt$SglSold),
                  sum(dt$SglRev),
                  sum(dt$TotSold),
                  sum(dt$TotSold)/sum(dt$Seats),
                  sum(dt$TotRev),
                  sum(dt$TotComp),
                  sum(dt$TotComp)/sum(dt$Seats),
                  sum(dt$Att),
                  sum(dt$CompAtt*dt$TotComp)/sum(dt$TotComp),
                  sum(dt$SubAtt*dt$SubSold)/sum(dt$SubSold),
                  sum(dt$SglAtt*dt$SglSold)/sum(dt$SglSold),
                  sum(dt$Att)/sum(dt$TotSold+dt$TotComp))
  colnames(x) <- names(dt)
  rbind(dt,x)
}

DataFormatting <- function(dt){
  dt %>%
    mutate(SubAvg = dollar(SubAvg),
           SubRev = dollar(SubRev),
           SglAvg = dollar(SglAvg),
           SglRev = dollar(SglRev),
           PrcntSold = percent(PrcntSold),
           TotRev = dollar(TotRev),
           PrcntComp = percent(PrcntComp),
           CompAtt = percent(CompAtt),
           SubAtt = percent(SubAtt),
           SglAtt = percent(SglAtt),
           TotAtt = percent(TotAtt))
}


###############################################################################################
##################################### USER INPUT ##############################################
###############################################################################################

date_of_concert <- as.Date('2019-01-31')
# If unsure which number to lookup for specific concert, run entire code, then open
#   perf_dt_unique df to view a table showing concert number by performance date

###############################################################################################

df_og <- LoadData()
df <- NewDf(df_og)
df_final <- SummarizeDf(df)
df_final <- BindFinalwTotalRow(df_final)
df_final <- DataFormatting(df_final)

df_general <- df_final %>%
  select(Zone,Seats,SubSold,SubAvg,SubRev,SglSold,SglAvg,SglRev,TotSold,PrcntSold,TotRev)
df_comps <- df_final %>%
  select(Zone,Seats,TotComp,PrcntComp)
df_attendance <- df_final %>%
  select(Zone,Seats,Att,CompAtt,SubAtt,SglAtt,TotAtt)
