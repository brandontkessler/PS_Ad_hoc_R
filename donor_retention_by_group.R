# No user input required
# If changing FYs, (not fy15-19), must update the section labeled: "MAY NEED TO UPDATE"
# Everything else is good

library(dplyr)
library(xlsx)
library(ggplot2)

LoadData <- function(){
  return(read.csv('data/enhanced_fund_fy15-present.csv', header=TRUE))
}

CleanData <- function(ds){
  ds %>%
    select(summary_cust_id,gift_plus_pledge,cont_fy) %>%
    group_by(cont_fy,summary_cust_id) %>%
    summarise(gift_plus_pledge = sum(gift_plus_pledge)) %>%
    mutate(donor_group = ifelse(gift_plus_pledge < 1000,'A',
                                ifelse(gift_plus_pledge < 10000,'B',
                                       ifelse(gift_plus_pledge < 100000,'C','D'))),
           fiscal_year = paste0('fy',cont_fy)) %>%
    group_by(summary_cust_id) %>%
    select(summary_cust_id,fiscal_year,donor_group) %>%
    spread(fiscal_year,donor_group)
}

retention_evaluation <- function(ds){
  sequence <- colnames(ds[3:ncol(ds)])
  
  for(col in sequence){
    A <- ds[,which(colnames(ds) == colnames(ds[,col]))-1]
    B <- ds[,col]
    
    for(group in c('A','B','C','D')){
      if(group == 'A'){
        ds[[paste0(colnames(B[1]),'grp',group)]] <- ifelse(A[,1] != 'A', NA, 
                                                           ifelse(!is.na(B[,1]),
                                                                  ifelse(B[,1] == 'A',
                                                                         'maintain',
                                                                         'upgrade'),
                                                                  'lost'))
      } else if(group == 'B'){
        ds[[paste0(colnames(B[1]),'grp',group)]] <- ifelse(A[,1] != 'B', NA,
                                                           ifelse(!is.na(B[,1]),
                                                                  ifelse(B[,1] == 'A',
                                                                         'downgrade',
                                                                         ifelse(B[,1] == 'B',
                                                                                'maintain',
                                                                                'upgrade')),
                                                                  'lost'))
      } else if(group == 'C'){
        ds[[paste0(colnames(B[1]),'grp',group)]] <- ifelse(A[,1] != 'C', NA, 
                                                           ifelse(!is.na(B[,1]),
                                                                  ifelse(B[,1] == 'C',
                                                                         'maintain',
                                                                         ifelse(B[,1] == 'D',
                                                                                'upgrade',
                                                                                'downgrade')),
                                                                  'lost'))
      } else {
        ds[[paste0(colnames(B[1]),'grp',group)]] <- ifelse(A[,1] != 'D', NA,
                                                           ifelse(!is.na(B[,1]),
                                                                  ifelse(B[,1] == 'D',
                                                                         'maintain',
                                                                         'downgrade'),
                                                                  'lost'))
      }
    }
  }
  return(ds)
}

create_dfs_fy <- function(ds,year){
  new_df <- ds %>%
    select(summary_cust_id,
           paste0('fy',year,'grpA'),
           paste0('fy',year,'grpB'),
           paste0('fy',year,'grpC'),
           paste0('fy',year,'grpD'))
  
  colnames(new_df) <- c('summary_cust_id','grpA','grpB','grpC','grpD')
  
  new_df <- new_df %>%
    mutate(grp = ifelse(!is.na(grpA),
                        'A',
                        ifelse(!is.na(grpB),
                               'B',
                               ifelse(!is.na(grpC),
                                      'C',
                                      ifelse(!is.na(grpD),
                                             'D',NA)))),
           ret_score = coalesce(grpA,grpB,grpC,grpD),
           count_col = 1) %>%
    group_by(grp) %>%
    summarise(py_tot = sum(count_col),
              lost = sum(count_col[ret_score == 'lost'])/py_tot,
              downgrade = sum(count_col[ret_score == 'downgrade'])/py_tot,
              maintain = sum(count_col[ret_score == 'maintain'])/py_tot,
              upgrade = sum(count_col[ret_score == 'upgrade'])/py_tot,
              total_retention = sum(downgrade,maintain,upgrade)) %>%
    select(-one_of('py_tot'))
  
  return(new_df[-nrow(new_df),])
}


df_og <- LoadData()
donors <- CleanData(df_og)
retention <- retention_evaluation(donors)


################# MAY NEED TO UPDATE - OPEN ######################
fy16 <- create_dfs_fy(retention,2016) %>%
  mutate(fy = 2016) %>%
  select(grp,fy,everything())

fy17 <- create_dfs_fy(retention,2017) %>%
  mutate(fy = 2017) %>%
  select(grp,fy,everything())

fy18 <- create_dfs_fy(retention,2018) %>%
  mutate(fy = 2018) %>%
  select(grp,fy,everything())

fy19 <- create_dfs_fy(retention,2019) %>%
  mutate(fy = 2019) %>%
  select(grp,fy,everything())

final_df <- rbind(fy16,fy17,fy18,fy19) %>%
  arrange(grp)
################# MAY NEED TO UPDATE - CLOSE ######################


write.xlsx(as.data.frame(final_df[final_df$grp=='A',]),'donor_retention.xlsx','grpA',row.names=FALSE)
write.xlsx(as.data.frame(final_df[final_df$grp=='B',]),'donor_retention.xlsx','grpB',row.names=FALSE,append=TRUE)
write.xlsx(as.data.frame(final_df[final_df$grp=='C',]),'donor_retention.xlsx','grpC',row.names=FALSE,append=TRUE)
write.xlsx(as.data.frame(final_df[final_df$grp=='D',]),'donor_retention.xlsx','grpD',row.names=FALSE,append=TRUE)


