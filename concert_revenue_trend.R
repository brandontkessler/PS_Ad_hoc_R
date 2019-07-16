# --- NOTES ---
# Make sure to update the imported data with new file names
#
# Make sure to update the if else statement in DATA SETUP
#   to include only the years in question

###################################################################################
###################################################################################
# --------------------------------------------------------------------------
# --------------------------------- USER INPUTS -------------------------
# --------------------------------------------------------------------------

date_to_trend_until <- as.Date('2019-01-31')
# Concert date must be in this format

###################################################################################
###################################################################################
# --------------------------------------------------------------------------
# --------------------------------- DATA SETUP -------------------------
# --------------------------------------------------------------------------
# ------------------ TICKETING ---------------------
# import ticketing data
df_og <- rbind(read.csv('data/Clx15.csv', header = TRUE, skip = 3),
               read.csv('data/Clx16.csv', header = TRUE, skip = 3),
               read.csv('data/Clx17.csv', header = TRUE, skip = 3),
               read.csv('data/Clx18.csv', header = TRUE, skip = 3),
               read.csv('data/Clx19.csv', header = TRUE, skip = 3))

# Only keep columns below
keep_cols <- c('summary_cust_id', 
               'season_desc', 
               'price_type_group',
               'paid_amt',
               'zone_desc',
               'perf_dt')
df <- df_og[keep_cols]

# --------------- Remove NA summary customer ID
na_vec_cust_id <- which(!complete.cases(df$summary_cust_id))
df <- df[-na_vec_cust_id,]

# -------- Format date
library(lubridate)
df$perf_dt <- mdy_hms(df$perf_dt)

# -------- Aggregate by date
df <- as.data.frame(aggregate(paid_amt~perf_dt,
                              df,
                              sum))

# -------- DOW and REMOVE TUESDAY
df$perf_DOW <- weekdays(as.Date(df$perf_dt,'%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles'))
df <- df[df$perf_DOW != 'Tuesday',]

# --------- PARSE DATE
df$month <- format(df$perf_dt, '%Y-%m')

# ----- Exclude anything after trend to date
df <- df[df$perf_dt < date_to_trend_until + 1,]

# -------- Month + DOW to aggregate as average across whole month
df$combo_col <- paste(df$month,df$perf_DOW)

# -------- Aggregate by combo column
df <- as.data.frame(aggregate(paid_amt~combo_col,
                              df,
                              mean))

# Split combo column
library(tidyr)
df <- separate(df,combo_col,into=c('month','day'),sep=' ')

# rename paid_amt column
names(df) <- c('date','day','total_revenue')

###################################################################################
############################### ORGANIZE DF ########################################
###################################################################################
df_thursday <- df[df$day == 'Thursday',]
df_friday <- df[df$day == 'Friday',]
df_saturday <- df[df$day == 'Saturday',]

cols_to_keep <- c('date','total_revenue')
df_thursday <- df_thursday[cols_to_keep]
df_friday <- df_friday[cols_to_keep]
df_saturday <- df_saturday[cols_to_keep]

# Rename revenue column
names(df_thursday) <- c('date','thursday')
names(df_friday) <- c('date','friday')
names(df_saturday) <- c('date','saturday')

df <- merge(df_thursday,df_friday, by = 'date')
df <- merge(df,df_saturday, by = 'date')

###################################################################################
############################### ORGANIZE BY YEAR ##################################
###################################################################################
df_year <- df
df_year$year <- substr(df_year$date, start = 1, stop = 4)
df_year$date <- NULL
df_year <- aggregate(. ~ year,
                     df_year,
                     mean)

###################################################################################
# --------------------------------------------------------------------------
# --------------------------------- WRITE TO XLSX -------------------------
# --------------------------------------------------------------------------

library('xlsx')
write.xlsx(df, file = paste('concert_trend_clx.xlsx'),
           sheetName = 'monthly', row.names = FALSE, append = FALSE)
write.xlsx(df_year, file = paste('concert_trend_clx.xlsx'),
           sheetName = 'annual', row.names = FALSE, append = TRUE)