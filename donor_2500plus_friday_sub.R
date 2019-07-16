source("functions/data_import.R")
library(xlsx)
# 2500 donors
donor.data <- donor_data() %>%
  filter(cont_fy == 2018) %>%
  group_by(summary_cust_id) %>%
  summarise(total_gift_fy18 = sum(gift_plus_pledge)) %>%
  filter(total_gift_fy18 >= 2500)

tix.data <- ticketing_data(c('Clx19')) %>%
  filter(perf_dt < today()) %>%
  mutate(dow = weekdays(perf_dt),
         attendance = ifelse(attended == "Attended",1,0)) %>%
  filter(dow == "Friday" | dow == "Tuesday",
         price_type_group == "Subscription",
         !is.na(summary_cust_id)) %>%
  group_by(summary_cust_id) %>%
  summarise(tix = n(),
            attendance = sum(attendance),
            total_paid_amt = sum(paid_amt)) %>%
  mutate(attendance_rate = attendance / tix)

user.info.data <- read.csv("/data/active_user_info.csv") %>%
  select(customer_no, display_name)


merged.data <- donor.data %>%
  merge(tix.data, by = "summary_cust_id", all.x = TRUE) %>%
  filter(!is.na(tix)) %>%
  merge(user.info.data, by.x = "summary_cust_id", by.y = "customer_no", all.x = TRUE)


write.xlsx(merged.data, "donor_2500plus_friday_sub.xlsx")
