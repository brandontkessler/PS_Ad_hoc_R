library(dplyr)

df_og <- rbind(read.csv('data/PS 2014 Promotions.csv', header=TRUE),
                read.csv('data/PS 2015 Promotions.csv', header=TRUE),
                read.csv('data/PS 2016 Promotions.csv', header=TRUE),
                read.csv('data/PS 2017 Promotions.csv', header=TRUE),
                read.csv('data/PS 2018 Promotions.csv', header=TRUE))

df <- df_og

df_promo <- df %>%
  replace_na(list(response = 0)) %>%
  group_by(mt_desc) %>%
  summarise(number_of_outreach = n(),
            number_of_conversions = sum(response)) %>%
  mutate(conv_perc = number_of_conversions/number_of_outreach)

# THIS CANNOT BE ACCURATE