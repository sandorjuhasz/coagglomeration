# US NAICS2012 ind-ind supply data transformation -- sandorjuhasz


library(data.table)
library(dplyr)


# import crosswalk tables
naics_isic <- fread("../data/crosswalk_tables/NAICS2012US-ISIC4.txt")


# baseline cleaning
naics_isic <- subset(naics_isic, NAICS2012Code != "n/a")


# create 4-digit crosswalk
cross_table1 <- naics_isic %>%
  mutate(naics_4d = substr(NAICS2012Code, 1, 4)) %>%
  select(naics_4d, ISIC4Code) %>%
  data.table() %>%
  unique()


# 

