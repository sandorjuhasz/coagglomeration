# US NAICS2012 ind-ind supply data transformation -- sandorjuhasz


library(data.table)
library(dplyr)
library(tidyr)
library(mefa4)


# import raw data -- already transformed to .csv from the original .xls
supp_raw <- fread("../data/io_external/us_io_supply2012_naics2012.csv")
dim(supp_raw)


# matrix to edgelist -- NOTE -- strange dimensions
supp_df <- Melt(as.matrix(supp_raw, rownames = 1))
colnames(supp_df) <- c("ind1", "ind2", "value")
length(unique(supp_df$ind1))
length(unique(supp_df$ind2))
supp_df$value[is.na(supp_df$value) == 1] <- 0


# create 4-digit industry-industry edgelist
supp_df$ind1_4d <- substr(supp_df$ind1, 1, 4)
supp_df$ind2_4d <- substr(supp_df$ind2, 1, 4)

supp_df4 <- supp_df %>%
  group_by(ind1_4d, ind2_4d) %>%
  summarise(value = sum(value)) %>%
  data.table()


# 