# US NAICS2012 ind-ind supply data transformation -- sandorjuhasz


library(data.table)
library(dplyr)
library(mefa4)




# 1 -- prepare crosswalk tables

# import crosswalk tables
naics_isic <- fread("../data/crosswalk_tables/NAICS2012US-ISIC4.txt")
isic_nace <- fread("../data/crosswalk_tables/ISIC4_NACE2.txt")

# baseline cleaning
naics_isic <- subset(naics_isic, NAICS2012Code != "n/a")

# create 4-digit crosswalk tables
crosswalk_naics_isic <- naics_isic %>%
  mutate(naics_4d = substr(NAICS2012Code, 1, 4)) %>%
  select(naics_4d, ISIC4Code) %>%
  unique() %>%
  group_by(naics_4d) %>%
  mutate(nr_isic = 1/n()) %>%
  data.table()

crosswalk_isic_nace <- isic_nace %>%
  mutate(
    isic_digits = nchar(ISIC4code),
    #isic_4d = as.numeric(ISIC4code)
    isic_4d = sub("^0+", "", ISIC4code)
  ) %>%
  filter(isic_digits == 4) %>%
  mutate(
    nace_4d = sub("^(..).", "\\1", NACE2code),
    nace_4d = sub("^0+", "", nace_4d)
  ) %>%
  group_by(isic_4d) %>%
  mutate(
    nr_nace = 1/n()
  ) %>%
  data.table()




# 2 -- prepare US supply table

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
supp_df$naics1_4d <- substr(supp_df$ind1, 1, 4)
supp_df$naics2_4d <- substr(supp_df$ind2, 1, 4)

supp_4d <- supp_df %>%
  group_by(naics1_4d, naics2_4d) %>%
  summarise(value = sum(value)) %>%
  data.table()


# merge
table1 <- merge(
  supp_4d,
  crosswalk_naics_isic,
  by.x = "naics2_4d",
  by.y = "naics_4d",
  all.x = TRUE,
  all.y = TRUE,
  allow.cartesian = TRUE
)








