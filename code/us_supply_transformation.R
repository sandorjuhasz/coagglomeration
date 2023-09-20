# US NAICS2012 ind-ind supply data transformation -- sandorjuhasz


library(data.table)
library(dplyr)
library(mefa4)
library(reshape2)


# parameters
region <- "nuts4"
year <- 2017




# 1 -- prepare crosswalk tables

# import crosswalk tables
naics_isic <- fread("../data/crosswalk_tables/NAICS2012US-ISIC4.txt")
isic_nace <- fread("../data/crosswalk_tables/ISIC4_NACE2.txt")


# baseline cleaning
naics_isic <- subset(
  naics_isic,
  (NAICS2012Code != "n/a") &
  (ISIC4Code != 12)
)

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


# join ISIC codes to NAICS industry2
table1 <- merge(
  supp_4d,
  crosswalk_naics_isic,
  by.x = "naics2_4d",
  by.y = "naics_4d",
  all.x = TRUE,
  all.y = TRUE,
  allow.cartesian = TRUE
)


# many mismatches -- clean up for now and revisit this part
table1 <- table1[complete.cases(table1[ , c("naics1_4d", "ISIC4Code", "value")]), ] 


# compute corrected value and keep key columns
table1$value_corrected <- table1$value * table1$nr_isic

table1 <- table1 %>%
  select(naics1_4d, ISIC4Code, value_corrected) %>%
  unique() %>%
  arrange(naics1_4d, ISIC4Code) %>%
  data.table()


# edgelist to matrix
mat1 <- as.matrix(reshape2::acast(table1, naics1_4d ~ ISIC4Code, value.var = "value_corrected", fun.aggregate = sum))




# 3 -- matrix manipulation
table2 <- crosswalk_isic_nace %>%
  select(isic_4d, nace_4d, nr_nace) %>%
  data.table() %>%
  unique()


# filter for relevant industries -- present in the supply data
isic_codes <- unique(table1$ISIC4Code)
table2 <- subset(table2, isic_4d %in% isic_codes)


# edgelist to matrix again
mat2 <- as.matrix(reshape2::acast(table2, isic_4d ~ nace_4d, value.var = "nr_nace"))
mat2[is.na(mat2)==1] <- 0


# matrix multiplication
naics_nace_mat <- mat1 %*% mat2
nace_nace_mat <- t(naics_nace_mat) %*% naics_nace_mat


# matrix to edgelist again
tsupp <- data.table(Melt(as.matrix(nace_nace_mat)))
colnames(tsupp) <- c("ind1", "ind2", "value")




# 4 -- normalized IO flow between 3 digit NACE-NACE -- final touch
tsupp$ind1_3d <- substr(tsupp$ind1, 1, 3)
tsupp$ind2_3d <- substr(tsupp$ind2, 1, 3)

tsupp <- tsupp %>%
  group_by(ind1_3d, ind2_3d) %>%
  summarise(value = sum(value)) %>%
  data.table()


# industry codes from the master file
mdf3 <- fread(paste0("../data/oc10_2023_sep/oc_mdf3_", region, "_", year, "_based.csv"), sep = ";")
industries <- unique(c(mdf3$ind1, mdf3$ind2))


# create full combination of industry codes and merge the US data
full_el <- data.table(expand.grid(industries, industries))
colnames(full_el) <- c("ind1", "ind2")
full_el$ind1 <- as.character(full_el$ind1)
full_el$ind2 <- as.character(full_el$ind2)


# edge id for undirected edgelist creation
add_eid <- function(elist_frame_df){
  tech_df1 <-
    elist_frame_df %>%
    filter(elist_frame_df[, 1] <= elist_frame_df[, 2]) %>%
    mutate(eid = seq(from = 1, to = n(), by = 1))
  
  tech_df2 <-
    tech_df1 %>%
    select(2, 1, 3)	
  colnames(tech_df2) <- colnames(tech_df1)
  
  elist_frame_df <-
    bind_rows(tech_df1, tech_df2) %>%
    distinct() %>%
    arrange(.[1], .[2])
  
  return(elist_frame_df)
}
full_el <- add_eid(full_el)


full_supp <- merge(
  full_el,
  tsupp,
  by.x = c("ind1", "ind2"),
  by.y = c("ind1_3d", "ind2_3d"),
  all.x = TRUE,
  all.y = FALSE
)

full_supp$value[is.na(full_supp$value)==1] <- 0


# sr_norm style
full_supp <- full_supp %>%
  group_by(ind1) %>%
  mutate(f_i = sum(value)) %>%
  group_by(ind2) %>%
  mutate(f_j = sum(value)) %>%
  ungroup() %>%
  mutate(f = sum(value)) %>%
  mutate(iv_io = value / ((f_i * f_j) / f)) %>%
  mutate(iv_io_norm = (iv_io - 1) / (iv_io + 1)) %>%
  ungroup() %>%
  data.table()

full_supp$iv_io_norm[is.na(full_supp$iv_io_norm)==1] <- -1


# make undirected
full_supp <- full_supp %>%
  arrange(eid, ind1, ind2) %>%
  group_by(eid) %>%
  mutate(iv_io_norm = sum(iv_io_norm, na.rm = TRUE) / 2) %>%
  mutate(tag_drop = ifelse(sum(is.na(iv_io)) == 2, 1, 0)) %>% # handle the twoway missings here
  ungroup() %>%
  # drop if edge is not identified either way, or loop
  # filter(ind_i != ind_j) %>%
  # filter(tag_drop != 1) %>%
  # keep necessary variables
  data.table()



write.table(full_supp, "../outputs/us_supply_3digit_nace_nace.csv", sep=";", row.names = FALSE)



