# US NAICS2012 ind-ind supply data transformation -- sandorjuhasz


library(data.table)
library(dplyr)
library(mefa4)
library(reshape2)
library(readxl)


# parameters
region <- "nuts4"
year <- 2017



# import aggregate table for ECU
#edf <- fread("../data/io_external/sector_sector_interactions_fromVAT_22300.csv")
edf <- fread("../data/io_external/sector_sector_interactions_fromVAT_0.csv")


# drop first letters
edf$ind1_isic <- as.integer(substr(edf$SuppISIC, 2, 6))
edf$ind2_isic <- as.integer(substr(edf$CustISIC, 2, 6))


# ISIC to NACE table
isic_nace <- fread("../data/crosswalk_tables/ISIC4_NACE2.txt")


# create crosswalk table with distributor column
crosswalk_isic_nace <- isic_nace %>%
  mutate(
    isic_digits = nchar(ISIC4code),
    isic_4d = sub("^0+", "", ISIC4code)
  ) %>%
  filter(isic_digits == 4) %>%
  mutate(
    nace_4d = sub("^(..).", "\\1", NACE2code),
    nace_4d = sub("^0+", "", nace_4d)
  ) %>%
  group_by(isic_4d) %>%
  mutate(
    isic_4d = as.numeric(isic_4d),
    nace_4d = as.numeric(nace_4d),
    nr_nace = 1/n()
  ) %>%
  select(isic_4d, nace_4d, nr_nace) %>%
  data.table() %>%
  unique()
crosswalk_isic_nace$isic_4d <- as.integer(crosswalk_isic_nace$isic_4d)


# join
edf <- merge(
  edf,
  crosswalk_isic_nace,
  by.x = "ind1_isic",
  by.y = "isic_4d",
  all.x = TRUE,
  all.y = FALSE,
  allow.cartesian = TRUE
)
edf <- merge(
  edf,
  crosswalk_isic_nace,
  by.x = "ind2_isic",
  by.y = "isic_4d",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("1", "2"),
  allow.cartesian = TRUE
)


# distributor
edf$distributor <- edf$nr_nace1 * edf$nr_nace2
edf$sum_weight <- edf$sum_weight * edf$distributor


# clear UP  -- NACE x NACE table
iv_edf <- edf %>%
  select(nace_4d1, nace_4d2, sum_weight) %>%
  distinct() %>%
  group_by(nace_4d1, nace_4d2) %>%
  summarise(value_final = sum(sum_weight)) %>%
  filter(is.na(nace_4d1) == 0 & is.na(nace_4d2) == 0) %>%
  data.table()


# transform to 3-digit codes
iv_edf$ind1 <- iv_edf$nace_4d1 %/% 10
iv_edf$ind2 <- iv_edf$nace_4d2 %/% 10
iv_edf <- iv_edf %>%
  group_by(ind1, ind2) %>%
  summarise(value = sum(value_final)) %>%
  data.table()


# industry codes from the master file
mdf3 <- fread(paste0("../data/oc11_2023_oct//oc_mdf3_", region, "_", year, "_based.csv"), sep = ";")
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

full_el$ind1 <- as.numeric(full_el$ind1)
full_el$ind2 <- as.numeric(full_el$ind2)


# join transaction data
iv_full <- merge(
  full_el,
  iv_edf,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
iv_full$value[is.na(iv_full$value)==1] <- 0


# sr_norm style
iv_full <- iv_full %>%
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

iv_full$iv_io_norm[is.na(iv_full$iv_io_norm)==1] <- -1


# make undirected
iv_full <- iv_full %>%
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



#write.table(iv_full, "../outputs/ecu_vat_iv.csv", sep=";", row.names = FALSE)
write.table(iv_full, "../outputs/ecu_vat_iv_no_limit.csv", sep=";", row.names = FALSE)


