# US NAICS2012 ind-ind supply data transformation -- sandorjuhasz


library(data.table)
library(dplyr)
library(mefa4)
library(reshape2)
library(readxl)

# parameters
region <- "nuts4"
year <- 2017




# import crosswalk tables
naics_supp_naics <- data.table(read_xlsx("../data/crosswalk_tables/naics_correction_230927.xlsx", sheet = 1))
naics_isic <- fread("../data/crosswalk_tables/NAICS2012US-ISIC4.txt")
isic_nace <- fread("../data/crosswalk_tables/ISIC4_NACE2.txt")




# 1 -- prepare US supply table

# import supply table NAICS correction
naics_supp_naics <- data.table(read_xlsx("../data/crosswalk_tables/naics_correction_230927.xlsx", sheet = 1))

# import raw data -- already transformed to .csv from the original .xls
supp_raw <- fread("../data/io_external/us_io_supply2012_naics2012.csv")
dim(supp_raw)


# matrix to edgelist -- NOTE -- strange dimensions
supp_df <- Melt(as.matrix(supp_raw, rownames = 1))
colnames(supp_df) <- c("ind1", "ind2", "value")
length(unique(supp_df$ind1))
length(unique(supp_df$ind2))
supp_df$value[is.na(supp_df$value) == 1] <- 0


## NAICS supply -- to NAICS correction
head(supp_df)
head(naics_supp_naics)

# add edge id
supp_df$eid <- seq(1, nrow(supp_df), 1)

supp_df2 <- merge(
  supp_df,
  select(naics_supp_naics, naics_supp, naics_code),
  by.x = "ind1",
  by.y = "naics_supp",
  all.x = TRUE,
  all.y = FALSE
)
supp_df2 <- merge(
  supp_df2,
  select(naics_supp_naics, naics_supp, naics_code),
  by.x = "ind2",
  by.y = "naics_supp",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("1", "2")
)



# 2 -- NAICS to ISIC

# 4 digit transformation
supp_df2$naics1_4d <- substr(supp_df2$naics_code1, 1, 4)
supp_df2$naics2_4d <- substr(supp_df2$naics_code2, 1, 4)


# distribution part
supp_df2 <- supp_df2 %>%
  group_by(eid) %>%
  mutate(
    nr_naics1 = n_distinct(naics1_4d),
    nr_naics2 = n_distinct(naics2_4d),
    
    # tricky part
    d1 = 1 / nr_naics1,
    d2 = 1 / nr_naics2,
    distributor = d1 * d2,
    value_distributed = value * distributor
  ) %>%
  data.table()


# clear UP  -- NAICS x NAICS table
supp_df3 <- supp_df2 %>%
  select(naics1_4d, naics2_4d, value_distributed) %>%
  distinct() %>%
  group_by(naics1_4d, naics2_4d) %>%
  #mutate(n_eid = n()) %>%
  summarise(value_distributed2 = sum(value_distributed)) %>%
  data.table()


# create 4-digit crosswalk tables
naics_isic <- subset(
  naics_isic,
  #  (NAICS2012Code != "n/a") &
  (ISIC4Code != 12 & ISIC4Code != 14)
)

crosswalk_naics_isic <- naics_isic %>%
  mutate(naics_4d = as.numeric(substr(NAICS2012Code, 1, 4))) %>%
  select(naics_4d, ISIC4Code) %>%
  distinct() %>%
  group_by(naics_4d) %>%
  mutate(nr_isic = 1/n()) %>%
  mutate(naics_4d = as.character(naics_4d)) %>%
  data.table()


# join ISIC to NAICS 4d edgelist with weights
supp_df4 <- merge(
  supp_df3,
  crosswalk_naics_isic,
  by.x = "naics1_4d",
  by.y = "naics_4d",
  all.x = TRUE,
  all.y = FALSE,
  allow.cartesian = TRUE
)
supp_df4 <- merge(
  supp_df4,
  crosswalk_naics_isic,
  by.x = "naics2_4d",
  by.y = "naics_4d",
  all.x = TRUE,
  all.y = FALSE,
  allow.cartesian = TRUE,
  suffixes = c("1", "2")
) 
supp_df4$distributor <- supp_df4$nr_isic1 * supp_df4$nr_isic2
supp_df4$value_distributed3 <- supp_df4$value_distributed2 * supp_df4$distributor


# create separate NAICS -- NA ISIC code table
rest_supp_df4 <- subset(supp_df4, is.na(ISIC4Code1)==1 | is.na(ISIC4Code2)==1)
supp_df5 <- subset(supp_df4, is.na(ISIC4Code1)==0 & is.na(ISIC4Code2)==0)

# clear UP  -- ISIC x ISIC table
supp_df5 <- supp_df5 %>%
  select(ISIC4Code1, ISIC4Code2, value_distributed3) %>%
  distinct() %>%
  group_by(ISIC4Code1, ISIC4Code2) %>%
  summarise(value_distributed4 = sum(value_distributed3)) %>%
  data.table()


# 3 -- ISIC to NACE
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


# join NACE codes to ISIC-ISIC edgelist
supp_df6 <- merge(
  supp_df5,
  crosswalk_isic_nace,
  by.x = "ISIC4Code1",
  by.y = "isic_4d",
  all.x = TRUE,
  all.y = FALSE,
  allow.cartesian = TRUE
)
supp_df6 <- merge(
  supp_df6,
  crosswalk_isic_nace,
  by.x = "ISIC4Code2",
  by.y = "isic_4d",
  all.x = TRUE,
  all.y = FALSE,
  allow.cartesian = TRUE,
  suffixes = c("1", "2")
)

# distributor
supp_df6$distributor <- supp_df6$nr_nace1 * supp_df6$nr_nace2
supp_df6$value_distributed5 <- supp_df6$value_distributed4 * supp_df6$distributor

# clear UP  -- ISIC x ISIC table
supp_df6 <- supp_df6 %>%
  select(nace_4d1, nace_4d2, value_distributed5) %>%
  distinct() %>%
  group_by(nace_4d1, nace_4d2) %>%
  summarise(value_final = sum(value_distributed5)) %>%
  data.table()




# 4 -- add NAICS - NACE direct transformation






# 5 -- 
supp_final <- supp_df6
supp_final$ind1 <- supp_final$nace_4d1 %/% 10
supp_final$ind2 <- supp_final$nace_4d2 %/% 10
supp_final <- supp_final %>%
  group_by(ind1, ind2) %>%
  summarise(value_final = sum(value_final)) %>%
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

full_el$ind1 <- as.numeric(full_el$ind1)
full_el$ind2 <- as.numeric(full_el$ind2)








full_supp <- merge(
  full_el,
  supp_final,
  by = c("ind1", "ind2"),
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



