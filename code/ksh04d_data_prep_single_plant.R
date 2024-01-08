### COAGGLOMERATION project -- data preparation for regressions ###
# by sandorjuhasz



library(Matrix)
library(data.table)
library(dplyr)
library(foreign)
library(igraph)
library(reshape2)
source("../scripts/00_functions.R")



# parameters
#reg <- "megye"
#output_reg <- "nuts3"
reg <- "jaras"
output_reg <- "nuts4"
#reg <- "telepules"
#output_reg <- "city"
ind <- 3
focal_year <- 2017
min_emp_per_firm <- 2
min_indreg_emp <- 2
mne_min_foreign_share <- 0.5
mne_min_emp <- 5
manufacturing_min_nace <- 100
manufacturing_max_nace <- 350
services_min_nace <- 490
services_max_nace <- 960
manufacturing_focus <- FALSE
services_focus <- FALSE
manufacturing_and_services_focus <- FALSE




###### 01 -- Mcp matrix to calculate coagglomeration measures ######

# firm information -- prep
merleg <- fread(paste0("../data/mta_merleg_full_", focal_year,".csv"))

# rearrange and filter the original merleg data
merleg <- reshape_merleg_data(merleg, min_emp_per_firm = min_emp_per_firm)


# read gszr data
gszr <- fread(paste0("../../side_project00_source_data/output/gszr_full_", focal_year, ".csv"))

# rearrange and filter the original gszr data
gszr <- reshape_gszr_data(gszr, min_emp_per_firm)


# firms -- merleg + gszr for missing
firms <- create_firms_data(merleg, gszr, include_gszr = TRUE)
rm(merleg, gszr)
gc()


# add geographic info to firms -- with name corrections -- agglom_data is the required table
telecodes <- fread("../data/teruleti_kodok.csv", encoding = "Latin-1")
agglom_data <- fread("../data/agglomerations_data.csv")
firms <- location_info_to_firm_data(firms, telecodes, agglom_data, include_agglomeration = TRUE)
firms <- location_specific_cleaning(firms, region_level = reg)

# MNE identification
firms$mne <- ifelse((firms$emp >= mne_min_emp) & (firms$foreign_share >= mne_min_foreign_share), 1, 0)
firms$mne[is.na(firms$mne) == 1] <- 0




# focus on single plant firms ONLY
sw_single_plant <- fread("../outputs/sw_table_opten_admin_single_plant.csv")
firms <- merge(
  firms,
  sw_single_plant,
  by = "azonosito",
  all.x = TRUE,
  all.y = FALSE
)

# 159.376 -- 66.693
print(nrow(firms))
firms <- subset(firms, nr_locations == 1)
print(nrow(firms))




# Mcp with employment data
mcp_mat <- mcp_from_firm_data(all_firms = firms,
                              focal_firms = firms,
                              region_level = reg,
                              industry_level = ind,
                              rca = FALSE,
                              min_indreg_emp = min_indreg_emp)


# Mcp with RCA 0/1
mcp_mat_rca01 <- mcp_from_firm_data(all_firms = firms,
                              focal_firms = firms,
                              region_level = reg,
                              industry_level = ind,
                              rca = TRUE,
                              min_indreg_emp = min_indreg_emp)


# MNE / local (domestic) Mcp tables
mnes <- subset(firms, mne == 1)
locals <- subset(firms, mne == 0)

# MNE only
mne_mcp_mat <- mcp_from_firm_data(all_firms = firms,
                                           focal_firms = mnes,
                                           region_level = reg,
                                           industry_level = ind,
                                           rca = FALSE,
                                           min_indreg_emp = min_indreg_emp)

mne_mcp_mat_rca01 <- mcp_from_firm_data(all_firms = firms,
                                  focal_firms = mnes,
                                  region_level = reg,
                                  industry_level = ind,
                                  rca = TRUE,
                                  min_indreg_emp = min_indreg_emp)


# local (domestic) only
local_mcp_mat <- mcp_from_firm_data(all_firms = firms,
                                             focal_firms = locals,
                                             region_level = reg,
                                             industry_level = ind,
                                             rca = FALSE,
                                             min_indreg_emp = min_indreg_emp)

local_mcp_mat_rca01 <- mcp_from_firm_data(all_firms = firms,
                                    focal_firms = locals,
                                    region_level = reg,
                                    industry_level = ind,
                                    rca = TRUE,
                                    min_indreg_emp = min_indreg_emp)



# mix -- MNE x local (domestic) -- Mcp base for MNE * local product
mcp_df <- data.table(reshape2::melt(mcp_mat), col)[,c(1:2)]
colnames(mcp_df) <- c("reg", "ind")



# mixed -- mne %*% local -- with emp
mne_df1_mcp <- data.table(reshape2::melt(mne_mcp_mat))
colnames(mne_df1_mcp) <- c("reg", "ind", "value")
mne_df1_mixed <- merge(
  mcp_df,
  mne_df1_mcp,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)
mne_df1_mixed[is.na(mne_df1_mixed)==1] <- 0

local_df1_mcp <- data.table(reshape2::melt(local_mcp_mat))
colnames(local_df1_mcp) <- c("reg", "ind", "value")
local_df1_mixed <- merge(
  mcp_df,
  local_df1_mcp,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)
local_df1_mixed[is.na(local_df1_mixed)==1] <- 0

mne_mat1_mixed <- create_industry_region_matrix(
  mne_df1_mixed,
  region_col = "reg",
  industry_col = "ind",
  weight_col = "value"
)

local_mat1_mixed <- create_industry_region_matrix(
  local_df1_mixed,
  region_col = "reg",
  industry_col = "ind",
  weight_col = "value"
)



# mixed -- mne %*% local -- with RCA 0/1
mne_df2_rca <- data.table(reshape2::melt(mne_mcp_mat_rca01))
colnames(mne_df2_rca) <- c("reg", "ind", "value")
mne_df2_mixed <- merge(
  mcp_df,
  mne_df2_rca,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)
mne_df2_mixed[is.na(mne_df2_mixed)==1] <- 0

local_df2_rca <- data.table(reshape2::melt(local_mcp_mat_rca01))
colnames(local_df2_rca) <- c("reg", "ind", "value")
local_df2_mixed <- merge(
  mcp_df,
  local_df2_rca,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)
local_df2_mixed[is.na(local_df2_mixed)==1] <- 0

mne_mat2_mixed <- create_industry_region_matrix(
  mne_df2_mixed,
  region_col = "reg",
  industry_col = "ind",
  weight_col = "value"
)

local_mat2_mixed <- create_industry_region_matrix(
  local_df2_mixed,
  region_col = "reg",
  industry_col = "ind",
  weight_col = "value"
)





###### 02 -- dependent variables ######

# EGK coagglomeration
egk_df <- EGK_coagglomeration(mcp_mat, return_matrix = FALSE)
egk_mne_df <- EGK_coagglomeration(mne_mcp_mat, return_matrix = FALSE)
colnames(egk_mne_df) <- c("ind1", "ind2", "egk_coagg_mne")
egk_local_df <- EGK_coagglomeration(local_mcp_mat, return_matrix = FALSE)
colnames(egk_local_df) <- c("ind1", "ind2", "egk_coagg_local")


# Porter coagglomeration
# - based on employment
porter_df <- data.table(reshape2::melt(cor(mcp_mat, mcp_mat)))
colnames(porter_df) <- c("ind1", "ind2", "coagg_porter_emp")
mne_porter_df <- data.table(reshape2::melt(cor(mne_mcp_mat, mne_mcp_mat)))
colnames(mne_porter_df) <- c("ind1", "ind2", "coagg_porter_emp_mne")
local_porter_df <- data.table(reshape2::melt(cor(local_mcp_mat, local_mcp_mat)))
colnames(local_porter_df) <- c("ind1", "ind2", "coagg_porter_emp_local")
mixed_porter_df <- data.table(reshape2::melt(cor(mne_mat1_mixed, local_mat1_mixed)))
colnames(mixed_porter_df) <- c("ind1", "ind2", "coagg_porter_emp_mixed")



# Porter coagglomeration
# - based on RCA 0/1
porter_df_rca <- data.table(reshape2::melt(cor(mcp_mat_rca01, mcp_mat_rca01)))
colnames(porter_df_rca) <- c("ind1", "ind2", "coagg_porter_rca01")
mne_porter_df_rca <- data.table(reshape2::melt(cor(mne_mcp_mat_rca01, mne_mcp_mat_rca01)))
colnames(mne_porter_df_rca) <- c("ind1", "ind2", "coagg_porter_rca01_mne")
local_porter_df_rca <- data.table(reshape2::melt(cor(local_mcp_mat_rca01, local_mcp_mat_rca01)))
colnames(local_porter_df_rca) <- c("ind1", "ind2", "coagg_porter_rca01_local")
mixed_porter_df_rca <- data.table(reshape2::melt(cor(mne_mat2_mixed, local_mat2_mixed)))
colnames(mixed_porter_df_rca) <- c("ind1", "ind2", "coagg_porter_rca01_mixed")



# relatedness-like normalized coagglomeration
mpp_df <- mpp_from_mcp(mcp_mat, normalized = TRUE, return_matrix = FALSE)
colnames(mpp_df) <- c("ind1", "ind2", "coagg_mat")
mne_mpp_df <- mpp_from_mcp(mne_mcp_mat, normalized = TRUE, return_matrix = FALSE)
colnames(mne_mpp_df) <- c("ind1", "ind2", "coagg_mat_mne")
local_mpp_df <- mpp_from_mcp(local_mcp_mat, normalized = TRUE, return_matrix = FALSE)
colnames(local_mpp_df) <- c("ind1", "ind2", "coagg_mat_local")
ml_mpp_mat <- as.matrix(coo_mixed_normalized(mne_mat1_mixed, local_mat1_mixed, assoc))
colnames(ml_mpp_mat) <- colnames(mne_mat1_mixed)
rownames(ml_mpp_mat) <- colnames(mne_mat1_mixed)
mixed_mpp_df <- data.table(reshape2::melt(ml_mpp_mat))
colnames(mixed_mpp_df) <- c("ind1", "ind2", "coagg_mat_mixed")



# combine -- df1
df1 <- merge(
  egk_df,
  egk_mne_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  egk_local_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  porter_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mne_porter_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  local_porter_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mixed_porter_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  porter_df_rca,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mne_porter_df_rca,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  local_porter_df_rca,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mixed_porter_df_rca,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mpp_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mne_mpp_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  local_mpp_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df1 <- merge(
  df1,
  mixed_mpp_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)





###### 03 -- independent variables ######

# labor flow
labor_hun <- fread(paste0("../outputs/01_sr_full_2015_", focal_year, "_undirected.csv"))
labor_hun <- select(labor_hun, -eid)
labor_swe <- fread("../outputs/01_sr_full_swe_2013_2019_undirected.csv")


# IO connections
io_hun_2d <- fread(paste0("../outputs/02_io_full_2015_", focal_year, "_2digit_undirected.csv"))
io_hun_2d <- select(io_hun_2d, -eid) %>% rename(io_norm2 = io_norm)
io_hun_3d <- fread(paste0("../outputs/02_io_full_2015_", focal_year, "_3digit_undirected.csv"))
io_hun_3d <- select(io_hun_3d, -eid) %>% rename(io_norm3 = io_norm)
io_swe <- fread("../outputs/02_io_full_swe_2digit_undirected.csv")
io_swe <- io_swe %>% select(-eid) %>% rename(swe_io_norm = io_norm)


# WIOT database
wiot <- fread("../data/wiot_edgelist_2_digit.csv")
wiot_hun <- wiot_for_country(wiot, selected_country = "HUN") %>%
  rename(io_wiot_hun = iv_io_norm)


# nr_firms as weights
nr_firms_in_industry <- firms %>%
  group_by(nace3d) %>%
  summarise(nr_firms = n_distinct(azonosito)) %>%
  rename(ind = nace3d) %>%
  data.table()


# add 2-digit industry codes to df1 for MERGE
df1$ind1_2d <- df1$ind1 %/% 10
df1$ind2_2d <- df1$ind2 %/% 10


# combine -- df2
df2 <- merge(
  df1,
  labor_hun,
  by.x = c("ind1", "ind2"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  labor_swe,
  by.x = c("ind1", "ind2"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  io_hun_2d,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  io_hun_3d,
  by.x = c("ind1", "ind2"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  wiot_hun,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  io_swe,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  nr_firms_in_industry,
  by.x = "ind1",
  by.y = "ind",
  all.x = TRUE,
  all.y = FALSE
)
df2 <- merge(
  df2,
  nr_firms_in_industry,
  by.x = "ind2",
  by.y = "ind",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("1", "2")
)





summary(m1 <- lm(egk_coagg ~ sr_norm, data = df2))
summary(m2 <- lm(coagg_porter_emp ~ sr_norm, data = df2))
summary(m3 <- lm(egk_coagg ~ io_norm3 + sr_norm, data = df2))
summary(m4 <- lm(coagg_porter_emp ~ io_norm3 + sr_norm, data = df2))





###### 04 -- instrumental variables ######

# 3 digit US supply data
us_supply <- fread("../data/us_supply_3digit_nace_nace.csv") %>%
  select(ind1, ind2, iv_io_norm) %>%
  rename(iv_us_supply_norm = iv_io_norm) %>%
  data.table()


# WIOT IVs
wiot <- fread("../data/wiot_edgelist_2_digit.csv")

# WIOT mean
wiot_mean <- wiot %>%
  filter(c_code != "HUN") %>%
  group_by(ind1, ind2) %>%
  summarise(iv_wiot_mean = mean(iv_io_norm)) %>%
  data.table()


# WIOT selected countries - USA / CZE / SWE
wiot_usa <- wiot_for_country(wiot, selected_country = "USA")
colnames(wiot_usa) <- c("ind1", "ind2", "iv_wiot_usa")
wiot_swe <- wiot_for_country(wiot, selected_country = "SWE")
colnames(wiot_swe) <- c("ind1", "ind2", "iv_wiot_swe")
wiot_cze <- wiot_for_country(wiot, selected_country = "CZE")
colnames(wiot_cze) <- c("ind1", "ind2", "iv_wiot_cze")


# combine -- df3
df3 <- merge(
  df2,
  us_supply,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df3 <- merge(
  df3,
  wiot_mean,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df3 <- merge(
  df3,
  wiot_usa,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df3 <- merge(
  df3,
  wiot_swe,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
df3 <- merge(
  df3,
  wiot_cze,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)




# export
write.table(df3,
            paste0("../outputs/04d_data_single_plant_", output_reg, "_", focal_year, ".csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)








### tables of OC ###

df3 <- fread(paste0("../outputs/04d_data_single_plant_", output_reg, "_", focal_year, ".csv"))

oc_df3 <- df3 %>%
  dplyr::select(
    -sr_norm,
    -swe_sr_norm,
    -io_norm2,
    -io_norm3,
    -io_wiot_hun,
    -swe_io_norm,
    -nr_firms1,
    -nr_firms2,
    -iv_us_supply_norm,
    -iv_wiot_mean,
    -iv_wiot_usa,
    -iv_wiot_swe,
    -iv_wiot_cze
  ) %>%
  data.table()



# export
write.table(oc_df3,
            paste0("../outputs/oc_gszr_update/04d_oc_data_single_plant_", output_reg, "_", focal_year, ".csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)


