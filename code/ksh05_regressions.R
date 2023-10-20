### COAGGLOMERATION project -- data preparation for regressions ###
# by sandorjuhasz



library(data.table)
library(dplyr)
library(stargazer)
library(ivreg)
library(lmtest)
library(sandwich)
library(interplot)
library(cowplot)
source("../scripts/00_functions.R")



# parameters
focal_year <- 2017

#region_level <- "nuts3"
region_level <- "nuts4"
#region_level <- "city"

version <- ""
#version <- "full"
#version <- "manuf"
#version <- "serv"
#version <- "BP_out"
#version <- "synthetic"



# read data
reg_df <- fread(paste0("../outputs/04_data_", region_level, "_", focal_year, ".csv"))
#reg_df <- fread(paste0("../outputs/04b_data_budapest_excluded_", region_level, "_", focal_year, ".csv"))
#reg_df <- fread(paste0("../outputs/04c_data_syn_MNEs_", region_level, "_", focal_year, ".csv"))
#reg_df <- fread(paste0("../outputs/04d_data_single_plant_", region_level, "_", focal_year, ".csv"))


# manufacturing focus?
if(version=="manuf")
{
  reg_df <- subset(
    reg_df,
    (ind1 >= 100 & ind2 >= 100 & ind1 < 300 & ind2 < 360)
  )
}

# manufacturing focus?
if(version=="serv")
{
  reg_df <- subset(
    reg_df,
    (ind1 >= 490 & ind2 >= 490 & ind1 < 960 & ind2 < 960)
  )
}




# remove self loops and repeated pairs
print(nrow(reg_df))
reg_df <- subset(reg_df, ind1 < ind2)
print(nrow(reg_df))



# variable manipulation -- nr firms in industries
reg_df$log_nr_firms1 <- log10(reg_df$nr_firms1)
reg_df$log_nr_firms2 <- log10(reg_df$nr_firms2)

# IO and labor natural dummyies
reg_df$io01 <- ifelse(reg_df$io_norm3 > 0, 1, 0)
reg_df$labor01 <- ifelse(reg_df$sr_norm > 0, 1, 0)

# industry pair IDs
reg_df$ind_pair_id <- paste0(reg_df$ind1, "_", reg_df$ind2)



# standardization -- dependent variables
reg_df$egk_coagg_stand <- scale(reg_df$egk_coagg)
reg_df$egk_coagg_mne_stand <- scale(reg_df$egk_coagg_mne)
reg_df$egk_coagg_local_stand <- scale(reg_df$egk_coagg_local)

reg_df$coagg_porter_emp_stand <- scale(reg_df$coagg_porter_emp)
reg_df$coagg_porter_emp_mne_stand <- scale(reg_df$coagg_porter_emp_mne)
reg_df$coagg_porter_emp_local_stand <- scale(reg_df$coagg_porter_emp_local)
reg_df$coagg_porter_emp_mixed_stand <- scale(reg_df$coagg_porter_emp_mixed)

reg_df$coagg_porter_rca01_stand <- scale(reg_df$coagg_porter_rca01)
reg_df$coagg_porter_rca01_mne_stand <- scale(reg_df$coagg_porter_rca01_mne)
reg_df$coagg_porter_rca01_local_stand <- scale(reg_df$coagg_porter_rca01_local)
reg_df$coagg_porter_rca01_mixed_stand <- scale(reg_df$coagg_porter_rca01_mixed)

reg_df$coagg_mat_stand <- scale(reg_df$coagg_mat)
reg_df$coagg_mat_mne_stand <- scale(reg_df$coagg_mat_mne)
reg_df$coagg_mat_local_stand <- scale(reg_df$coagg_mat_local)
reg_df$coagg_mat_mixed_stand <- scale(reg_df$coagg_mat_mixed)



# standardization -- independent variables
reg_df$io2_stand <- scale(reg_df$io_norm2)
reg_df$io3_stand <- scale(reg_df$io_norm3)
reg_df$lab_stand <- scale(reg_df$sr_norm)
reg_df$io_wiot_hun_stand <- scale(reg_df$io_wiot_hun)



# standardization -- instrumental variables
reg_df$iv_swe_io_stand <- scale(reg_df$swe_io_norm)
reg_df$iv_swe_lab_stand <- scale(reg_df$swe_sr_norm)
reg_df$iv_us_supply_stand <- scale(reg_df$iv_us_supply_norm)
reg_df$iv_wiot_mean_stand <- scale(reg_df$iv_wiot_mean)
reg_df$iv_wiot_usa_stand <- scale(reg_df$iv_wiot_usa)
reg_df$iv_wiot_swe_stand <- scale(reg_df$iv_wiot_swe)
reg_df$iv_wiot_cze_stand <- scale(reg_df$iv_wiot_cze)

 

# to have an equal setting -- drop rows with NAs in MNE related variables
print(nrow(reg_df))
reg_df <- subset(reg_df, is.na(coagg_porter_emp_mne) == 0 & is.na(coagg_porter_emp_local) == 0)
print(nrow(reg_df))



# baseline models -- EGK
summary(egk_m00 <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = reg_df))
summary(cp_m00 <- lm(coagg_porter_emp_stand ~ io3_stand + lab_stand, data = reg_df))

summary(egk_ivm00 <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
summary(cp_ivm00 <- ivreg::ivreg(coagg_porter_emp_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))


summary(egk_m01 <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(cp_m01 <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
#summary(cp_m01 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))

summary(egk_ivm01 <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
summary(cp_ivm01 <- ivreg::ivreg(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
#summary(cp_ivm02 <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))


summary(egk_ivm02 <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df))
summary(cp_ivm02 <- ivreg::ivreg(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df))
#summary(cp_ivm02 <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df))

summary(egk_m03 <- lm(egk_coagg_stand ~ io2_stand + lab_stand, data = reg_df))
summary(cp_m03 <- lm(coagg_porter_emp_stand ~ io2_stand + lab_stand, data = reg_df))

summary(egk_ivm02 <- ivreg::ivreg(egk_coagg_stand ~ io2_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
summary(cp_ivm02 <- ivreg::ivreg(coagg_porter_emp_stand ~ io2_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))







### interactions ###
summary(i_egk <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(i_cp <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))



# create q75 vars
summary(i01_egk <- lm(egk_coagg_stand ~ io01 * labor01, data = reg_df))
summary(i01_cpe <- lm(coagg_porter_emp_stand ~ io01 * labor01, data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ io01 * labor01, data = reg_df))




### dummy interaction -- raw numbers
crosstab <- data.table(table(reg_df$io01, reg_df$labor01))
#nrow(subset(reg_df, io_q75 == 0 & lab_01 == 1))
colnames(crosstab) <- c("io_ties", "lab_ties", "nr_indind")

temp00 <- subset(reg_df, io01 == 0 & labor01 == 0)
mean00_egk <- mean(temp00$egk_coagg)
mean00_porter <- mean(temp00$coagg_porter_emp)

temp10 <- subset(reg_df, io01 == 1 & labor01 == 0)
mean10_egk <- mean(temp10$egk_coagg)
mean10_porter <- mean(temp10$coagg_porter_emp)

temp01 <- subset(reg_df, io01 == 0 & labor01 == 1)
mean01_egk <- mean(temp01$egk_coagg)
mean01_porter <- mean(temp01$coagg_porter_emp)

temp11 <- subset(reg_df, io01 == 1 & labor01 == 1)
mean11_egk <- mean(temp11$egk_coagg)
mean11_porter <- mean(temp11$coagg_porter_emp)

crosstab$mean_egk <- c(mean00_egk, mean10_egk, mean01_egk, mean11_egk)
crosstab$mean_porter <- c(mean00_porter, mean10_porter, mean01_porter, mean11_porter)






### MNE / domestic versions


# baseline models
summary(egk_mm01 <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = reg_df))
summary(egk_mm02 <- lm(egk_coagg_mne_stand ~ io3_stand + lab_stand, data = reg_df))
summary(egk_mm03 <- lm(egk_coagg_local_stand ~ io3_stand + lab_stand, data = reg_df))

summary(egk_mm01 <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(egk_mm02 <- lm(egk_coagg_mne_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(egk_mm03 <- lm(egk_coagg_local_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))


summary(cp_mm01 <- lm(coagg_porter_emp_stand ~ io3_stand + lab_stand, data = reg_df))
summary(cp_mm02_mne <- lm(coagg_porter_emp_mne_stand ~ io3_stand + lab_stand, data = reg_df))
summary(cp_mm03_local <- lm(coagg_porter_emp_local_stand ~ io3_stand + lab_stand, data = reg_df))
summary(cp_mm04_mixed <- lm(coagg_porter_emp_mixed_stand ~ io3_stand + lab_stand, data = reg_df))

summary(cp_mm01 <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(cp_mm02_mne <- lm(coagg_porter_emp_mne_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(cp_mm03_local <- lm(coagg_porter_emp_local_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(cp_mm04_mixed <- lm(coagg_porter_emp_mixed_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))



# MNE / domestic interactions
summary(egk_im01 <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(egk_im02 <- lm(egk_coagg_mne_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(egk_im03 <- lm(egk_coagg_local_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))


summary(cp_im01 <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(cp_im02_mne <- lm(coagg_porter_emp_mne_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(cp_im03_local <- lm(coagg_porter_emp_local_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(cp_im04_mixed <- lm(coagg_porter_emp_mixed_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))


summary(cp_im01 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(cp_im02_mne <- lm(coagg_porter_rca01_mne_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(cp_im03_local <- lm(coagg_porter_rca01_local_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(cp_im04_mixed <- lm(coagg_porter_rca01_mixed_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))






