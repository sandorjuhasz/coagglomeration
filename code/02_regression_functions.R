### COAGGLOMERATION project -- functions to support regressions in the paper ###
# by sandorjuhasz



library(data.table)
library(dplyr)
library(stargazer)
library(ivreg)
library(lmtest)
library(sandwich)
library(interplot)
library(cowplot)



prep_baseline_regression_table <- function(path)
{
  # dataframe from OC
  reg_df <- fread(path)
  
  # keep edges only once
  reg_df <- subset(reg_df, ind1 < ind2)
  
  # variable manipulation -- nr firms in industries -- only inside Databank
  #reg_df$log_nr_firms1 <- log10(reg_df$nr_firms1)
  #reg_df$log_nr_firms2 <- log10(reg_df$nr_firms2)
  
  # IO and labor natural dummies
  reg_df$io01 <- ifelse(reg_df$io_norm3 > 0, 1, 0)
  reg_df$io_wiot01 <- ifelse(reg_df$io_wiot_hun > 0, 1, 0)
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
  #reg_df <- subset(reg_df, is.na(coagg_porter_emp_mne) == 0 & is.na(coagg_porter_emp_local) == 0)
  
  return(reg_df)
}



manufacturing_filter <- function(data)
{
  data <- subset(
    data,
    (ind1 >= 100 & ind2 >= 100 & ind1 < 300 & ind2 < 360)
  )
  return(data)
}



services_filter <- function(data)
{
  data <- subset(
    data,
    (ind1 >= 490 & ind2 >= 490 & ind1 < 960 & ind2 < 960)
  )
  return(data)
}



prep_alternative_table <- function(path_full_table, path_alt_dep_var_table)
{
  # read in both
  reg_df <- fread(path_full_table)
  alt_df <- fread(path_alt_dep_var_table)
  
  # remove overlapping columns -- both NOT ind1, ind2 used for merge
  remove_cols <- colnames(alt_df)
  join_cols <- c("ind1", "ind2")
  remove_cols <- setdiff(remove_cols, join_cols)
  reg_df <- dplyr::select(reg_df, -remove_cols) %>% data.table()
  
  reg_df <- merge(
    reg_df,
    alt_df,
    by = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  # keep edges only once
  reg_df <- subset(reg_df, ind1 < ind2)
  
  # variable manipulation -- nr firms in industries -- only inside Databank
  #reg_df$log_nr_firms1 <- log10(reg_df$nr_firms1)
  #reg_df$log_nr_firms2 <- log10(reg_df$nr_firms2)
  
  # IO and labor natural dummies
  reg_df$io01 <- ifelse(reg_df$io_norm3 > 0, 1, 0)
  reg_df$io_wiot01 <- ifelse(reg_df$io_wiot_hun > 0, 1, 0)
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
  # reg_df <- subset(reg_df, is.na(coagg_porter_emp_mne) == 0 & is.na(coagg_porter_emp_local) == 0)
  
  return(reg_df)
}




prep_symmetric_regression_table <- function(path)
{
  # dataframe from OC
  reg_df <- fread(path)
  
  # keep edges only once
  
  # variable manipulation -- nr firms in industries -- only inside Databank
  #reg_df$log_nr_firms1 <- log10(reg_df$nr_firms1)
  #reg_df$log_nr_firms2 <- log10(reg_df$nr_firms2)
  
  # IO and labor natural dummies
  reg_df$io01 <- ifelse(reg_df$io_norm3 > 0, 1, 0)
  reg_df$io_wiot01 <- ifelse(reg_df$io_wiot_hun > 0, 1, 0)
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
  #reg_df <- subset(reg_df, is.na(coagg_porter_emp_mne) == 0 & is.na(coagg_porter_emp_local) == 0)
  
  return(reg_df)
}


