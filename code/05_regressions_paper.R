# regression tables for the paper -- sandorjuhasz


library(data.table)
library(dplyr)
library(stargazer)
library(ivreg)
library(lmtest)
library(sandwich)
library(interplot)
library(cowplot)
#source("../scripts/m00_functions.R")


# parameters
region_codes <- c("nuts3", "nuts4", "city")
ind <- 3
year <- 2017
version <- "us_supply_iv_"


# wiot dataset
iv_el <- fread("../outputs/wiot_edgelist_2_digit.csv")
iv_us <- fread("../outputs/us_supply_3digit_nace_nace.csv")




### --- table 1 OLS and table 2 IV

em <- list()
pm <- list()
ive <- list()
ivp <- list()
for(r in 1:length(region_codes)){
  # file from OC
  mdf3 <- fread(paste0("../data/oc11_2023_oct/oc_mdf3_", region_codes[r], "_", year, "_based.csv"), sep = ";")
  
  
  # remove self loops and repeated pairs
  mdf3 <- subset(mdf3, ind1 < ind2)
  
  
  # variable manipulation -- for regressions
  mdf3$egk_coagg <- scale(mdf3$egk_coagg)
  mdf3$coagg_porter <- scale(mdf3$coagg_porter)
  mdf3$coagg_porter_mne <- scale(mdf3$coagg_porter_mne)
  mdf3$coagg_porter_local <- scale(mdf3$coagg_porter_local)
  mdf3$coagg_porter_mne_local <- scale(mdf3$coagg_porter_mne_local)
  mdf3$lab_standard <- scale(mdf3$sr_norm)
  mdf3$ind1_2d <- mdf3$ind1 %/% 10
  mdf3$ind2_2d <- mdf3$ind2 %/% 10
  mdf3$swe_lab_standard <- scale(mdf3$swe_sr_norm)
  mdf3$ind_pair_id <- paste0(mdf3$ind1, "_", mdf3$ind2)
  
  # add WIOT HUN data
  hun_wiot <- fread("../outputs/wiot_edgelist_2_digit.csv") %>%
    filter(c_code == "HUN") %>%
    dplyr::select(ind1, ind2, iv_io_norm) %>%
    rename(io_norm_wiot = iv_io_norm) %>%
    data.table()
  
  mdf3 <- merge(
    mdf3,
    hun_wiot,
    by.x = c("ind1_2d", "ind2_2d"),
    by.y = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  mdf3$io_standard <- scale(mdf3$io_norm_wiot)
  
  
  # add US supply table
  mdf3 <- merge(
    mdf3,
    dplyr::select(iv_us, ind1, ind2, iv_io_norm) %>% rename(iv_us_supply = iv_io_norm),
    by = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  # drop rows with MNE / domestic NAs -- create an equal sample across different models
  mdf3 <- subset(mdf3, is.na(coagg_porter_mne)==0 & is.na(coagg_porter_local)==0)
  #mdf3 <- subset(mdf3, ind1_2d != ind2_2d)
  
  # list of IVs -- first element with ind IDs
  iv_df <- dplyr::select(subset(iv_el, c_code == "USA"), ind1, ind2, iv_io_norm)
  colnames(iv_df) <- c("ind1", "ind2", "USA")
  
  mdf3 <- merge(
    mdf3,
    iv_df,
    by.x = c("ind1_2d", "ind2_2d"),
    by.y = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  mdf3$iv_io_standard <- scale(mdf3$USA)
  
  # baseline models -- EGK and Porter
  em[[r]] <- lm(egk_coagg ~ io_standard + lab_standard, data = mdf3)
  pm[[r]] <- lm(coagg_porter ~ io_standard + lab_standard, data = mdf3)
  
  
  # iv models
  #iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3)
  #iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3)
  iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | iv_us_supply + swe_lab_standard, data = mdf3)
  iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | iv_us_supply + swe_lab_standard, data = mdf3)
  
  
  ive[[r]] <- coeftest(iv_egk, vcov = vcovCL, cluster = ~ind_pair_id)
  ivp[[r]] <- coeftest(iv_porter, vcov = vcovCL, cluster = ~ind_pair_id)
}


stargazer(em[[1]],
          em[[2]],
          em[[3]],
          pm[[1]],
          pm[[2]],
          pm[[3]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (corr)"),
          covariate.labels = c("IO connections", "Labor flow"),
          out = paste0("../outputs/regression_tables/", version, "egk_porter_baseline_", year, "_", region_codes[r], ".html"))


stargazer(ive[[1]],
          ive[[2]],
          ive[[3]],
          ivp[[1]],
          ivp[[2]],
          ivp[[3]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (corr)"),
          covariate.labels = c("IO connections", "Labor flow"),
          out = paste0("../outputs/regression_tables/", version, "iv_egk_porter_baseline_", year, "_", region_codes[r], ".html"))




### --- SI -- first stage IV (and related descriptives)




### --- SI -- transaction data OLS




### --- SI -- single plant firms OLS




### --- SI -- synthetic MNE OLS




### --- SI -- without Budapest OLS




