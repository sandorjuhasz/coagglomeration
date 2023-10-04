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
version <- ""


# wiot dataset
iv_el <- fread("../outputs/wiot_edgelist_2_digit.csv")


em <- list()
pm <- list()
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
  
  
  # drop rows with MNE / domestic NAs -- create an equal sample across different models
  mdf3 <- subset(mdf3, is.na(coagg_porter_mne)==0 & is.na(coagg_porter_local)==0)
  
  
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
  summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3))
  summary(iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3))
  
  civ_egk <- coeftest(iv_egk, vcov = vcovCL, cluster = ~ind_pair_id)
  civ_porter <- coeftest(iv_porter, vcov = vcovCL, cluster = ~ind_pair_id)
  stargazer(civ_egk, civ_porter,
            dep.var.labels = c("EGK | coagg Porter"),
            out = paste0("../outputs/regression_tables/", version, "local_iv_", year, "_", region_codes[r], ".html"))
}


stargazer(em[[1]],
          em[[2]],
          em[[3]],
          pm[[1]],
          pm[[2]],
          pm[[3]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          covariate.labels = c("IO connections", "Labor flow"),
          out = paste0("../outputs/regression_tables/", version, "egk_porter_baseline_", year, "_", region_codes[r], ".html"))

