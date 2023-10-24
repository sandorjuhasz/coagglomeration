# regression tables for the paper -- sandorjuhasz
# setup is the same as in the Databank



library(data.table)
library(dplyr)
library(stargazer)
library(ivreg)
library(lmtest)
library(sandwich)
library(interplot)
library(cowplot)
source("../code/04_functions_for_regressions.R")



# parameters
focal_year <- 2017

region_level <- "nuts3"
#region_level <- "nuts4"
#region_level <- "city"

version <- ""
#version <- "budapest_excluded_"
#version <- "syn_MNEs_"
#version <- "single_plant_"
#version <- "manuf"
#version <- "serv"



### --- baseline setting for local tests
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

summary(em <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))



### --- table 1 OLS and table 2 IV

region_codes <- c("nuts3", "nuts4", "city")
em <- list()
pm <- list()
ive <- list()
ivp <- list()
for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  
  # baseline models -- EGK and Porter
  em[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df)
  pm[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df)
  
  
  # iv models
  iv_em <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df)
  iv_pm <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df)
  
  
  ive[[r]] <- coeftest(iv_em, vcov = vcovCL, cluster = ~ind_pair_id)
  ivp[[r]] <- coeftest(iv_pm, vcov = vcovCL, cluster = ~ind_pair_id)
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
          out = paste0("../outputs/regression_tables/01_ols.html"))


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
          out = paste0("../outputs/regression_tables/02_iv.html"))




### --- table 3 OLS with interactions






### --- SI -- first stage IV (and related descriptives)




### --- SI -- transaction data OLS




### --- SI -- single plant firms OLS




### --- SI -- synthetic MNE OLS




### --- SI -- without Budapest OLS




