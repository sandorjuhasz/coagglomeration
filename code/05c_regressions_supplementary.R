# regression tables for the paper -- sandorjuhasz
# setup is the same as in the Databank



library(data.table)
library(dplyr)
library(tidyr)
library(stargazer)
library(ivreg)
library(lmtest)
library(sandwich)
library(interplot)
library(cowplot)
source("../code/05a_regression_functions.R")



# parameters
focal_year <- 2017

#region_level <- "nuts3"
region_level <- "nuts4"
#region_level <- "city"

version <- ""
#version <- "budapest_excluded_"
#version <- "syn_MNEs_"
#version <- "single_plant_"
#version <- "manuf"
#version <- "serv"








### --- 00 -- setting for city level



### -- 00 -- dummy level interactions



### -- 00 -- interaction models based on NUTS3 level data


### --- Table 3 OLS with interactions NUTS3 level
region_codes <- c("nuts3")
version <- c("")
ei <- list()
eif <- list()
eit <- list()
eitf <- list()
eic <- list()
eifc <- list()
eitc <- list()
eitfc <- list()

pi <- list()
pif <- list()
pit <- list()
pitf <- list()
pic <- list()
pifc <- list()
pitc <- list()
pitfc <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc15_2023_dec/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  
  if(version != ""){
    reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  }
  
  # baseline models -- EGK and Porter
  ei[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  eit[[r]] <- lm(egk_coagg_stand ~ io3_stand * lab_stand, data = reg_df)
  pi[[r]] <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  pit[[r]] <- lm(coagg_porter_emp_stand ~ io3_stand * lab_stand, data = reg_df)
  eif[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pif[[r]] <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  eitf[[r]] <- lm(egk_coagg_stand ~ io3_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pitf[[r]] <- lm(coagg_porter_emp_stand ~ io3_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  
  eic[[r]] <- coeftest(ei[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  eitc[[r]] <- coeftest(eit[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pic[[r]] <- coeftest(pi[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pitc[[r]] <- coeftest(pit[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  
  eifc[[r]] <- coeftest(eif[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  eitfc[[r]] <- coeftest(eitf[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pifc[[r]] <- coeftest(pif[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pitfc[[r]] <- coeftest(pitf[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  
}  


stargazer(
  ei[[1]],
  eif[[1]],
  eitf[[1]],
  pi[[1]],
  pif[[1]],
  pitf[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3", version, ".tex")
  out = paste0("../outputs/regression_tables/si_interactions_nuts3", version, ".html")
)

stargazer(
  eic[[1]],
  eifc[[1]],
  eitfc[[1]],
  pic[[1]],
  pifc[[1]],
  pitfc[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)





### --- 01 -- Stepwise IV models

### IV stepwise version -- LABOR
region_codes <- c("nuts3", "nuts4")

# NUTS3 version
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[1], "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# first stage
summary(s1 <- lm(lab_stand ~ iv_swe_lab_stand, data = reg_df))
reg_df$first_stage_pred <- predict(s1, newdata = reg_df)
cor(reg_df$lab_stand, reg_df$first_stage_pred)

# second stage
summary(s2egkn3 <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))
summary(s2pn3 <- lm(coagg_porter_rca01_stand ~ first_stage_pred, data = reg_df))
#summary(iv_egk <- ivreg::ivreg(egk_coagg_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))
#summary(iv_p <- ivreg::ivreg(coagg_porter_rca01_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))


# NUTS4 version
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[2], "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# first stage -- should be the same
summary(s1 <- lm(lab_stand ~ iv_swe_lab_stand, data = reg_df))
reg_df$first_stage_pred <- predict(s1, newdata = reg_df)
cor(reg_df$lab_stand, reg_df$first_stage_pred)

# second stage
summary(s2egkn4 <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))
summary(s2pn4 <- lm(coagg_porter_rca01_stand ~ first_stage_pred, data = reg_df))


stargazer(
  s1,
  s2egkn3,
  s2egkn4,
  s2pn3,
  s2pn4,
  #column.labels = c("First stage", "Second stage"),
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("Labor flow SWE", "Labor flow pred"),
  out = paste0("../outputs/regression_tables/si_iv_stepwise_labor.html")
)




### IV stepwise version -- WIOD
region_codes <- c("nuts3", "nuts4")

# NUTS3 version
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[1], "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# first stage
summary(s1 <- lm(io_wiot_hun_stand ~ iv_wiot_mean_stand, data = reg_df))
reg_df$first_stage_pred <- predict(s1, newdata = reg_df)
cor(reg_df$io_wiot_hun_stand, reg_df$first_stage_pred)

# second stage
summary(s2egkn3 <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))
summary(s2pn3 <- lm(coagg_porter_rca01_stand ~ first_stage_pred, data = reg_df))
#summary(iv_egk <- ivreg::ivreg(egk_coagg_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))
#summary(iv_p <- ivreg::ivreg(coagg_porter_rca01_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))


# NUTS4 version
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[2], "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# first stage -- should be the same
summary(s1 <- lm(io_wiot_hun_stand ~ iv_wiot_mean_stand, data = reg_df))
reg_df$first_stage_pred <- predict(s1, newdata = reg_df)
cor(reg_df$io_wiot_hun_stand, reg_df$first_stage_pred)

# second stage
summary(s2egkn4 <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))
summary(s2pn4 <- lm(coagg_porter_rca01_stand ~ first_stage_pred, data = reg_df))


stargazer(
  s1,
  s2egkn3,
  s2egkn4,
  s2pn3,
  s2pn4,
  #column.labels = c("First stage", "Second stage"),
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO (WIOD mean)", "IO pred"),
  out = paste0("../outputs/regression_tables/si_iv_stepwise_io_wiod.html")
)



### IV stepwise version -- transactions
region_codes <- c("nuts3", "nuts4")

# NUTS3 version
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[1], "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# first stage
summary(s1 <- lm(io3_stand ~ iv_wiot_mean_stand, data = reg_df))
reg_df$first_stage_pred <- predict(s1, newdata = reg_df)
cor(reg_df$io3_stand, reg_df$first_stage_pred)

# second stage
summary(s2egkn3 <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))
summary(s2pn3 <- lm(coagg_porter_rca01_stand ~ first_stage_pred, data = reg_df))
#summary(iv_egk <- ivreg::ivreg(egk_coagg_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))
#summary(iv_p <- ivreg::ivreg(coagg_porter_rca01_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))


# NUTS4 version
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[2], "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# first stage -- should be the same
summary(s1 <- lm(io3_stand ~ iv_wiot_mean_stand, data = reg_df))
reg_df$first_stage_pred <- predict(s1, newdata = reg_df)
cor(reg_df$io3_stand, reg_df$first_stage_pred)

# second stage
summary(s2egkn4 <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))
summary(s2pn4 <- lm(coagg_porter_rca01_stand ~ first_stage_pred, data = reg_df))


stargazer(
  s1,
  s2egkn3,
  s2egkn4,
  s2pn3,
  s2pn4,
  #column.labels = c("First stage", "Second stage"),
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO (WIOD mean)", "IO pred"),
  out = paste0("../outputs/regression_tables/si_iv_stepwise_io_transactions.html")
)




### --- 01 -- Alternative IV -- US supply

#region_codes <- c("nuts3", "nuts4", "city")
focal_year <- 2017
region_codes <- c("nuts3", "nuts4")
#version <- c("_same2digit_dropped")
version <- c("")
ie <- list()
ip <- list()
iet <- list()
ipt <- list()
iec <- list()
ipc <- list()
iect <- list()
ipct <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  
  # remove rows where 2-digit industries are the same
  if(version != ""){
    reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  }
  
  
  # iv models
  ie[[r]] <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df)
  ip[[r]] <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df)
  iet[[r]] <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df)
  ipt[[r]] <- ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_us_supply_stand + iv_swe_lab_stand, data = reg_df)
  
  iec[[r]] <- coeftest(ie[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  ipc[[r]] <- coeftest(ip[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  iect[[r]] <- coeftest(iet[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  ipct[[r]] <- coeftest(ipt[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  
}



# IV output
stargazer(iec[[1]],
          iec[[2]],
          iect[[1]],
          iect[[2]],
          #ipc[[1]],
          #ipc[[2]],
          ipct[[1]],
          ipct[[2]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          omit = c("ind1", "ind2"),
          #type="text")
          #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
          #covariate.labels = c("IO transactions", "Labor flow"),
          out = paste0("../outputs/regression_tables/si_iv_us_supply", ".html"))
#out = paste0("../outputs/regression_tables/02_iv", version[1], ".tex"))





### --- 01 -- Alternative IV -- CZE WIOD

#region_codes <- c("nuts3", "nuts4", "city")
focal_year <- 2017
region_codes <- c("nuts3", "nuts4")
#version <- c("_same2digit_dropped")
version <- c("")
ie <- list()
ip <- list()
iet <- list()
ipt <- list()
iec <- list()
ipc <- list()
iect <- list()
ipct <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  
  # remove rows where 2-digit industries are the same
  if(version != ""){
    reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  }
  
  
  # iv models
  ie[[r]] <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_cze_stand + iv_swe_lab_stand, data = reg_df)
  ip[[r]] <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_cze_stand + iv_swe_lab_stand, data = reg_df)
  iet[[r]] <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_cze_stand + iv_swe_lab_stand, data = reg_df)
  ipt[[r]] <- ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_wiot_cze_stand + iv_swe_lab_stand, data = reg_df)
  
  iec[[r]] <- coeftest(ie[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  ipc[[r]] <- coeftest(ip[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  iect[[r]] <- coeftest(iet[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  ipct[[r]] <- coeftest(ipt[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  
}



# IV output
stargazer(iec[[1]],
          iec[[2]],
          iect[[1]],
          iect[[2]],
          #ipc[[1]],
          #ipc[[2]],
          ipct[[1]],
          ipct[[2]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          omit = c("ind1", "ind2"),
          #type="text")
          #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
          #covariate.labels = c("IO transactions", "Labor flow"),
          out = paste0("../outputs/regression_tables/si_iv_cze_wiod2", ".html"))
#out = paste0("../outputs/regression_tables/02_iv", version[1], ".tex"))














### --- 01 -- setting for Budapest excluded regressions
# some of the tables needs manual adjustment and joining

path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
path2 <- paste0("../data/oc15_2023_dec/04b_oc_data_budapest_excluded_", region_level, "_", focal_year, ".csv")
bpe_df <- prep_alternative_table(path, path2)


# WIOD
summary(ew <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = bpe_df))
summary(ew_iv <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = bpe_df))
ew_cl <- coeftest(ew, vcov = vcovCL, cluster = ~ind_pair_id)
ew_iv_cl <- coeftest(ew_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pw <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = bpe_df))
summary(pw_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = bpe_df))
pw_cl <- coeftest(pw, vcov = vcovCL, cluster = ~ind_pair_id)
pw_iv_cl <- coeftest(pw_iv, vcov = vcovCL, cluster = ~ind_pair_id)



# transactions
summary(et <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = bpe_df))
summary(et_iv <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = bpe_df))
et_cl <- coeftest(et, vcov = vcovCL, cluster = ~ind_pair_id)
et_iv_cl <- coeftest(et_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pt <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = bpe_df))
summary(pt_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = bpe_df))
pt_cl <- coeftest(pt, vcov = vcovCL, cluster = ~ind_pair_id)
pt_iv_cl <- coeftest(pt_iv, vcov = vcovCL, cluster = ~ind_pair_id)



stargazer(
  ew_cl,
  et_cl,
  ew_iv_cl,
  et_iv_cl,
  #pw_cl,
  #pt_cl,
  #pw_iv_cl,
  #pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_bp_exclused_cse_p1", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)

stargazer(
  #ew_cl,
  #et_cl,
  #ew_iv_cl,
  #et_iv_cl,
  pw_cl,
  pt_cl,
  pw_iv_cl,
  pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_bp_exclused_cse_p2", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)











### --- 02 -- setting for single plant data regressions
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
path3 <- paste0("../data/oc15_2023_dec/04d_oc_data_single_plant_", region_level, "_", focal_year, ".csv")
single_df <- prep_alternative_table(path, path3)


# WIOD
summary(ew <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = single_df))
summary(ew_iv <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = single_df))
ew_cl <- coeftest(ew, vcov = vcovCL, cluster = ~ind_pair_id)
ew_iv_cl <- coeftest(ew_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pw <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = single_df))
summary(pw_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = single_df))
pw_cl <- coeftest(pw, vcov = vcovCL, cluster = ~ind_pair_id)
pw_iv_cl <- coeftest(pw_iv, vcov = vcovCL, cluster = ~ind_pair_id)



# transactions
summary(et <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = single_df))
summary(et_iv <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = single_df))
et_cl <- coeftest(et, vcov = vcovCL, cluster = ~ind_pair_id)
et_iv_cl <- coeftest(et_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pt <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = single_df))
summary(pt_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = single_df))
pt_cl <- coeftest(pt, vcov = vcovCL, cluster = ~ind_pair_id)
pt_iv_cl <- coeftest(pt_iv, vcov = vcovCL, cluster = ~ind_pair_id)



stargazer(
  ew_cl,
  et_cl,
  ew_iv_cl,
  et_iv_cl,
  #pw_cl,
  #pt_cl,
  #pw_iv_cl,
  #pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_single_plant_cse_p1", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)

stargazer(
  #ew_cl,
  #et_cl,
  #ew_iv_cl,
  #et_iv_cl,
  pw_cl,
  pt_cl,
  pw_iv_cl,
  pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_single_plant_cse_p2", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)











### --- 03 -- interplots with IO (transactions)
fontsize <- 25
region_codes <- c("nuts4")
model_version <- c("noFE", "FE")
path <- paste0("../data/oc15_2023_dec/04oc_data_", version, region_codes, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
#reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]


# the model -- EGK
int_egk <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = reg_df)

ip_a <- interplot(m = int_egk,
                  var1 = "sr_norm",
                  #var2 = "io_norm3",
                  var2 = "io_norm3",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A") +
  xlab("IO (transactions)") +
  ylab("Coeff. labor (SR)") +
  ylim(-0.125, 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm"))

ip_b <- interplot(m = int_egk,
                  #var1 = "io_wiot_hun",
                  var1 = "io_norm3",
                  var2 = "sr_norm",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A") +
  xlab("Labor (SR)") +
  ylab("Coeff. IO (transactions)") +
  ylim(-0.125, 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm"))


# the model -- LC (emp)
#int_pemp <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = reg_df)
int_pemp <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = reg_df)

ip_c <- interplot(m = int_pemp,
                  var1 = "sr_norm",
                  #var2 = "io_wiot_hun",
                  var2 = "io_norm3",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A") +
  xlab("IO (transactions)") +
  ylab("Coeff. labor (SR)") +
  ylim(0, 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))

ip_d <- interplot(m = int_pemp,
                  #var1 = "io_wiot_hun",
                  var1 = "io_norm3",
                  var2 = "sr_norm",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A") +
  xlab("Labor (SR)") +
  ylab("Coeff. IO (transactions)") +
  ylim(0, 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))


# combine subplots
subplots1 <- plot_grid(
  ip_a, ip_b,
  labels = c("A", "B"), label_size = fontsize, ncol = 2
)
subplots2 <- plot_grid(
  ip_c, ip_d,
  labels = c("C", "D"), label_size = fontsize, ncol = 2
)


# titles
plot_title1 <- ggdraw() + draw_label("Coagglomeration (EGK)", fontface="bold", size = fontsize)
plot_title2 <- ggdraw() + draw_label("Coagglomeration (LC)", fontface="bold", size = fontsize)


# combined version
title <- paste0("si_interplots_transactions_", model_version[1])
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=1000, height=750, units = 'px')
plot_grid(plot_title1, subplots1, plot_title2, subplots2, ncol=1, rel_heights=c(0.1, 1, 0.1, 1))
dev.off()







### --- 01 -- manufacturing only
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
version <- "_manuf"

# filter for manufacturing only
manuf <- c(10, 34)
manuf_df <- subset(
  reg_df,
  (ind1_2d >= manuf[1]) &
  (ind1_2d < manuf[2]) &
  (ind2_2d >= manuf[1]) &
  (ind2_2d < manuf[2])
)


# WIOD
summary(ew <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = manuf_df))
summary(ew_iv <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = manuf_df))
ew_cl <- coeftest(ew, vcov = vcovCL, cluster = ~ind_pair_id)
ew_iv_cl <- coeftest(ew_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pw <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = manuf_df))
summary(pw_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = manuf_df))
pw_cl <- coeftest(pw, vcov = vcovCL, cluster = ~ind_pair_id)
pw_iv_cl <- coeftest(pw_iv, vcov = vcovCL, cluster = ~ind_pair_id)



# transactions
summary(et <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = manuf_df))
summary(et_iv <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = manuf_df))
et_cl <- coeftest(et, vcov = vcovCL, cluster = ~ind_pair_id)
et_iv_cl <- coeftest(et_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pt <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = manuf_df))
summary(pt_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = manuf_df))
pt_cl <- coeftest(pt, vcov = vcovCL, cluster = ~ind_pair_id)
pt_iv_cl <- coeftest(pt_iv, vcov = vcovCL, cluster = ~ind_pair_id)



stargazer(
  ew_cl,
  et_cl,
  ew_iv_cl,
  et_iv_cl,
  #pw_cl,
  #pt_cl,
  #pw_iv_cl,
  #pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_egk", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)

stargazer(
  #ew_cl,
  #et_cl,
  #ew_iv_cl,
  #et_iv_cl,
  pw_cl,
  pt_cl,
  pw_iv_cl,
  pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_porter", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)



stargazer(ew, type="text")
stargazer(pw, type="text")
stargazer(et, type="text")
stargazer(pt, type="text")


summary(int_ew <- lm(egk_coagg_stand ~ io_wiot_hun * sr_norm, data = manuf_df))
summary(int_ew <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = manuf_df))

interplot(m = int_ew,
          #var1 = "io_wiot_hun",
          var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_ew,
          var1 = "sr_norm",
          var2 = "io_norm3",
          #var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 

summary(int_pw <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = manuf_df))
interplot(m = int_pw,
          var1 = "io_wiot_hun",
          #var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_pw,
          var1 = "sr_norm",
          #var2 = "io_norm3",
          var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 

summary(int_pw <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = manuf_df))
interplot(m = int_pw,
          #var1 = "io_wiot_hun",
          var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_pw,
          var1 = "sr_norm",
          var2 = "io_norm3",
          #var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 





### --- 01 -- services only
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
version <- "_services"

# filter for services only
services <- c(49, 89)
services_df <- subset(
  reg_df,
  (ind1_2d >= services[1]) &
  (ind1_2d < services[2]) &
  (ind2_2d >= services[1]) &
  (ind2_2d < services[2])
)


# WIOD
summary(ew <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = services_df))
summary(ew_iv <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = services_df))
ew_cl <- coeftest(ew, vcov = vcovCL, cluster = ~ind_pair_id)
ew_iv_cl <- coeftest(ew_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pw <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = services_df))
summary(pw_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = services_df))
pw_cl <- coeftest(pw, vcov = vcovCL, cluster = ~ind_pair_id)
pw_iv_cl <- coeftest(pw_iv, vcov = vcovCL, cluster = ~ind_pair_id)



# transactions
summary(et <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = services_df))
summary(et_iv <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = services_df))
et_cl <- coeftest(et, vcov = vcovCL, cluster = ~ind_pair_id)
et_iv_cl <- coeftest(et_iv, vcov = vcovCL, cluster = ~ind_pair_id)

summary(pt <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = services_df))
summary(pt_iv <- ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = services_df))
pt_cl <- coeftest(pt, vcov = vcovCL, cluster = ~ind_pair_id)
pt_iv_cl <- coeftest(pt_iv, vcov = vcovCL, cluster = ~ind_pair_id)



stargazer(
  ew_cl,
  et_cl,
  ew_iv_cl,
  et_iv_cl,
  #pw_cl,
  #pt_cl,
  #pw_iv_cl,
  #pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_egk", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)

stargazer(
  #ew_cl,
  #et_cl,
  #ew_iv_cl,
  #et_iv_cl,
  pw_cl,
  pt_cl,
  pw_iv_cl,
  pt_iv_cl,
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  out = paste0("../outputs/regression_tables/si_porter", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)





















### --- 01 -- interplot w/ manuf and services lines
fontsize <- 25
version <- ""
region_codes <- c("nuts3")
#region_codes <- c("nuts4")
model_version <- c("noFE", "FE")
path <- paste0("../data/oc15_2023_dec/04oc_data_", version, region_codes, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# filter for manufacturing and services only
manuf <- c(10, 34)
manuf_df <- subset(
  reg_df,
  (ind1_2d >= manuf[1]) &
    (ind1_2d < manuf[2]) &
    (ind2_2d >= manuf[1]) &
    (ind2_2d < manuf[2])
)

services <- c(49, 89)
services_df <- subset(
  reg_df,
  (ind1_2d >= services[1]) &
    (ind1_2d < services[2]) &
    (ind2_2d >= services[1]) &
    (ind2_2d < services[2])
)


int_prca <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = reg_df)
#int_prca <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = reg_df)
ip_a <- interplot(m = int_prca,
                  var1 = "sr_norm",
                  var2 = "io_norm3",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A")

ip_b <- interplot(m = int_prca,
                  var1 = "io_norm3",
                  var2 = "sr_norm",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A")

x_coords_main_a <- ggplot_build(ip_a)$data[[1]]$x
y_coords_main_a <- ggplot_build(ip_a)$data[[1]]$y
x_coords_main_b <- ggplot_build(ip_b)$data[[1]]$x
y_coords_main_b <- ggplot_build(ip_b)$data[[1]]$y



# manufacturing coords
int_prca_manuf <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = manuf_df)
#int_prca_manuf <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = manuf_df)

ip_manuf_a <- interplot(m = int_prca_manuf, var1 = "sr_norm", var2 = "io_norm3", size=3, xmin = -1, xmax = 1,  rfill = "Black") +
  aes(color = "") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  xlim(-1, 1) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=15), axis.title=element_text(size=25)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm")) +
  guides(fill="none") +
  theme(legend.position="none")

x_coords_manuf_a <- ggplot_build(ip_manuf_a)$data[[1]]$x
y_coords_manuf_a <- ggplot_build(ip_manuf_a)$data[[1]]$y


ip_manuf_b <- interplot(m = int_prca_manuf, var1 = "io_norm3", var2 = "sr_norm", size=3, xmin = -1, xmax = 1,  rfill = "Black") +
  aes(color = "") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  xlim(-1, 1) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=15), axis.title=element_text(size=25)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm")) +
  guides(fill="none") +
  theme(legend.position="none")

x_coords_manuf_b <- ggplot_build(ip_manuf_b)$data[[1]]$x
y_coords_manuf_b <- ggplot_build(ip_manuf_b)$data[[1]]$y




# services coords
int_prca_serv <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = services_df)
#int_prca_serv <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = services_df)

ip_serv_a <- interplot(m = int_prca_serv, var1 = "sr_norm", var2 = "io_norm3", size=3, xmin = -1, xmax = 1,  rfill = "Black") +
  aes(color = "") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  xlim(-1, 1) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=15), axis.title=element_text(size=25)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm")) +
  guides(fill="none") +
  theme(legend.position="none")

x_coords_serv_a <- ggplot_build(ip_serv_a)$data[[1]]$x
y_coords_serv_a <- ggplot_build(ip_serv_a)$data[[1]]$y


ip_serv_b <- interplot(m = int_prca_serv, var1 = "io_norm3", var2 = "sr_norm", size=3, xmin = -1, xmax = 1,  rfill = "Black") +
  aes(color = "") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  xlim(-1, 1) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=15), axis.title=element_text(size=25)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm")) +
  guides(fill="none") +
  theme(legend.position="none")

x_coords_serv_b <- ggplot_build(ip_serv_b)$data[[1]]$x
y_coords_serv_b <- ggplot_build(ip_serv_b)$data[[1]]$y



int_ms_a <- ggplot() +
  geom_line(data = data.frame(x = x_coords_manuf_a, y = y_coords_manuf_a), aes(x, y), size = 2, color = "grey") +
  geom_line(data = data.frame(x = x_coords_serv_a, y = y_coords_serv_a), aes(x, y), size = 2, color = "grey") +
  geom_line(data = data.frame(x = x_coords_main_a, y = y_coords_main_a), aes(x, y), size = 4, color = "#15636A") +
  annotate("text", x = 0.5, y = 0.225, label = "Manufacturing", color = "black", size=10) +
  annotate("text", x = 0.435, y = 0.85, label = "Services", color = "black", size=10) +
  xlab("IO (transactions)") +
  ylab("Coeff. labor (SR)") +
  ylim(-0.1, 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))

int_ms_b <- ggplot() +
  geom_line(data = data.frame(x = x_coords_manuf_b, y = y_coords_manuf_b), aes(x, y), size = 2, color = "grey") +
  geom_line(data = data.frame(x = x_coords_serv_b, y = y_coords_serv_b), aes(x, y), size = 2, color = "grey") +
  geom_line(data = data.frame(x = x_coords_main_b, y = y_coords_main_b), aes(x, y), size = 4, color = "#15636A") +
  annotate("text", x = 0.5, y = 0.115, label = "Manufacturing", color = "black", size=10) +
  annotate("text", x = 0.435, y = 0.85, label = "Services", color = "black", size=10) +
  xlab("Labor (SR)") +
  ylab("Coeff. IO (transactions)") +
  ylim(-0.1, 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))



# combine subplots
subplots1 <- plot_grid(
  int_ms_a, int_ms_b,
  labels = c("A", "B"), label_size = fontsize, ncol = 2
)

# titles
plot_title1 <- ggdraw() + draw_label("Coagglomeration (LC)", fontface="bold", size = fontsize)

# combined version
title <- paste0("si_interp_manuf_serv_", model_version[1])
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=1000, height=400, units = 'px')
plot_grid(plot_title1, subplots1, ncol=1, rel_heights=c(0.1, 1, 0.1, 1))
dev.off()













# this works mainly for LC coagglomeration (RCA01)
int_prca <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = reg_df)

ip_a <- interplot(m = int_prca,
                  var1 = "sr_norm",
                  var2 = "io_wiot_hun",
                  #var2 = "io_norm3",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A") +
  xlab("IO (WIOD)") +
  ylab("Coeff. labor (SR)") +
  ylim(0, 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))

ip_b <- interplot(m = int_prca,
                  var1 = "io_wiot_hun",
                  #var1 = "io_norm3",
                  var2 = "sr_norm",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#15636A") +
  xlab("Labor (SR)") +
  ylab("Coeff. IO (WIOD)") +
  ylim(0, 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))


# combine subplots
subplots1 <- plot_grid(
  ip_a, ip_b,
  labels = c("A", "B"), label_size = fontsize, ncol = 2
)










# titles
plot_title1 <- ggdraw() + draw_label("Coagglomeration (EGK)", fontface="bold", size = fontsize)
plot_title2 <- ggdraw() + draw_label("Coagglomeration (LC)", fontface="bold", size = fontsize)


# combined version
title <- paste0("mf02_interplots_", model_version[1])
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=1000, height=750, units = 'px')
plot_grid(plot_title1, subplots1, plot_title2, subplots2, ncol=1, rel_heights=c(0.1, 1, 0.1, 1))
dev.off()









summary(int_ew <- lm(egk_coagg_stand ~ io_wiot_hun * sr_norm, data = services_df))
interplot(m = int_ew,
          var1 = "io_wiot_hun",
          #var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_ew,
          var1 = "sr_norm",
          #var2 = "io_norm3",
          var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 

summary(int_ew <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = services_df))
interplot(m = int_ew,
          #var1 = "io_wiot_hun",
          var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_ew,
          var1 = "sr_norm",
          var2 = "io_norm3",
          #var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 


summary(int_pw <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = services_df))
interplot(m = int_pw,
          var1 = "io_wiot_hun",
          #var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_pw,
          var1 = "sr_norm",
          #var2 = "io_norm3",
          var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 

summary(int_pw <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = services_df))
interplot(m = int_pw,
          #var1 = "io_wiot_hun",
          var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 
interplot(m = int_pw,
          var1 = "sr_norm",
          var2 = "io_norm3",
          #var2 = "io_wiot_hun",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#15636A") 























### --- 04 -- alternative specification (coagg Porter) OLS with interactions
### --- Table 4 alternative specification OLS with interactions
#region_codes <- c("nuts3", "nuts4", "city")
region_codes <- c("nuts4")
ei <- list()
ei_fe <- list()
eifet <- list()
eic <- list()
eic_fe <- list()
#eim01 <- list()
pi <- list()
pi_fe <- list()
pifet <- list()
pic <- list()
pic_fe <- list()
#pim01 <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  #reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  
  # baseline models -- EGK and Porter
  ei[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  pi[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  ei_fe[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pi_fe[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  eifet[[r]] <- lm(egk_coagg_stand ~ io2_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pifet[[r]] <- lm(coagg_porter_rca01_stand ~ io2_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  
  
  eic[[r]] <- coeftest(eim[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  eic_fe[[r]] <- coeftest(eim_fe[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pic[[r]] <- coeftest(pim[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pic_fe[[r]] <- coeftest(pim_fe[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
}  


stargazer(
  ei[[1]],
  ei_fe[[1]],
  eifet[[1]],
  pi[[1]],
  pi_fe[[1]],
  pifet[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO table", "IO transactions", "Labor flow", "IO table X Labor flow", "IO transactions X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions.html")
)


stargazer(
  eimc[[1]],
  eimc_fe[[1]],
  pimc[[1]],
  pimc_fe[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions_cse.html")
)











### --- Table 1 OLS and Table 2 IV

#region_codes <- c("nuts3", "nuts4", "city")
region_codes <- c("nuts3", "nuts4")
em <- list()
pm <- list()
emt <- list()
pmt <- list()
emc <- list()
pmc <- list()
emct <- list()
pmct <- list()
ie <- list()
ip <- list()
iet <- list()
ipt <- list()
iec <- list()
ipc <- list()
iect <- list()
ipct <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc15_2023_dec/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  #reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  
  # baseline models -- EGK and Porter
  em[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df)
  pm[[r]] <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df)
  emc[[r]] <- coeftest(em[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pmc[[r]] <- coeftest(pm[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  emt[[r]] <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = reg_df)
  pmt[[r]] <- lm(coagg_porter_emp_stand ~ io3_stand + lab_stand, data = reg_df)
  emct[[r]] <- coeftest(emt[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pmct[[r]] <- coeftest(pmt[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  
  
  
  # iv models
  ie[[r]] <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df)
  ip[[r]] <- ivreg::ivreg(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df)
  iet[[r]] <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df)
  ipt[[r]] <- ivreg::ivreg(coagg_porter_emp_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df)
  
  iec[[r]] <- coeftest(ie[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  ipc[[r]] <- coeftest(ip[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  iect[[r]] <- coeftest(iet[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  ipct[[r]] <- coeftest(ipt[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  
}

# OLS output
stargazer(em[[1]],
          em[[2]],
          #em[[3]],
          emt[[2]],
          pm[[1]],
          pm[[2]],
          #pm[[3]],
          pmt[[2]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          omit = c("ind1", "ind2"),
          dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
          #covariate.labels = c("IO transactions", "Labor flow"),
          out = paste0("../outputs/regression_tables/01_ols_main.html"))

# OLS output
stargazer(emc[[1]],
          emc[[2]],
          emct[[2]],
          pmc[[1]],
          pmc[[2]],
          pmct[[2]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
          covariate.labels = c("IO (WIOT)", "IO (transactions)", "Labor (SR)"),
          out = paste0("../outputs/regression_tables/01_ols_cse.html"))





# IV output
stargazer(iec[[1]],
          iec[[2]],
          iect[[2]],
          ipc[[1]],
          ipc[[2]],
          ipct[[2]],
          omit.stat=c("f", "ser"),
          dep.var.caption = "",
          omit = c("ind1", "ind2"),
          #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
          #covariate.labels = c("IO transactions", "Labor flow"),
          out = paste0("../outputs/regression_tables/02_iv.html"))






### --- Table 3 OLS with interactions
#region_codes <- c("nuts3", "nuts4", "city")
region_codes <- c("nuts4")
eim <- list()
eim_fe <- list()
eimc <- list()
eimc_fe <- list()
#eim01 <- list()
pim <- list()
pim_fe <- list()
pimc <- list()
pimc_fe <- list()
#pim01 <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  
  # baseline models -- EGK and Porter
  eim[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  pim[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  eim_fe[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pim_fe[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  
  eimc[[r]] <- coeftest(eim[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  eimc_fe[[r]] <- coeftest(eim_fe[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pimc[[r]] <- coeftest(pim[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pimc_fe[[r]] <- coeftest(pim_fe[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
}  


stargazer(
  eim[[1]],
  eim_fe[[1]],
  pim[[1]],
  pim_fe[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions.html")
)


stargazer(
  eimc[[1]],
  eimc_fe[[1]],
  pimc[[1]],
  pimc_fe[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions_cse.html")
)




### --- Table 4 alternative specification OLS with interactions
#region_codes <- c("nuts3", "nuts4", "city")
region_codes <- c("nuts4")
ei <- list()
ei_fe <- list()
eifet <- list()
eic <- list()
eic_fe <- list()
#eim01 <- list()
pi <- list()
pi_fe <- list()
pifet <- list()
pic <- list()
pic_fe <- list()
#pim01 <- list()

for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  
  # baseline models -- EGK and Porter
  ei[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  pi[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  ei_fe[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pi_fe[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  eifet[[r]] <- lm(egk_coagg_stand ~ io2_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  pifet[[r]] <- lm(coagg_porter_rca01_stand ~ io2_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df)
  
  
  eic[[r]] <- coeftest(eim[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  eic_fe[[r]] <- coeftest(eim_fe[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pic[[r]] <- coeftest(pim[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
  pic_fe[[r]] <- coeftest(pim_fe[[r]], vcov = vcovCL, cluster = ~ind_pair_id)
}  


stargazer(
  ei[[1]],
  ei_fe[[1]],
  eifet[[1]],
  pi[[1]],
  pi_fe[[1]],
  pifet[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO table", "IO transactions", "Labor flow", "IO table X Labor flow", "IO transactions X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions.html")
)


stargazer(
  eimc[[1]],
  eimc_fe[[1]],
  pimc[[1]],
  pimc_fe[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions_cse.html")
)











### --- Figure 2 -- interplots
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
#path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
#reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]

# baseline models -- EGK and Porter
#pipm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)
#pipm <- lm(coagg_porter_emp_stand ~ io_norm3 * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)
pipm <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)

# interplot 1 
title <- "figure021_interplot_labor_IO"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

ip1 <- interplot(m = pipm,
          var1 = "sr_norm",
          #var2 = "io_wiot_hun",
          var2 = "io_norm3",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#6da3d0") +
  xlab("IO connections") +
  ylab("Estimated coefficient for\nlabor flow") +
  #ylim(0, 0.5) +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "figure022_interplot_IO_labor"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

ip2 <- interplot(m = pipm,
          #var1 = "io_wiot_hun",
          var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#6da3d0") +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO connections") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# combined version
title <- "fig02_interplots"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=1400, height=650, units = 'px')
plot_grid(ip1, ip2, labels = c('A', 'B'), label_size = 40)
dev.off()













### --- Neave-style IO and labor betas
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
reg_df <- prep_symmetric_regression_table(path)

industries <- unique(reg_df$ind1)
betas_df <- list()
#industries <- c(11)
for(i in 1:length(industries)){
  # print(industries[i])
  
  # separate OLS for each ind_i
  ols_data <- subset(reg_df, ind1 == industries[i])  
  model_summary <- summary(lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = ols_data))
  coefficients <- model_summary$coefficients[, "Estimate"]
  se <- model_summary$coefficients[, "Std. Error"]
  p_values <- model_summary$coefficients[, "Pr(>|t|)"]
  
  coeffs_dt <- data.table(
    variable = names(coefficients),
    beta = coefficients,
    se = se,
    p_value = p_values
  )
  
  # add industry code
  coeffs_dt$ind <- industries[i]
  betas_df[[i]] <- coeffs_dt
}
betas_df <- Filter(function(x) dim(x)[1] == 3 && dim(x)[2] == 5, betas_df)
betas_df <- rbindlist(betas_df)


# add groups
nace_labels <- readxl::read_excel("../data/nace_labels.xlsx") %>%
  dplyr::select(ind3dig, ind2dig, ind_group, ind_group_color) %>%
  unique() %>%
  data.table()

betas_df <- merge(
  betas_df,
  nace_labels,
  by.x = "ind",
  by.y = "ind3dig",
  all.x = TRUE,
  all.y = FALSE
)



# export
write.table(betas_df,
            paste0("../outputs/betas_io_labor.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)









### --- table 5 OLS for MNE / domestic -- synthetic MNEs
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
path_syn <- paste0("../data/oc13_2023_oct_3/04c_oc_data_syn_MNEs_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
alt_df <- prep_alternative_table(path, path_syn)

# common ground for comparable regressions
common_ind_pairs <- intersect(unique(reg_df$ind_pair_id), unique(alt_df$ind_pair_id))
reg_df <- subset(reg_df, ind_pair_id %in% common_ind_pairs)
alt_df <- subset(alt_df, ind_pair_id %in% common_ind_pairs)

summary(pm01 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm02 <- lm(coagg_porter_rca01_mne_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm03 <- lm(coagg_porter_rca01_local_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm04 <- lm(coagg_porter_rca01_mixed_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))

summary(pms01 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = alt_df))
summary(pms02 <- lm(coagg_porter_rca01_mne_stand ~ io_wiot_hun_stand + lab_stand, data = alt_df))
summary(pms03 <- lm(coagg_porter_rca01_local_stand ~ io_wiot_hun_stand + lab_stand, data = alt_df))
summary(pms04 <- lm(coagg_porter_rca01_mixed_stand ~ io_wiot_hun_stand + lab_stand, data = alt_df))



stargazer(
  #pm01,
  #pms01,
  pm02,
  pms02,
  pm03,
  pms03,
  pm04,
  pms04,
  omit.stat=c("f", "ser"),
  dep.var.caption = "Coagglomeration (LC)",
  dep.var.labels = c("M-M", "sM-sM", "D-D", "sD-sD", "M-D", "sM-sD"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/05_ols_foreign_domestic_syn.html")
)












### --- SI -- interaction of dummies
region_level <- "nuts4"
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

summary(i01_egk <- lm(egk_coagg_stand ~ io_wiot01 * labor01 + as.factor(ind1) + as.factor(ind2), data = reg_df))
summary(i01_cpe <- lm(coagg_porter_emp_stand ~ io3_stand * labor01, data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ io_wiot01 * labor01 + as.factor(ind1) + as.factor(ind2), data = reg_df))

summary(i01_egk <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df))


stargazer(
  i01_egk,
  i01_cpr,
  omit.stat=c("f", "ser"),
  omit = c("ind1", "ind2"),
  out = paste0("../outputs/regression_tables/interaction_tests_cont.html")
)

