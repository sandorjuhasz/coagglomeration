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

#region_level <- "nuts3"
region_level <- "nuts4"
#region_level <- "city"

version <- ""
#version <- "budapest_excluded_"
#version <- "syn_MNEs_"
#version <- "single_plant_"
#version <- "manuf"
#version <- "serv"



### --- 00 -- baseline setting for local tests
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

summary(em <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))



### --- Table 1 OLS and Table 2 IV

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






### --- Table 3 OLS with interactions
region_codes <- c("nuts3", "nuts4", "city")
eim <- list()
eim01 <- list()
pim <- list()
pim01 <- list()
for(r in 1:length(region_codes)){
  # file from OC
  path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  
  # baseline models -- EGK and Porter
  eim[[r]] <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
  pim[[r]] <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df)
}  

stargazer(
  #eim[[1]],
  #eim[[2]],
  #eim[[3]],
  pim[[1]],
  pim[[2]],
  pim[[3]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  dep.var.labels = c("Coagglomeration (corr)"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/03_interactions.html")
)




### --- Figure 2 -- interplots
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

# baseline models -- EGK and Porter
pipm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = reg_df)


# interplot 1 
title <- "figure021_interplot_labor_IO"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

ip1 <- interplot(m = pipm,
          var1 = "sr_norm",
          var2 = "io_wiot_hun",
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
          var1 = "io_wiot_hun",
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




### --- Table 4 OLS for MNE / domestic
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)

summary(pm01 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm02 <- lm(coagg_porter_rca01_mne_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm03 <- lm(coagg_porter_rca01_local_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(pm04 <- lm(coagg_porter_rca01_mixed_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))

summary(pmi01 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(pmi02 <- lm(coagg_porter_rca01_mne_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(pmi03 <- lm(coagg_porter_rca01_local_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))
summary(pmi04 <- lm(coagg_porter_rca01_mixed_stand ~ io_wiot_hun_stand * lab_stand, data = reg_df))


stargazer(
  pm01,
  pmi01,
  pm02,
  pmi02,
  pm03,
  pmi03,
  pm04,
  pmi04,
  omit.stat=c("f", "ser"),
  dep.var.caption = "Coagglomeration (LC)",
  dep.var.labels = c("All", "MNE-MNE", "Dom-Dom", "MNE-Dom", "All", "MNE-MNE", "Dom-Dom", "MNE-Dom"),
  covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  out = paste0("../outputs/regression_tables/04_ols_foreign_domestic.html")
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

summary(i01_egk <- lm(egk_coagg_stand ~ io_wiot01 * labor01, data = reg_df))
summary(i01_cpe <- lm(coagg_porter_emp_stand ~ io_wiot01 * labor01, data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ io_wiot01 * labor01, data = reg_df))







### --- SI -- first stage IV (and related descriptives)




### --- SI -- transaction data OLS




### --- SI -- single plant firms and without BP OLS
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
path_BP <- paste0("../data/oc13_2023_oct_3/04b_oc_data_budapest_excluded_", version, region_level, "_", focal_year, ".csv")
path_single <- paste0("../data/oc13_2023_oct_3/04d_oc_data_single_plant_", version, region_level, "_", focal_year, ".csv")

bpo_df <- prep_alternative_table(path, path_BP)
summary(em_bp <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = bpo_df))
summary(pm_bp <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = bpo_df))

single_df <- prep_alternative_table(path, path_single)
summary(em_sing <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = single_df))
summary(pm_sing <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = single_df))



stargazer(
  em_bp,
  em_sing,
  pm_bp,
  pm_sing,
  omit.stat=c("f", "ser"),
  dep.var.caption = c("Coagglomeration (EGK)"),
  dep.var.labels = c("Excl. BP", "Single plant", "Excl. BP", "Single plant"),
  covariate.labels = c("IO connections", "Labor flow"),
  out = paste0("../outputs/regression_tables/si_ols_without_bp_single_plant.html")
)






### --- SI -- without Budapest OLS




