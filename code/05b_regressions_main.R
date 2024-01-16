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



### --- 00 -- baseline setting for local tests
#path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
path <- paste0("../data/oc15_2023_dec/04oc_data_", region_level, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)



summary(em <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = reg_df))
summary(ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
summary(ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_usa_stand + iv_swe_lab_stand, data = reg_df))
iv_subset <- subset(reg_df, ind1_2d != ind2_2d)
summary(emiv <- ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_usa_stand + iv_swe_lab_stand, data = iv_subset))
coeftest(emiv, vcov = vcovCL, cluster = ~ind_pair_id)


summary(ivreg::ivreg(egk_coagg_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))


summary(pm <- lm(coagg_porter_emp_stand ~ io3_stand + lab_stand, data = reg_df))
summary(pmiv <- ivreg::ivreg(coagg_porter_emp_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
coeftest(pmiv, vcov = vcovCL, cluster = ~ind_pair_id)
summary(ivreg::ivreg(coagg_porter_emp_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = iv_subset))

summary(pm <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = reg_df))
summary(ivreg::ivreg(coagg_porter_rca01_stand ~ io3_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))




summary(em <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))

summary(pm <- lm(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(ivreg::ivreg(coagg_porter_emp_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))

summary(pm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = reg_df))
summary(ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))



### --- Table 1 OLS and Table 2 IV

#region_codes <- c("nuts3", "nuts4", "city")
focal_year <- 2017
region_codes <- c("nuts3", "nuts4")
#version <- c("_same2digit_dropped")
version <- c("")
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
  path <- paste0("../data/oc15_2023_dec/04oc_data_", region_codes[r], "_", focal_year, ".csv")
  reg_df <- prep_baseline_regression_table(path)
  
  # remove rows where 2-digit industries are the same
  if(version != ""){
    reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]
  }
  
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
          out = paste0("../outputs/regression_tables/01_ols_main", version[1], ".html"))
          #out = paste0("../outputs/regression_tables/01_ols_main", version[1], ".tex"))

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
          #out = paste0("../outputs/regression_tables/01_ols_cse", version[1], ".html"))
          out = paste0("../outputs/regression_tables/01_ols_cse", version[1], ".tex"))





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
          out = paste0("../outputs/regression_tables/02_iv", version[1], ".html"))
          #out = paste0("../outputs/regression_tables/02_iv", version[1], ".tex"))






### --- Table 3 OLS with interactions
#region_codes <- c("nuts3", "nuts4", "city")
#region_codes <- c("nuts3", "nuts4")
#region_codes <- c("nuts3")
region_codes <- c("nuts4")
#version <- c("_same2digit_dropped")
version <- c("")
ei <- list()
eif <- list()
eit <- list()
eitf <- list()
eic <- list()
eifc <- list()
eitc <- list()
eitfc <- list()

#eim01 <- list()
pi <- list()
pif <- list()
pit <- list()
pitf <- list()
pic <- list()
pifc <- list()
pitc <- list()
pitfc <- list()
#pim01 <- list()

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
  #eimt[[1]],
  eitf[[1]],
  pi[[1]],
  pif[[1]],
  pitf[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  #covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  #out = paste0("../outputs/regression_tables/03_interactions", version, ".tex")
  out = paste0("../outputs/regression_tables/03_interactions", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3", version, ".tex")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3", version, ".html")
)

stargazer(
  eic[[1]],
  eifc[[1]],
  #eimt[[1]],
  eitfc[[1]],
  pic[[1]],
  pifc[[1]],
  pitfc[[1]],
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  omit = c("ind1", "ind2"),
  add.lines=list(c("Two way industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  #covariate.labels = c("IO connections", "Labor flow", "IO connections X Labor flow"),
  #out = paste0("../outputs/regression_tables/03_interactions_cse", version, ".tex")
  out = paste0("../outputs/regression_tables/03_interactions_cse", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".html")
  #out = paste0("../outputs/regression_tables/si_interactions_nuts3_cse", version, ".tex")
)







### --- Figure 2 -- interplots
fontsize <- 25
region_codes <- c("nuts4")
model_version <- c("noFE", "FE")
path <- paste0("../data/oc15_2023_dec/04oc_data_", version, region_codes, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
#reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]


# the model -- EGK
int_egk <- lm(egk_coagg_stand ~ io_wiot_hun * sr_norm, data = reg_df)

ip_a <- interplot(m = int_egk,
                 var1 = "sr_norm",
                 var2 = "io_wiot_hun",
                 #var2 = "io_norm3",
                 size = 3,
                 xmin = -1,
                 xmax = 1,
                 rfill = "#6da3d0") +
  xlab("IO connections") +
  ylab("Estimated coefficient for\nlabor flow") +
  ylim(-0.025, 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm"))

ip_b <- interplot(m = int_egk,
                 var1 = "io_wiot_hun",
                 #var1 = "io_norm3",
                 var2 = "sr_norm",
                 size = 3,
                 xmin = -1,
                 xmax = 1,
                 rfill = "#6da3d0") +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO connections") +
  ylim(-0.025, 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm"))


# the model -- LC (emp)
int_pemp <- lm(coagg_porter_emp_stand ~ io_wiot_hun * sr_norm, data = reg_df)

ip_c <- interplot(m = int_pemp,
                  var1 = "sr_norm",
                  var2 = "io_wiot_hun",
                  #var2 = "io_norm3",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#6da3d0") +
  xlab("IO connections") +
  ylab("Estimated coefficient for\nlabor flow") +
  ylim(0, 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))

ip_d <- interplot(m = int_pemp,
                  var1 = "io_wiot_hun",
                  #var1 = "io_norm3",
                  var2 = "sr_norm",
                  size = 3,
                  xmin = -1,
                  xmax = 1,
                  rfill = "#6da3d0") +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO connections") +
  ylim(0, 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=fontsize-5), axis.title=element_text(size=fontsize)) +
  theme(plot.margin = unit(c(2, 1.5, 0, 1), "cm"))


# combined version
title <- paste0("mf02_interplots_", model_version[1])
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=1000, height=700, units = 'px')
plot_grid(
  ip_a, ip_b, ip_c, ip_d,
  labels = c("A", "B", "C", "D"), label_size = fontsize, ncol = 2
)
dev.off()









region_codes <- c("nuts4")
model_version <- c("noFE", "FE")
path <- paste0("../data/oc15_2023_dec/04oc_data_", version, region_codes, "_", focal_year, ".csv")
reg_df <- prep_baseline_regression_table(path)
#reg_df <- reg_df[complete.cases(reg_df[ , c("io_norm2")]), ]

# baseline models -- EGK and Porter
#pipm <- lm(egk_coagg_stand ~ io_wiot_hun * sr_norm, data = reg_df)
pipm <- lm(coagg_porter_emp_stand ~ io_wiot_hun * sr_norm, data = reg_df)
#pipm <- lm(egk_coagg_stand ~ io_norm3 * sr_norm, data = reg_df)
#pipm <- lm(coagg_porter_emp_stand ~ io_wiot_hun * sr_norm, data = reg_df)
#pipm <- lm(coagg_porter_emp_stand ~ io_norm3 * sr_norm, data = reg_df)
#pipm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = reg_df)
#pipm <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm, data = reg_df)
#pipm <- lm(egk_coagg_stand ~ io_wiot_hun * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)
#pipm <- lm(coagg_porter_emp_stand ~ io_wiot_hun * sr_norm, data = reg_df)
#pipm <- lm(coagg_porter_emp_stand ~ io_wiot_hun * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)
#pipm <- lm(coagg_porter_emp_stand ~ io_norm3 * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)
#pipm <- lm(coagg_porter_rca01_stand ~ io_norm3 * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)

# interplot 1 
title <- paste0("figure021_interplot_labor_IO_", model_version[1])
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=600, height=600, units = 'px')

ip1 <- interplot(m = pipm,
          var1 = "sr_norm",
          var2 = "io_wiot_hun",
          #var2 = "io_norm3",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#6da3d0") +
  xlab("IO connections") +
  ylab("Estimated coefficient for\nlabor flow") +
  #ylim(-0.025, 0.35) +
  #ylim(-0.1, 0.4) +
  ylim(0, 0.7) +
  #ylim(0, 0.6) +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm"))
dev.off()


# interplot 2
title <- paste0("figure022_interplot_IO_labor_", model_version[1])
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=600, height=600, units = 'px')

ip2 <- interplot(m = pipm,
          var1 = "io_wiot_hun",
          #var1 = "io_norm3",
          var2 = "sr_norm",
          size = 3,
          xmin = -1,
          xmax = 1,
          rfill = "#6da3d0") +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO connections") +
  #ylim(-0.025, 0.35) +
  #ylim(-0.1, 0.4) +
  ylim(0, 0.7) +
  #ylim(0, 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40)) +
  theme(plot.margin = unit(c(2, 1, 0, 1), "cm"))
dev.off()


# combined version
title <- paste0("fig02_interplots_pemp_", model_version[1])
#title <- "fig02_interplots_pemp_FE"
file_name <- paste0("../figures/", title, "_", region_codes, ".png")
png(file_name, width=1400, height=650, units = 'px')
plot_grid(ip1, ip2, labels = c('A', 'B'), label_size = 40)
dev.off()








# transactions based interplot
pifet <- lm(coagg_porter_rca01_stand ~ io2_stand * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df)

# interplot 1 
title <- "figure021_interplot_labor_IO_transactions"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

ip1 <- interplot(m = pifet,
                 var1 = "sr_norm",
                 var2 = "io2_stand",
                 size = 3,
                 xmin = -1,
                 xmax = 1,
                 rfill = "#6da3d0") +
                 #rfill = "#2C728EFF") +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nlabor flow") +
  #ylim(0, 0.5) +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "figure022_interplot_IO_transactions_labor"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

ip2 <- interplot(m = pifet,
                 var1 = "io2_stand",
                 var2 = "sr_norm",
                 size = 3,
                 xmin = -1,
                 xmax = 1,
                 rfill = "#6da3d0") +
                 #rfill = "#2C728EFF") +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# combined version
#title <- "fig02_interplots_transactions"
title <- "fig02_interplots_transactions_same2digit_dropped"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=1400, height=650, units = 'px')
plot_grid(ip1, ip2, labels = c('A', 'B'), label_size = 40)
dev.off()








summary(pipm <- lm(egk_coagg_stand ~ io_wiot_hun * sr_norm, data = reg_df))
summary(pipm <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm, data = reg_df))

summary(pipm1 <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df))
summary(pipm2 <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand + as.factor(ind1) + as.factor(ind2), data = reg_df))

stargazer(
  pipm1,
  pipm2,
  type = "text",
  omit = c("ind1", "ind2")
)


summary(iv_em <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
summary(iv_pm <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))

summary(ivi_em <- ivreg::ivreg(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand | iv_wiot_usa_stand * iv_swe_lab_stand, data = reg_df))
summary(ivi_pm <- ivreg::ivreg(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand | iv_wiot_usa_stand * iv_swe_lab_stand, data = reg_df))









### --- Table -- labor flow SWE -- first stage illustration

# first and second stage separately
summary(iv_egk <- ivreg::ivreg(egk_coagg_stand ~ lab_stand | iv_swe_lab_stand, data = reg_df))

# first stage with lm
summary(first_stage <- lm(lab_stand ~ iv_swe_lab_stand, data = reg_df))
reg_df$first_stage_pred <- predict(first_stage, newdata = reg_df)
cor(reg_df$lab_stand, reg_df$first_stage_pred)

# second stage with lm
summary(second_stage <- lm(egk_coagg_stand ~ first_stage_pred, data = reg_df))

stargazer(
  first_stage,
  second_stage,
  column.labels = c("First stage", "Second stage"),
  omit.stat=c("f", "ser"),
  dep.var.caption = "",
  #omit = c("ind1", "ind2"),
  #add.lines=list(c("Two way industry FE", "No", "Yes", "No", "Yes")),
  #dep.var.labels = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  covariate.labels = c("Labor flow SWE", "Labor flow pred"),
  out = paste0("../outputs/regression_tables/05_iv_illustration.html")
  
)









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



summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ io_wiot_hun * sr_norm + as.factor(ind1) + as.factor(ind2), data = reg_df))


# interplot 1 
title <- "figure021_interplot_labor_IO_FE"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = i01_cpr,
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
title <- "figure022_interplot_IO_labor_FE"
file_name <- paste0("../figures/", title, "_", region_level, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = i01_cpr,
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













reg_df$abs0_io <- ifelse(reg_df$io_wiot_hun == -1, 0, 1)
reg_df$abs0_lab <- ifelse(reg_df$sr_norm == -1, 0, 1)


summary(i01_egk <- lm(egk_coagg_stand ~ abs0_io * abs0_lab, data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ abs0_io * abs0_lab, data = reg_df))

summary(i01_egk <- lm(egk_coagg_stand ~ abs0_lab + labor01, data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ abs0_lab + labor01, data = reg_df))

summary(i01_egk <- lm(egk_coagg_stand ~ abs0_io + io_wiot01, data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ abs0_io + io_wiot01, data = reg_df))




reg_df$d00 <- ifelse(
  (reg_df$io_wiot_hun == -1) &
  (reg_df$sr_norm == -1),
  1,
  0
)
reg_df$d01 <- ifelse(
  (reg_df$io_wiot_hun == -1) &
    (reg_df$sr_norm > -1 & reg_df$sr_norm < 0),
  1,
  0
)
reg_df$d02 <- ifelse(
  (reg_df$io_wiot_hun == -1) &
    (reg_df$sr_norm > 0),
  1,
  0
)

reg_df$d10 <- ifelse(
  (reg_df$io_wiot_hun > -1 & reg_df$io_wiot_hun < 0) &
    (reg_df$sr_norm == -1),
  1,
  0
)
reg_df$d20 <- ifelse(
  (reg_df$io_wiot_hun > 0) &
    (reg_df$sr_norm == -1),
  1,
  0
)

reg_df$d11 <- ifelse(
  (reg_df$io_wiot_hun > -1 & reg_df$io_wiot_hun < 0) &
  (reg_df$sr_norm > -1 & reg_df$sr_norm < 0),
  1,
  0
)

reg_df$d12 <- ifelse(
  (reg_df$io_wiot_hun > -1 & reg_df$io_wiot_hun < 0) &
  (reg_df$sr_norm > 0),
  1,
  0
)

reg_df$d21 <- ifelse(
  (reg_df$io_wiot_hun > 0) &
  (reg_df$sr_norm > -1 & reg_df$sr_norm < 0),
  1,
  0
)

reg_df$d22 <- ifelse(
  (reg_df$io_wiot_hun > 0) &
  (reg_df$sr_norm > 0),
  1,
  0
)



summary(i01_egk <- lm(egk_coagg_stand ~ d01 + d02 + d10 + d11 + d12 + d20 + d21 + d22 , data = reg_df))
summary(i01_cpr <- lm(coagg_porter_rca01_stand ~ d01 + d02 + d10 + d11 + d12 + d20 + d21 + d22 , data = reg_df))





### --- SI -- transaction data OLS




### --- SI -- single plant firms and without BP OLS
path <- paste0("../data/oc13_2023_oct_3/04oc_data_", version, region_level, "_", focal_year, ".csv")
path_BP <- paste0("../data/oc13_2023_oct_3/04b_oc_data_budapest_excluded_", version, region_level, "_", focal_year, ".csv")
path_single <- paste0("../data/oc13_2023_oct_3/04d_oc_data_single_plant_", version, region_level, "_", focal_year, ".csv")

bpo_df <- prep_alternative_table(path, path_BP)
summary(ebp <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = bpo_df))
summary(ebpi <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = bpo_df))
summary(pbp <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = bpo_df))
summary(pbpi <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = bpo_df))

single_df <- prep_alternative_table(path, path_single)
summary(esi <- lm(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand, data = single_df))
summary(esii <- lm(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand, data = single_df))
summary(psi <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand, data = single_df))
summary(psii <- lm(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand, data = single_df))

summary(esi2 <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = single_df))
summary(esii2 <- lm(egk_coagg_stand ~ io3_stand * lab_stand, data = single_df))
summary(psi2 <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = single_df))
summary(psii2 <- lm(coagg_porter_rca01_stand ~ io3_stand * lab_stand, data = single_df))



reg_df <- prep_baseline_regression_table(path)
summary(eio <- lm(egk_coagg_stand ~ io3_stand + lab_stand, data = reg_df))
summary(eioi <- lm(egk_coagg_stand ~ io3_stand * lab_stand, data = reg_df))
summary(pio <- lm(coagg_porter_rca01_stand ~ io3_stand + lab_stand, data = reg_df))
summary(pioi <- lm(coagg_porter_rca01_stand ~ io2_stand * lab_stand, data = reg_df))





stargazer(
  ebp,
  ebpi,
  #esi,
  #esii,
  eio,
  eioi,
  pbp,
  pbpi,
  #psi,
  #psii,
  pio,
  pioi,
  omit.stat=c("f", "ser"),
  #dep.var.caption = c("Coagglomeration (EGK)", "Coagglomeration (LC)"),
  dep.var.labels = c("Excl. BP", "Single plant", "Excl. BP", "Single plant"),
  covariate.labels = c("IO connections", "IO transactions", "Labor flow"),
  #out = paste0("../outputs/regression_tables/si_ols_without_bp_single_plant.html")
  type = "text"
)



stargazer(
  esi,
  esii,
  psi,
  psii,
  esi2,
  esii2,
  psi2,
  psii2,
  omit.stat=c("f", "ser"),
  dep.var.caption = c("Coagglomeration (LC)"),
  #dep.var.labels = c("Excl. BP", "Single plant", "Transaction data"),
  covariate.labels = c("IO connections", "IO transactions", "Labor flow", "IO conn X Labor", "IO trans X Labor"),
  #out = paste0("../outputs/regression_tables/si_ols_without_bp_single_plant.html")
  type = "text"
)


interplot(m = pioi,
          var1 = "io2_stand",
          var2 = "lab_stand",
          size = 3,
          #xmin = -1,
          #xmax = 1,
          rfill = "#6da3d0")
  



### --- SI -- without Budapest OLS


summary(lm(coagg_porter_rca01_stand ~ io2_stand + lab_stand, reg_df ))
summary(ivreg::ivreg(egk_coagg_stand ~ io2_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))
summary(ivreg::ivreg(coagg_porter_rca01_stand ~ io2_stand + lab_stand | iv_wiot_mean_stand + iv_swe_lab_stand, data = reg_df))





