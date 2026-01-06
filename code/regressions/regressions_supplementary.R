# Regression tables for the Supplementary information -- sandorjuhasz
# setup is the same as in the Databank



# --- Structure following the Supplementary information file --- #
### S3 -- Results considering knowledge spillovers through patent data
### S4 -- Univariate OLS estimations for labor and input-output channels
### S5 -- Stepwise instrumental variable estimations
### S6 -- Alternative instruments for input-output connections
### S7 -- Results for manufacturing and services
### S8 -- Results for knowledge intensive services
### S9 -- Geographic restrictions for robustness checks
### S10 -- Multivariate instrumental variable regressions
### S11 -- Alternative specifications for our models with interactions
# --- --- #



library(data.table)
library(dplyr)
library(lmtest)
library(sandwich)
library(fixest)
library(interplot)
library(cowplot)
library(margins)
source("../../code/regressions/regressions_utils.R")


# parameters
focal_year <- 2017
regions <- c("nuts3", "nuts4")






### S3 -- Results considering knowledge spillovers through patent data

# -- data preparation block

# EPO patent data
epo_df <- fread("../../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_IPC.txt")

# focal period
epo_df <- subset(epo_df, (prio_year<=focal_year))

# applicant region
app_df <- fread("../../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_Inv_reg.txt")
app_df <- app_df %>%
  filter(ctry_code == "HU") %>%
  dplyr::select(appln_id, ctry_code) %>%
  unique() %>%
  data.table()



# only patents from Hungary
epo_df <- merge(
  epo_df,
  app_df,
  by = "appln_id",
  all.x = TRUE,
  all.y = FALSE
)
epo_df <- subset(epo_df, ctry_code == "HU")
epo_df$IPC4d <- substr(epo_df$IPC, 1, 4)

# clean EPO
epo_df <- epo_df %>%
  dplyr::select(appln_id, ctry_code, IPC4d) %>%
  unique() %>%
  data.table()

write.table(epo_df,
            paste0("../../data/outputs/epo_until_2017_Hungary.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)
rm(list=ls())
gc()




# EPO citation data
epo_df <- fread("../../data/outputs/epo_until_2017_Hungary.csv")
epo_df <- dplyr::select(epo_df, appln_id, ctry_code) %>% unique()

# citation table
cdf <- fread("../../data/patent_data/OECD_patent_data_2024SEPT/OECD_CITATIONS_202408/202408_EPO_CITATIONS.txt")

# add country code to patents in citation table
cdf <- merge(
  cdf,
  epo_df,
  by.x = "Citing_appln_id",
  by.y = "appln_id",
  all.x = TRUE,
  all.y = FALSE
)
cdf <- merge(
  cdf,
  epo_df,
  by.x = "Cited_Appln_id",
  by.y = "appln_id",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_citing", "_cited")
)

# all cited inside Hungary
ctable <- cdf %>%
  filter((ctry_code_citing == "HU") & (ctry_code_cited == "HU")) %>%
  dplyr::select(appln_id_citing = Citing_appln_id, appln_id_cited = Cited_Appln_id)

# add tech codes to citation relations in Hungary
epo_df <- fread("../../data/outputs/epo_until_2017_Hungary.csv")
epo_df <- dplyr::select(epo_df, appln_id, IPC4d) %>% unique()

# add tech codes to citation table
ctable <- merge(
  ctable,
  epo_df,
  by.x = "appln_id_citing",
  by.y = "appln_id",
  all.x = TRUE,
  all.y = FALSE,
  allow.cartesian = TRUE
)
ctable <- merge(
  ctable,
  epo_df,
  by.x = "appln_id_cited",
  by.y = "appln_id",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_citing", "_cited"),
  allow.cartesian = TRUE
)
ctable <- subset(ctable, IPC4d_citing != IPC4d_cited)



# concordance table
conc <- fread("../../data/patent_data/correspondance_ipc_nace.csv")
conc$ind <- substr(conc$NACE2, 1, 2)
conc <- dplyr::select(conc, ipc = IPCV2015, ind) %>% unique()

# add nace codes to citation table
cnace <- merge(
  ctable,
  conc,
  by.x = "IPC4d_citing",
  by.y = "ipc",
  all.x = TRUE,
  all.y = FALSE
)
cnace <- merge(
  cnace,
  conc,
  by.x = "IPC4d_cited",
  by.y = "ipc",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_i", "_j")
)
cnace <- cnace %>%
  group_by(ind_i, ind_j) %>%
  summarise(c_ij = n()) %>%
  data.table()
cnace <- subset(cnace, (is.na(ind_i)==0) & (is.na(ind_j)==0))
cnace <- subset(cnace, (ind_i != "Co") & (ind_j != "Co"))
cnace$ind_i <- as.integer(cnace$ind_i)
cnace$ind_j <- as.integer(cnace$ind_j)



# full 2-digit combination
reg_df <- combined_regression_df(regions, export = TRUE)
ind_2d <- unique(c(reg_df$ind1_2d, reg_df$ind2_2d))
full_comb <- data.table(expand.grid(ind_2d, ind_2d))
colnames(full_comb) <- c("ind_i", "ind_j")

# add citation to full combination
cnace <- merge(
  full_comb,
  cnace,
  by = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
cnace$c_ij[is.na(cnace$c_ij)==1] <- 0

# citation relatedness
cr_norm <- cnace %>%
  group_by(ind_i) %>%
  mutate(c_i = sum(c_ij)) %>%
  group_by(ind_j) %>%
  mutate(c_j = sum(c_ij)) %>%
  ungroup() %>%
  mutate(c = sum(c_ij)) %>%
  mutate(c_rel = c_ij / ((c_i * c_j) / c)) %>%
  mutate(cr_norm = (c_rel - 1) / (c_rel + 1)) %>%
  data.table()
cr_norm$cr_norm[is.na(cr_norm$cr_norm) == 1] <- -1


# edge id
cr_norm[, e_id := .GRP, by = .(pmin(ind_i, ind_j), pmax(ind_i, ind_j))]


# make undirected
cr_norm <- cr_norm %>%
  arrange(e_id, ind_i, ind_j) %>%
  group_by(e_id) %>%
  mutate(cr_norm = sum(cr_norm, na.rm = TRUE) / 2) %>%
  data.table()


# one-way only
#cr_norm <- subset(cr_norm, ind_i < ind_j)


# export
write.table(cr_norm,
            paste0("../../data/outputs/cr_norm_citation.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)





# -- regressions block

reg_df <- combined_regression_df(regions, export = TRUE)
cr_norm <- fread("../../data/outputs/cr_norm_citation.csv")
cr_norm$cr_stand <- scale(cr_norm$cr_norm)

reg_df <- merge(
  reg_df,
  cr_norm,
  by.x = c("ind1_2d", "ind2_2d"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)


# main OLS
m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)
m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)
m3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)
m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)

m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)
m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)
m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)
m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand + lab_stand + cr_norm,
            cluster = ~ind1 + ind2,
            data = reg_df)

etable(m1, m2, m3, m4, m5, m6, m7, m8)



# interaction mdoels
int_m1 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand + cr_norm,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand + cr_norm | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m3 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand + cr_norm,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand + cr_norm | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)

int_m5 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand + cr_norm,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand + cr_norm | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m7 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand + cr_norm,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand + cr_norm | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)

etable(int_m1, int_m2, int_m3, int_m4, int_m5, int_m6, int_m7, int_m8)



# interplot data generation -- WIOD
# fit the linear regression model with clustered standard errors
int_m1 <- lm(egk_coagg_stand_nuts4 ~ io_wiot_hun * sr_norm + cr_norm, data = reg_df)
clustered_se <- vcovCL(int_m1, vcov = vcovCL, cluster = ~ind1 + ind2)

# calculate marginal effects using clustered standard errors
marginal_effects11 <- margins(
  int_m1,
  variables = "io_wiot_hun",
  at = list(sr_norm = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf11 <- data.table(summary(marginal_effects11)) %>%
  rename(
    x_values = sr_norm,
    y_values = factor
  )
mf11$dep_var <- "egk_coagg_stand_nuts4"
mf11$x_var <- "sr_norm"


# calculate marginal effects using clustered standard errors
marginal_effects12 <- margins(
  int_m1,
  variables = "sr_norm",
  at = list(io_wiot_hun = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf12 <- data.table(summary(marginal_effects12)) %>%
  rename(
    x_values = io_wiot_hun,
    y_values = factor
  )
mf12$dep_var <- "egk_coagg_stand_nuts4"
mf12$x_var <- "io_wiot_hun"


# fit the linear regression model with clustered standard errors
int_m2 <- lm(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun * sr_norm + cr_norm, data = reg_df)
clustered_se <- vcovCL(int_m2, vcov = vcovCL, cluster = ~ind1 + ind2)

# calculate marginal effects using clustered standard errors
marginal_effects21 <- margins(
  int_m2,
  variables = "io_wiot_hun",
  at = list(sr_norm = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf21 <- data.table(summary(marginal_effects21)) %>%
  rename(
    x_values = sr_norm,
    y_values = factor
  )
mf21$dep_var <- "coagg_porter_rca01_stand_nuts4"
mf21$x_var <- "sr_norm"


# calculate marginal effects using clustered standard errors
marginal_effects22 <- margins(
  int_m2,
  variables = "sr_norm",
  at = list(io_wiot_hun = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf22 <- data.table(summary(marginal_effects22)) %>%
  rename(
    x_values = io_wiot_hun,
    y_values = factor
  )
mf22$dep_var <- "coagg_porter_rca01_stand_nuts4"
mf22$x_var <- "io_wiot_hun"



# interplot data generation -- transactions
# fit the linear regression model with clustered standard errors
int_m3 <- lm(egk_coagg_stand_nuts4 ~ io_norm3 * sr_norm + cr_norm, data = reg_df)
clustered_se <- vcovCL(int_m3, vcov = vcovCL, cluster = ~ind1 + ind2)


# calculate marginal effects using clustered standard errors
marginal_effects31 <- margins(
  int_m3,
  variables = "io_norm3",
  at = list(sr_norm = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf31 <- data.table(summary(marginal_effects31)) %>%
  rename(
    x_values = sr_norm,
    y_values = factor
  )
mf31$dep_var <- "egk_coagg_stand_nuts4"
mf31$x_var <- "sr_norm"


# calculate marginal effects using clustered standard errors
marginal_effects32 <- margins(
  int_m3,
  variables = "sr_norm",
  at = list(io_norm3 = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf32 <- data.table(summary(marginal_effects32)) %>%
  rename(
    x_values = io_norm3,
    y_values = factor
  )
mf32$dep_var <- "egk_coagg_stand_nuts4"
mf32$x_var <- "io_norm3"



# fit the linear regression model with clustered standard errors
int_m4 <- lm(coagg_porter_rca01_stand_nuts4 ~ io_norm3 * sr_norm + cr_norm, data = reg_df)
clustered_se <- vcovCL(int_m4, vcov = vcovCL, cluster = ~ind1 + ind2)

# calculate marginal effects using clustered standard errors
marginal_effects41 <- margins(
  int_m4,
  variables = "io_norm3",
  at = list(sr_norm = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf41 <- data.table(summary(marginal_effects41)) %>%
  rename(
    x_values = sr_norm,
    y_values = factor
  )
mf41$dep_var <- "coagg_porter_rca01_stand_nuts4"
mf41$x_var <- "sr_norm"


# calculate marginal effects using clustered standard errors
marginal_effects42 <- margins(
  int_m4,
  variables = "sr_norm",
  at = list(io_norm3 = seq(-1, 1, 0.1)), 
  vcov = clustered_se
)
mf42 <- data.table(summary(marginal_effects42)) %>%
  rename(
    x_values = io_norm3,
    y_values = factor
  )
mf42$dep_var <- "coagg_porter_rca01_stand_nuts4"
mf42$x_var <- "io_norm3"


mf_df <- rbind(mf11, mf12, mf21, mf22, mf31, mf32, mf41, mf42)


# export
write.table(mf_df,
            paste0("../../data/outputs/marginal_effects_for_interplots_with_citations.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)




# interaction mdoels -- patent X IO
int_m1 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * cr_norm + lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * cr_norm + lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m3 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * cr_norm + lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * cr_norm + lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)

int_m5 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * cr_norm + lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * cr_norm + lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m7 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * cr_norm + lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * cr_norm + lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)

etable(int_m1, int_m2, int_m3, int_m4, int_m5, int_m6, int_m7, int_m8)






### S4 -- Univariate OLS estimations for labor and input-output channels

# baseline table
reg_df <- combined_regression_df(regions, export = TRUE)

# LABOR
uols_labor_m1 <- feols(egk_coagg_stand_nuts3 ~ lab_stand,
                       cluster = ~ind1 + ind2,
                       data = reg_df)
uols_labor_m2 <- feols(egk_coagg_stand_nuts4 ~ lab_stand,
                       cluster = ~ind1 + ind2,
                       data = reg_df)
uols_labor_m3 <- feols(coagg_porter_rca01_stand_nuts3 ~ lab_stand,
                       cluster = ~ind1 + ind2,
                       data = reg_df)
uols_labor_m4 <- feols(coagg_porter_rca01_stand_nuts4 ~ lab_stand,
                       cluster = ~ind1 + ind2,
                       data = reg_df)


etable(
  uols_labor_m1, uols_labor_m2, uols_labor_m3, uols_labor_m4,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uols_labor_m1, uols_labor_m2, uols_labor_m3, uols_labor_m4,
       fitstat = c("r2", "ar2", "ivf1", "wh"))




# IO
uols_io_m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
uols_io_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
uols_io_m3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
uols_io_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)

uols_io_m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
uols_io_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
uols_io_m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
uols_io_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)



etable(
  uols_io_m1, uols_io_m2, uols_io_m3, uols_io_m4, uols_io_m5, uols_io_m6, uols_io_m7, uols_io_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uols_io_m1, uols_io_m2, uols_io_m3, uols_io_m4, uols_io_m5, uols_io_m6, uols_io_m7, uols_io_m8,
       fitstat = c("r2", "ar2"))






### S5 -- Stepwise instrumental variable estimations

# baseline table
reg_df <- combined_regression_df(regions, export = TRUE)

# first stage -- second stage -- LABOR
summary(fs_labor1 <- feols(
  lab_stand ~ iv_swe_lab_stand,
  cluster = ~ind1 + ind2,
  data = reg_df)
)
reg_df$first_stage_pred <- predict(fs_labor1, newdata = reg_df)
# cor(reg_df$lab_stand, reg_df$first_stage_pred)

# second stage
ss_labor2 <- feols(egk_coagg_stand_nuts3 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_labor3 <- feols(egk_coagg_stand_nuts4 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_labor4 <- feols(coagg_porter_rca01_stand_nuts3 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_labor5 <- feols(coagg_porter_rca01_stand_nuts4 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)

etable(
  fs_labor1, ss_labor2, ss_labor3, ss_labor4, ss_labor5,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
)




# first stage -- second stage -- IO WIOD
summary(fs_iowiod1 <- feols(
  io_wiot_hun_stand ~ iv_wiot_mean_stand,
  cluster = ~ind1 + ind2,
  data = reg_df)
)
reg_df$first_stage_pred <- predict(fs_iowiod1, newdata = reg_df)
# cor(reg_df$io_wiot_hun_stand, reg_df$first_stage_pred)

# second stage
ss_iowiod2 <- feols(egk_coagg_stand_nuts3 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_iowiod3 <- feols(egk_coagg_stand_nuts4 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_iowiod4 <- feols(coagg_porter_rca01_stand_nuts3 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_iowiod5 <- feols(coagg_porter_rca01_stand_nuts4 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)

etable(
  fs_iowiod1, ss_iowiod2, ss_iowiod3, ss_iowiod4, ss_iowiod5,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
)





# first stage -- second stage -- IO transactions
summary(fs_iot1 <- feols(
  io3_stand ~ iv_wiot_mean_stand,
  cluster = ~ind1 + ind2,
  data = reg_df)
)
reg_df$first_stage_pred <- predict(fs_iot1, newdata = reg_df)
# cor(reg_df$io_wiot_hun_stand, reg_df$first_stage_pred)

# second stage
ss_iot2 <- feols(egk_coagg_stand_nuts3 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_iot3 <- feols(egk_coagg_stand_nuts4 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_iot4 <- feols(coagg_porter_rca01_stand_nuts3 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)
ss_iot5 <- feols(coagg_porter_rca01_stand_nuts4 ~ first_stage_pred, cluster = ~ind1 + ind2, data = reg_df)

etable(
  fs_iot1, ss_iot2, ss_iot3, ss_iot4, ss_iot5,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1)
)







### S6 -- Alternative instruments for input-output connections

# baseline table
reg_df <- combined_regression_df(regions, export = TRUE)


# univar --  US supply
iv_us1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
iv_us2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
iv_us3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand  ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
iv_us4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)

iv_us5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
iv_us6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
iv_us7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
iv_us8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_us_supply_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)

etable(
  iv_us1, iv_us2, iv_us3, iv_us4, iv_us5, iv_us6, iv_us7, iv_us8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)



# univar --  CZE WIOD
iv_cze1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)
iv_cze2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)
iv_cze3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand  ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)
iv_cze4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)

iv_cze5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)
iv_cze6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)
iv_cze7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)
iv_cze8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_cze_stand,
                 cluster = ~ind1 + ind2,
                 data = reg_df)

etable(
  iv_cze1, iv_cze2, iv_cze3, iv_cze4, iv_cze5, iv_cze6, iv_cze7, iv_cze8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






### S7 -- Results for manufacturing and services


# multivar OLS manufacturing
mreg_df <- manufacturing_filter(reg_df)


manu1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
               #vcov = "HC1",
               cluster = ~ind1 + ind2,
               data = mreg_df)
manu2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)
manu3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)
manu4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)

manu5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)
manu6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)
manu7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)
manu8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand + lab_stand,
               cluster = ~ind1 + ind2,
               data = mreg_df)

etable(
  manu1, manu2, manu3, manu4, manu5, manu6, manu7, manu8,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(
  manu1, manu2, manu3, manu4, manu5, manu6, manu7, manu8
)  


# univar labor IV manufacturing
uiv_labor_manu1 <- feols(egk_coagg_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                         cluster = ~ind1 + ind2,
                         data = mreg_df)
uiv_labor_manu2 <- feols(egk_coagg_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                         cluster = ~ind1 + ind2,
                         data = mreg_df)

uiv_labor_manu3 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                         cluster = ~ind1 + ind2,
                         data = mreg_df)
uiv_labor_manu4 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                         cluster = ~ind1 + ind2,
                         data = mreg_df)

etable(
  uiv_labor_manu1, uiv_labor_manu2, uiv_labor_manu3, uiv_labor_manu4,
  fitstat = c("r2", "ar2", "ivfall", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uiv_labor_manu1, uiv_labor_manu2, uiv_labor_manu3, uiv_labor_manu4,
       fitstat = c("r2", "ar2", "ivf1", "wh"))





# univar IO IV manufacturing
uiv_io_manu1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                      #vcov = "HC1",
                      cluster = ~ind1 + ind2,
                      data = mreg_df)
uiv_io_manu2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)
uiv_io_manu3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)
uiv_io_manu4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)

uiv_io_manu5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)
uiv_io_manu6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)
uiv_io_manu7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)
uiv_io_manu8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                      cluster = ~ind1 + ind2,
                      data = mreg_df)


etable(
  uiv_io_manu1, uiv_io_manu2, uiv_io_manu3, uiv_io_manu4, uiv_io_manu5, uiv_io_manu6, uiv_io_manu7, uiv_io_manu8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)

etable(uiv_io_manu1, uiv_io_manu2, uiv_io_manu3, uiv_io_manu4, uiv_io_manu5, uiv_io_manu6, uiv_io_manu7, uiv_io_manu8)


# multivar OLS services
sreg_df <- services_filter(reg_df)
write.table(sreg_df,
            paste0("../outputs/services_reg_df_with_normalized_variables_2017.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)


servm1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)
servm2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)
servm3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)
servm4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)

servm5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)
servm6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)
servm7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)
servm8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = sreg_df)

etable(
  servm1, servm2, servm3, servm4, servm5, servm6, servm7, servm8,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(
  servm1, servm2, servm3, servm4, servm5, servm6, servm7, servm8
)  


# univar labor IV services
uiv_labor_servm1 <- feols(egk_coagg_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                          cluster = ~ind1 + ind2,
                          data = sreg_df)
uiv_labor_servm2 <- feols(egk_coagg_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                          cluster = ~ind1 + ind2,
                          data = sreg_df)

uiv_labor_servm3 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                          cluster = ~ind1 + ind2,
                          data = sreg_df)
uiv_labor_servm4 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                          cluster = ~ind1 + ind2,
                          data = sreg_df)

etable(
  uiv_labor_servm1, uiv_labor_servm2, uiv_labor_servm3, uiv_labor_servm4,
  fitstat = c("r2", "ar2", "ivfall", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uiv_labor_servm1, uiv_labor_servm2, uiv_labor_servm3, uiv_labor_servm4,
       fitstat = c("r2", "ar2", "ivf1", "wh"))





# univar IO IV services
uiv_io_servm1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)
uiv_io_servm2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)
uiv_io_servm3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)
uiv_io_servm4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)

uiv_io_servm5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)
uiv_io_servm6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)
uiv_io_servm7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)
uiv_io_servm8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                       cluster = ~ind1 + ind2,
                       data = sreg_df)


etable(
  uiv_io_servm1, uiv_io_servm2, uiv_io_servm3, uiv_io_servm4, uiv_io_servm5, uiv_io_servm6, uiv_io_servm7, uiv_io_servm8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






### S8 -- Results for knowledge intensive services

# baseline table
reg_df <- combined_regression_df(regions, export = TRUE)


# KIS table
kis_table <- fread("../../data/databank_oc//KIS_industries_2digit.csv")
kis_table$KIS01 <- 1

reg_df <- merge(
  reg_df,
  kis_table,
  by.x = "ind1_2d",
  by.y = "ind",
  all.x = TRUE,
  all.y = FALSE
)
reg_df <- merge(
  reg_df,
  kis_table,
  by.x = "ind2_2d",
  by.y = "ind",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_1", "_2")
)
reg_df$KIS01_1[is.na(reg_df$KIS01_1)==1] <- 0
reg_df$KIS01_2[is.na(reg_df$KIS01_2)==1] <- 0

kis_subset <- subset(reg_df, (KIS01_1 == 1) & KIS01_2 == 1)
not_kis_subset <- subset(reg_df, (KIS01_1 == 0) & KIS01_2 == 0)


### --- Table 2 -- KIS (knowledge-intensive services)
m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
            #vcov = "HC1",
            cluster = ~ind1 + ind2,
            data = kis_subset)
m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)
m3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)
m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)

m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)
m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)
m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)
m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = kis_subset)

etable(
  m1, m2, m3, m4, m5, m6, m7, m8,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)



# univar labor IV -- KIS
uiv_labor_kis1 <- feols(egk_coagg_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = kis_subset)
uiv_labor_kis2 <- feols(egk_coagg_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = kis_subset)

uiv_labor_kis3 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = kis_subset)
uiv_labor_kis4 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = kis_subset)

etable(
  uiv_labor_kis1, uiv_labor_kis2, uiv_labor_kis3, uiv_labor_kis4,
  fitstat = c("r2", "ar2", "ivfall", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uiv_labor_kis1, uiv_labor_kis2, uiv_labor_kis3, uiv_labor_kis4,
       fitstat = c("r2", "ar2", "ivf1", "wh"))






# univar IO IV KIS
uiv_io_kis1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     #vcov = "HC1",
                     cluster = ~ind1 + ind2,
                     data = kis_subset)
uiv_io_kis2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)
uiv_io_kis3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)
uiv_io_kis4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)

uiv_io_kis5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)
uiv_io_kis6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)
uiv_io_kis7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)
uiv_io_kis8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = kis_subset)


etable(
  uiv_io_kis1, uiv_io_kis2, uiv_io_kis3, uiv_io_kis4, uiv_io_kis5, uiv_io_kis6, uiv_io_kis7, uiv_io_kis8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)

etable(uiv_io_kis1, uiv_io_kis2, uiv_io_kis3, uiv_io_kis4, uiv_io_kis5, uiv_io_kis6, uiv_io_kis7, uiv_io_kis8)





### --- Table 2 -- non-KIS (knowledge-intensive services)
m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
            #vcov = "HC1",
            cluster = ~ind1 + ind2,
            data = not_kis_subset)
m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)
m3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)
m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)

m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)
m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)
m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)
m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = not_kis_subset)

etable(
  m1, m2, m3, m4, m5, m6, m7, m8,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(m1, m2, m3, m4, m5, m6, m7, m8)


# univar labor IV -- KIS
uiv_labor_kis1 <- feols(egk_coagg_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = not_kis_subset)
uiv_labor_kis2 <- feols(egk_coagg_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = not_kis_subset)

uiv_labor_kis3 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = not_kis_subset)
uiv_labor_kis4 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                        cluster = ~ind1 + ind2,
                        data = not_kis_subset)

etable(
  uiv_labor_kis1, uiv_labor_kis2, uiv_labor_kis3, uiv_labor_kis4,
  fitstat = c("r2", "ar2", "ivfall", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uiv_labor_kis1, uiv_labor_kis2, uiv_labor_kis3, uiv_labor_kis4,
       fitstat = c("r2", "ar2", "ivf1", "wh"))



# univar IO IV KIS
uiv_io_kis1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     #vcov = "HC1",
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)
uiv_io_kis2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)
uiv_io_kis3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)
uiv_io_kis4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)

uiv_io_kis5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)
uiv_io_kis6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)
uiv_io_kis7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)
uiv_io_kis8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = not_kis_subset)


etable(
  uiv_io_kis1, uiv_io_kis2, uiv_io_kis3, uiv_io_kis4, uiv_io_kis5, uiv_io_kis6, uiv_io_kis7, uiv_io_kis8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)

etable(uiv_io_kis1, uiv_io_kis2, uiv_io_kis3, uiv_io_kis4, uiv_io_kis5, uiv_io_kis6, uiv_io_kis7, uiv_io_kis8)



### -- interaction models -- KIS
int_m1 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = kis_subset)
int_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = kis_subset)
int_m3 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = kis_subset)
int_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = kis_subset)

int_m5 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = kis_subset)
int_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = kis_subset)
int_m7 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = kis_subset)
int_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = kis_subset)


etable(int_m1, int_m2, int_m3, int_m4, int_m5, int_m6, int_m7, int_m8)

etable(
  int_m1, int_m2, int_m3, int_m4, int_m5, int_m6, int_m7, int_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = FALSE
)






### S9 -- Geographic restrictions for robustness checks


# BUDAPEST excluded
path <- paste0("../../data/databank_oc/04oc_data_nuts4_2017.csv")
path2 <- paste0("../../data/oc15_2023_dec/04b_oc_data_budapest_excluded_", regions[2], "_", focal_year, ".csv")
bpe_df <- prep_alternative_table(path, path2)


# multivar OLS wo/ BP
bpe_m1 <- feols(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = bpe_df)
bpe_m2 <- feols(egk_coagg_stand ~ io3_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = bpe_df)

bpe_m3 <- feols(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = bpe_df)
bpe_m4 <- feols(coagg_porter_rca01_stand ~ io3_stand + lab_stand,
                cluster = ~ind1 + ind2,
                data = bpe_df)

etable(
  bpe_m1, bpe_m2, bpe_m3, bpe_m4,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)


# combined IV
uiv_io_bpe1 <- feols(egk_coagg_stand ~ 1 | lab_stand ~ iv_swe_lab_stand,
                     cluster = ~ind1 + ind2,
                     data = bpe_df)
uiv_io_bpe2 <- feols(egk_coagg_stand ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = bpe_df)
uiv_io_bpe3 <- feols(egk_coagg_stand ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = bpe_df)
uiv_io_bpe4 <- feols(coagg_porter_rca01_stand ~ 1 | lab_stand ~ iv_swe_lab_stand,
                     cluster = ~ind1 + ind2,
                     data = bpe_df)
uiv_io_bpe5 <- feols(coagg_porter_rca01_stand ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = bpe_df)
uiv_io_bpe6 <- feols(coagg_porter_rca01_stand ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = bpe_df)

etable(
  uiv_io_bpe1, uiv_io_bpe2, uiv_io_bpe3, uiv_io_bpe4, uiv_io_bpe5, uiv_io_bpe6,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






# single plant companies ONLY
path <- paste0("../../data/databank_oc/04oc_data_nuts4_2017.csv")
path2 <- paste0("../../data/oc15_2023_dec/04d_oc_data_single_plant_", regions[2], "_", focal_year, ".csv")
single_df <- prep_alternative_table(path, path2)


# multivar OLS wo/ BP
single_m1 <- feols(egk_coagg_stand ~ io_wiot_hun_stand + lab_stand,
                   cluster = ~ind1 + ind2,
                   data = single_df)
single_m2 <- feols(egk_coagg_stand ~ io3_stand + lab_stand,
                   cluster = ~ind1 + ind2,
                   data = single_df)

single_m3 <- feols(coagg_porter_rca01_stand ~ io_wiot_hun_stand + lab_stand,
                   cluster = ~ind1 + ind2,
                   data = single_df)
single_m4 <- feols(coagg_porter_rca01_stand ~ io3_stand + lab_stand,
                   cluster = ~ind1 + ind2,
                   data = single_df)

etable(
  single_m1, single_m2, single_m3, single_m4,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(single_m1, single_m2, single_m3, single_m4)


# combined IV
uiv_single1 <- feols(egk_coagg_stand ~ 1 | lab_stand ~ iv_swe_lab_stand,
                     cluster = ~ind1 + ind2,
                     data = single_df)
uiv_single2 <- feols(egk_coagg_stand ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = single_df)
uiv_single3 <- feols(egk_coagg_stand ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = single_df)
uiv_single4 <- feols(coagg_porter_rca01_stand ~ 1 | lab_stand ~ iv_swe_lab_stand,
                     cluster = ~ind1 + ind2,
                     data = single_df)
uiv_single5 <- feols(coagg_porter_rca01_stand ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = single_df)
uiv_single6 <- feols(coagg_porter_rca01_stand ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                     cluster = ~ind1 + ind2,
                     data = single_df)

etable(
  uiv_single1, uiv_single2, uiv_single3, uiv_single4, uiv_single5, uiv_single6,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)

etable(uiv_single1, uiv_single2, uiv_single3, uiv_single4, uiv_single5, uiv_single6)






### S10 -- Multivariate instrumental variable regressions

# baseline table
reg_df <- combined_regression_df(regions, export = TRUE)


# multivar -- clustered SE
iv_cse_mu1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
iv_cse_mu2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
iv_cse_mu3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
iv_cse_mu4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)

iv_cse_mu5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
iv_cse_mu6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
iv_cse_mu7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)
iv_cse_mu8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    cluster = ~ind1 + ind2,
                    data = reg_df)

etable(
  iv_cse_mu1, iv_cse_mu2, iv_cse_mu3, iv_cse_mu4, iv_cse_mu5, iv_cse_mu6, iv_cse_mu7, iv_cse_mu8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)





# multivar -- robust SE
iv_rob_mu1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)
iv_rob_mu2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)
iv_rob_mu3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)
iv_rob_mu4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)

iv_rob_mu5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)
iv_rob_mu6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)
iv_rob_mu7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)
iv_rob_mu8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
                    vcov = "HC1",
                    data = reg_df)

etable(
  iv_rob_mu1, iv_rob_mu2, iv_rob_mu3, iv_rob_mu4, iv_rob_mu5, iv_rob_mu6, iv_rob_mu7, iv_rob_mu8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






### S11 -- Alternative specifications for our models with interactions


# NUTS3 version
reg_df <- combined_regression_df(regions, export = TRUE)

sint_m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
sint_m2 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
sint_m3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
sint_m4 <- feols(egk_coagg_stand_nuts3 ~ io3_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)

sint_m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
sint_m6 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
sint_m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
sint_m8 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)


etable(sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8)

etable(
  sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)




# manufacturing
mreg_df <- manufacturing_filter(reg_df)

sint_m1 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)
sint_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)
sint_m3 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)
sint_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)

sint_m5 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)
sint_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)
sint_m7 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)
sint_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = mreg_df)


etable(sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8)

etable(
  sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)



# services
sreg_df <- services_filter(reg_df)

sint_m1 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)
sint_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)
sint_m3 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)
sint_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)

sint_m5 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)
sint_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)
sint_m7 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)
sint_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = sreg_df)


etable(sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8)


etable(
  sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)








# budapest excluded
path <- paste0("../../data/databank_oc/04oc_data_nuts4_2017.csv")
path2 <- paste0("../../data/oc15_2023_dec/04b_oc_data_budapest_excluded_", regions[2], "_", focal_year, ".csv")
bpe_df <- prep_alternative_table(path, path2)


sint_m1 <- feols(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)
sint_m2 <- feols(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)
sint_m3 <- feols(egk_coagg_stand ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)
sint_m4 <- feols(egk_coagg_stand ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)

sint_m5 <- feols(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)
sint_m6 <- feols(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)
sint_m7 <- feols(coagg_porter_rca01_stand ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)
sint_m8 <- feols(coagg_porter_rca01_stand ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = bpe_df)


etable(sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8)

etable(
  sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)




# single plant companies ONLY
path <- paste0("../../data/databank_oc/04oc_data_nuts4_2017.csv")
path2 <- paste0("../../data/oc15_2023_dec/04d_oc_data_single_plant_", regions[2], "_", focal_year, ".csv")
single_df <- prep_alternative_table(path, path2)


sint_m1 <- feols(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = single_df)
sint_m2 <- feols(egk_coagg_stand ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = single_df)
sint_m3 <- feols(egk_coagg_stand ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = single_df)
sint_m4 <- feols(egk_coagg_stand ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = single_df)

sint_m5 <- feols(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = single_df)
sint_m6 <- feols(coagg_porter_rca01_stand ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = single_df)
sint_m7 <- feols(coagg_porter_rca01_stand ~ io3_stand * lab_stand,
                 cluster = ~ind1 + ind2,
                 data = single_df)
sint_m8 <- feols(coagg_porter_rca01_stand ~ io3_stand * lab_stand | ind1 + ind2,
                 cluster = ~ind1 + ind2,
                 data = single_df)


etable(sint_m1, sint_m2, sint_m3, sint_m4, sint_m5, sint_m6, sint_m7, sint_m8)




