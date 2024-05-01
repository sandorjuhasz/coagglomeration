# regression tables for the paper -- sandorjuhasz
# setup is the same as in the Databank


library(emmeans)
library(data.table)
library(dplyr)
library(lmtest)
library(sandwich)
library(fixest)
library(interplot)
library(cowplot)
library(lmtest)
library(margins)


source("../code/02_regression_functions.R")



# parameters
focal_year <- 2017
regions <- c("nuts3", "nuts4")



# data prep
reg_df <- combined_regression_df(regions, export = TRUE)



### --- Table 2 -- clustered SE
m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
            #vcov = "HC1",
            cluster = ~ind1 + ind2,
            data = reg_df)
m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)
m3 <- feols(egk_coagg_stand_nuts3 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)
m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)

m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)
m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)
m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)
m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand + lab_stand,
            cluster = ~ind1 + ind2,
            data = reg_df)

etable(
  m1, m2, m3, m4, m5, m6, m7, m8,
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)








### -- Table 3 -- Labor IV -- univariate
uiv_labor_m1 <- feols(egk_coagg_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                      #vcov = "HC1",
                      cluster = ~ind1 + ind2,
                      data = reg_df)
uiv_labor_m2 <- feols(egk_coagg_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                      cluster = ~ind1 + ind2,
                      data = reg_df)

uiv_labor_m3 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                      cluster = ~ind1 + ind2,
                      data = reg_df)
uiv_labor_m4 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | lab_stand ~ iv_swe_lab_stand,
                      cluster = ~ind1 + ind2,
                      data = reg_df)

etable(
  uiv_labor_m1, uiv_labor_m2, uiv_labor_m3, uiv_labor_m4,
  fitstat = c("r2", "ar2", "ivfall", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)
etable(uiv_labor_m1, uiv_labor_m2, uiv_labor_m3, uiv_labor_m4,
       fitstat = c("r2", "ar2", "ivf1", "wh"))





### -- Table 4 -- IO IV -- univariate
uiv_io_m1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                   #vcov = "HC1",
                   cluster = ~ind1 + ind2,
                   data = reg_df)
uiv_io_m2 <- feols(egk_coagg_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)
uiv_io_m3 <- feols(egk_coagg_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)
uiv_io_m4 <- feols(egk_coagg_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)

uiv_io_m5 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)
uiv_io_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io_wiot_hun_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)
uiv_io_m7 <- feols(coagg_porter_rca01_stand_nuts3 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)
uiv_io_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ 1 | io3_stand ~ iv_wiot_mean_stand,
                   cluster = ~ind1 + ind2,
                   data = reg_df)


etable(
  uiv_io_m1, uiv_io_m2, uiv_io_m3, uiv_io_m4, uiv_io_m5, uiv_io_m6, uiv_io_m7, uiv_io_m8,
  fitstat = c("r2", "ar2", "f", "wh"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)





### -- Table 5 --interactions
int_m1 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m2 <- feols(egk_coagg_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m3 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m4 <- feols(egk_coagg_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)

int_m5 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m6 <- feols(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m7 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand,
                cluster = ~ind1 + ind2,
                data = reg_df)
int_m8 <- feols(coagg_porter_rca01_stand_nuts4 ~ io3_stand * lab_stand | ind1 + ind2,
                cluster = ~ind1 + ind2,
                data = reg_df)


etable(int_m1, int_m2, int_m3, int_m4, int_m5, int_m6, int_m7, int_m8)

etable(
  int_m1, int_m2, int_m3, int_m4, int_m5, int_m6, int_m7, int_m8,
  fitstat = c("r2", "ar2"),
  digits = 3,
  digits.stats = 3,
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
  tex = TRUE
)






### Interplot data generation -- WIOD
# fit the linear regression model with clustered standard errors
int_m1 <- lm(egk_coagg_stand_nuts4 ~ io_wiot_hun * sr_norm, data = reg_df)
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
int_m2 <- lm(coagg_porter_rca01_stand_nuts4 ~ io_wiot_hun * sr_norm, data = reg_df)
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




### Interplot data generation -- transactions
# fit the linear regression model with clustered standard errors
int_m3 <- lm(egk_coagg_stand_nuts4 ~ io_norm3 * sr_norm, data = reg_df)
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
int_m4 <- lm(coagg_porter_rca01_stand_nuts4 ~ io_norm3 * sr_norm, data = reg_df)
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
            paste0("../outputs/marginal_effects_for_interplots.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)

