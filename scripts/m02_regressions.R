## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz



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
reg <- "megye"
ind <- 3
ye <- 18

# read data -- prepared in m01_data_prep.R
#mdf3 <- fread("../outputs/m01_data_output.csv", sep = ";")
#mdf3 <- fread("../outputs/m01_data_output_manufacturing_only.csv", sep = ";")
#mdf3 <- fread("../outputs/m01_data_output_services_only.csv", sep = ";")
#mdf3 <- fread("../outputs/m01_data_output_manufacturing_and_services.csv", sep = ";")
#mdf3 <- fread("../outputs/m01_data_output_jaras_version.csv", sep = ";")
mdf3 <- fread("../data/oc_2023_march_labor/m01_data_output_OC.csv", sep = ";")

# remove self loops and repeated pairs
mdf3 <- subset(mdf3, ind1 < ind2)

# variable manipulation -- nr firms in industries
mdf3$log_nr_firms1 <- log10(mdf3$nr_firms1)
mdf3$log_nr_firms2 <- log10(mdf3$nr_firms2)


# variable manipulation -- transactions between industries
mdf3$log_undir_value <- log10(mdf3$undir_value)
mdf3$log_undir_swe_io <- log10(mdf3$undir_swe_io)
mdf3$log_undir_value[is.infinite(mdf3$log_undir_value) == 1] <- 0
mdf3$log_undir_swe_io[is.infinite(mdf3$log_undir_swe_io) == 1] <- 0


# variable manipulation -- labor flow between industries
#mdf3$log_undir_lab_value[is.infinite(mdf3$log_undir_lab_value) == 1] <- 0

# create an industry pair ID 
mdf3$ind_pair_id <- paste0(mdf3$ind1, "-", mdf3$ind2)



# baseline models -- EGK
summary(egk_m01 <- lm(egk_coagg ~ log_undir_value, data = mdf3))
summary(egk_m02 <- lm(egk_coagg ~ hun_sr_norm, data = mdf3))
summary(egk_m03 <- lm(egk_coagg ~ log_undir_value + hun_sr_norm, data = mdf3))
summary(egk_m04 <- lm(egk_coagg ~ log_undir_value + hun_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))

summary(egk_m05 <- lm(egk_coagg ~ log_undir_swe_io, data = mdf3))
summary(egk_m06 <- lm(egk_coagg ~ swe_sr_norm, data = mdf3))
summary(egk_m07 <- lm(egk_coagg ~ log_undir_swe_io + swe_sr_norm, data = mdf3))
summary(egk_m08 <- lm(egk_coagg ~ log_undir_swe_io + swe_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))


stargazer(egk_m01,
          egk_m02,
          egk_m03,
          egk_m04,
          egk_m05,
          egk_m06,
          egk_m07,
          egk_m08,
          omit.stat = c("f"),
          out = "../outputs/regression_outputs/egk_undir_full_jaras_version.html")


# interaction models
summary(egk_inter <- lm(egk_coagg ~ log_undir_value * hun_sr_norm, data = mdf3))

# interplot 1 
title <- "interplot_SR_transactions"
file_name <- paste0("../figures/", title, ".png")
png(file_name, width=800, height=600, units = 'px')

interplot(m = egk_inter, var1 = "hun_sr_norm", var2 = "log_undir_value", size=3) +
  xlab("Transaction value (log)") +
  ylab("Estimated coefficient for\nskill relatedness") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=20), axis.title=element_text(size=25))
dev.off()


# interplot 2
title <- "interplot_transactions_SR"
file_name <- paste0("../figures/", title, ".png")
png(file_name, width=800, height=600, units = 'px')

interplot(m = egk_inter, var1 = "log_undir_value", var2 = "hun_sr_norm", size=3) +
  xlab("Skill relatedness") +
  ylab("Estimated coefficient for\ntransaction value (log)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=20), axis.title=element_text(size=25))
dev.off()



# baseline models -- coagg
summary(coagg_m01 <- lm(coagg ~ log_undir_value, data = mdf3))
summary(coagg_m02 <- lm(coagg ~ hun_sr_norm, data = mdf3))
summary(coagg_m03 <- lm(coagg ~ log_undir_value + hun_sr_norm, data = mdf3))
summary(coagg_m04 <- lm(coagg ~ log_undir_value + hun_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))

summary(coagg_m05 <- lm(coagg ~ log_undir_swe_io, data = mdf3))
summary(coagg_m06 <- lm(coagg ~ swe_sr_norm, data = mdf3))
summary(coagg_m07 <- lm(coagg ~ log_undir_swe_io + swe_sr_norm, data = mdf3))
summary(coagg_m08 <- lm(coagg ~ log_undir_swe_io + swe_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))


stargazer(coagg_m01,
          coagg_m02,
          coagg_m03,
          coagg_m04,
          coagg_m05,
          coagg_m06,
          coagg_m07,
          coagg_m08,
          omit.stat = c("f"),
          out = "../outputs/regression_outputs/coagg_undir_full.txt")



# multivariates with IV
summary(iv_egk_m03 <- ivreg::ivreg(egk_coagg ~ log_undir_value + hun_sr_norm | log_undir_swe_io + swe_sr_norm, data = mdf3))
coeftest(iv_egk_m03, vcov = vcovCL, cluster = ~ind_pair_id)
#stargazer(coeftest(iv_egk_m03, vcov = vcovCL, cluster = ~ind_pair_id),
#          out = "../outputs/regression_outputs/egk_undir_full_model3_iv_clusterred_coeffs.txt")
stargazer(coeftest(iv_egk_m03, vcov = vcovCL, cluster = ~ind_pair_id),
          out = "../outputs/regression_outputs/egk_undir_full_model3_iv_clusterred_coeffs_jaras_version.html")


summary(iv_egk_m04 <- ivreg::ivreg(egk_coagg ~ log_undir_value + hun_sr_norm + log_nr_firms1 + log_nr_firms2 | log_undir_swe_io + swe_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))
coeftest(iv_egk_m04, vcov = vcovCL, cluster = ~ind_pair_id)
#stargazer(coeftest(iv_egk_m04, vcov = vcovCL, cluster = ~ind_pair_id),
#          out = "../outputs/regression_outputs/egk_undir_full_model4_iv_clusterred_coeffs.txt")
stargazer(coeftest(iv_egk_m04, vcov = vcovCL, cluster = ~ind_pair_id),
          out = "../outputs/regression_outputs/egk_undir_full_model4_iv_clusterred_coeffs_jaras_version.html")



summary(iv_coagg_m03 <- ivreg::ivreg(coagg ~ log_undir_value + hun_sr_norm | log_undir_swe_io + swe_sr_norm, data = mdf3))
coeftest(iv_coagg_m03, vcov = vcovCL, cluster = ~ind_pair_id)
stargazer(coeftest(iv_coagg_m03, vcov = vcovCL, cluster = ~ind_pair_id),
          out = "../outputs/regression_outputs/coagg_undir_full_model3_iv_clusterred_coeffs.txt")

summary(iv_coagg_m04 <- ivreg::ivreg(coagg ~ log_undir_value + hun_sr_norm + log_nr_firms1 + log_nr_firms2 | log_undir_swe_io + swe_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))
coeftest(iv_coagg_m04, vcov = vcovCL, cluster = ~ind_pair_id)
stargazer(coeftest(iv_coagg_m04, vcov = vcovCL, cluster = ~ind_pair_id),
          out = "../outputs/regression_outputs/coagg_undir_full_model4_iv_clusterred_coeffs.txt")








