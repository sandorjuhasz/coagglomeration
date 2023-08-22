# regression on outchecked data -- sandorjuhasz


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
region <- "nuts4"
ind <- 3
year <- 2017
version <- ""
manuf_focus <- FALSE


# file from OC
mdf3 <- fread(paste0("../data/oc09_2023_aug/oc_mdf3_", region, "_", year, "_based.csv"), sep = ";")

# remove self loops and repeated pairs
mdf3 <- subset(mdf3, ind1 < ind2)

# variable manipulation -- for regressions
mdf3$egk_coagg <- scale(mdf3$egk_coagg)
mdf3$egk_coagg_mne <- scale(mdf3$egk_coagg_mne)
mdf3$egk_coagg_local <- scale(mdf3$egk_coagg_local)

mdf3$coagg_porter <- scale(mdf3$coagg_porter)
mdf3$coagg_porter_mne <- scale(mdf3$coagg_porter_mne)
mdf3$coagg_porter_local <- scale(mdf3$coagg_porter_local)
mdf3$coagg_porter_mne_local <- scale(mdf3$coagg_porter_mne_local)

mdf3$log_nr_firms1 <- log10(mdf3$nr_firms1)
mdf3$log_nr_firms2 <- log10(mdf3$nr_firms2)

mdf3$lab_standard <- scale(mdf3$sr_norm)


# drop rows with NAs -- create an equal sample
mdf3 <- subset(mdf3, is.na(coagg_porter_mne)==0)

if(manuf_focus == TRUE){
  mdf3 <- subset(mdf3, (ind1 > 100) & (ind1 < 350))
  mdf3 <- subset(mdf3, (ind2 > 100) & (ind2 < 350))
} else
{
  mdf3
}


# baseline models -- EGK
summary(egk_m01 <- lm(egk_coagg ~ io_log_standard, data = mdf3))
summary(egk_m02 <- lm(egk_coagg ~ lab_standard, data = mdf3))
summary(egk_m03 <- lm(egk_coagg ~ io_log_standard + lab_standard, data = mdf3))
summary(egk_m04 <- lm(egk_coagg ~ io_log_standard + lab_standard + log_nr_firms1 + log_nr_firms2, data = mdf3))

stargazer(egk_m01,
          egk_m02,
          egk_m03,
          egk_m04,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_egk_baseline", version, ".html"))


# baseline models -- Porter
summary(porter_m01 <- lm(coagg_porter ~ io_log_standard, data = mdf3))
summary(porter_m02 <- lm(coagg_porter ~ lab_standard, data = mdf3))
summary(porter_m03 <- lm(coagg_porter ~ io_log_standard + lab_standard, data = mdf3))
summary(porter_m04 <- lm(coagg_porter ~ io_log_standard + lab_standard + log_nr_firms1 + log_nr_firms2, data = mdf3))

stargazer(porter_m01,
          porter_m02,
          porter_m03,
          porter_m04,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_porter_baseline", version, ".html"))



# set up the IV part here



# mne / domestic models -- EGK
summary(egk_mm01 <- lm(egk_coagg ~ io_log_standard + lab_standard, data = mdf3))
summary(egk_mm02 <- lm(egk_coagg_mne ~ io_log_standard + lab_standard, data = mdf3))
summary(egk_mm03 <- lm(egk_coagg_local ~ io_log_standard + lab_standard, data = mdf3))



stargazer(egk_mm01,
          egk_mm02,
          egk_mm03,
          omit.stat = c("f"),
          out = "../outputs/regression_tables/local_egk_mne_domestic_exploration_manuf.html")




# mne / domestic models -- porter
summary(porter_mm01 <- lm(coagg_porter ~ io_log_standard + lab_standard, data = mdf3))
summary(porter_mm02 <- lm(coagg_porter_mne ~ io_log_standard + lab_standard, data = mdf3))
summary(porter_mm03 <- lm(coagg_porter_local ~ io_log_standard + lab_standard, data = mdf3))
summary(porter_mm04 <- lm(coagg_porter_mne_local ~ io_log_standard + lab_standard, data = mdf3))




stargazer(porter_mm01,
          porter_mm02,
          porter_mm03,
          porter_mm04,
          omit.stat = c("f"),
          out = "../outputs/regression_tables/local_porter_mne_domestic_exploration_manuf.html")



summary(egk_inter01 <- lm(egk_coagg ~ io_log_standard * lab_standard, data = mdf3))
summary(egk_inter02 <- lm(egk_coagg_mne ~ io_log_standard * lab_standard, data = mdf3))
summary(egk_inter03 <- lm(egk_coagg_local ~ io_log_standard * lab_standard, data = mdf3))
summary(po_inter01 <- lm(coagg_porter ~ io_log_standard * lab_standard, data = mdf3))
summary(po_inter02 <- lm(coagg_porter_mne ~ io_log_standard * lab_standard, data = mdf3))
summary(po_inter03 <- lm(coagg_porter_local ~ io_log_standard * lab_standard, data = mdf3))
summary(po_inter04 <- lm(coagg_porter_mne_local ~ io_log_standard * lab_standard, data = mdf3))


stargazer(egk_inter01,
          egk_inter02,
          egk_inter03,
          omit.stat = c("f"),
          out = "../outputs/regression_tables/local_egk_interaction_models.html")


stargazer(po_inter01,
          po_inter02,
          po_inter03,
          po_inter04,
          omit.stat = c("f"),
          out = "../outputs/regression_tables/local_porter_interaction_models.html")





# interaction models
summary(egk_inter <- lm(egk_coagg ~ log_undir_value * hun_sr_norm, data = mdf3))
summary(coagg_inter <- lm(coagg ~ log_undir_value * hun_sr_norm, data = mdf3))

mdf3$undir_value_q75 <- ifelse(
  mdf3$log_undir_value > quantile(mdf3$log_undir_value)[4],
  1,
  0
)
mdf3$hun_sr_q75 <- ifelse(
  mdf3$hun_sr_norm > quantile(mdf3$hun_sr_norm)[4],
  1,
  0
)
summary(egk_du1 <- lm(egk_coagg ~ undir_value_q75 + hun_sr_q75, data = mdf3))
summary(egk_du1 <- lm(egk_coagg ~ undir_value_q75 * hun_sr_q75, data = mdf3))


# MNE / domestic interactions
summary(coagg_inter <- lm(coagg ~ log_undir_value * hun_sr_norm, data = mdf3))
summary(coagg_inter <- lm(coagg_mne ~ log_undir_value * hun_sr_norm, data = mdf3))
summary(coagg_inter <- lm(coagg_local ~ log_undir_value * hun_sr_norm, data = mdf3))
summary(coagg_inter <- lm(coagg_mne_local ~ log_undir_value * hun_sr_norm, data = mdf3))


# interplot 1 
title <- "interplot_SR_transactions"
file_name <- paste0("../figures/", title, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = egk_inter, var1 = "hun_sr_norm", var2 = "log_undir_value", size=3) +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nskill relatedness") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "interplot_transactions_SR"
file_name <- paste0("../figures/", title, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = egk_inter, var1 = "log_undir_value", var2 = "hun_sr_norm", size=3) +
  xlab("Skill relatedness") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
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




### MNE x coagg models

summary(coagg_m01 <- lm(coagg ~ log_undir_value, data = mdf3))
summary(coagg_m02 <- lm(coagg ~ hun_sr_norm, data = mdf3))
summary(coagg_m03 <- lm(coagg ~ log_undir_value + hun_sr_norm, data = mdf3))
summary(coagg_m04 <- lm(coagg ~ log_undir_value + hun_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))

summary(coagg_m05 <- lm(coagg ~ log_undir_swe_io, data = mdf3))
summary(coagg_m06 <- lm(coagg ~ swe_sr_norm, data = mdf3))
summary(coagg_m07 <- lm(coagg ~ log_undir_swe_io + swe_sr_norm, data = mdf3))
summary(coagg_m08 <- lm(coagg ~ log_undir_swe_io + swe_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))


summary(coagg_mne_m01 <- lm(coagg_mne ~ log_undir_value, data = mdf3))
summary(coagg_mne_m02 <- lm(coagg_mne ~ hun_sr_norm, data = mdf3))
summary(coagg_mne_m03 <- lm(coagg_mne ~ log_undir_value + hun_sr_norm, data = mdf3))
#summary(coagg_mne_m04 <- lm(coagg_mne ~ log_undir_value + hun_sr_norm + log_nr_firms1 + log_nr_firms2, data = mdf3))


summary(coagg_m03 <- lm(coagg ~ log_undir_value + hun_sr_norm, data = mdf3))
summary(coagg_mne_m03 <- lm(coagg_mne ~ log_undir_value + hun_sr_norm, data = mdf3))
summary(coagg_local_m03 <- lm(coagg_local ~ log_undir_value + hun_sr_norm, data = mdf3))
summary(coagg_mixed_m03 <- lm(coagg_mne_local ~ log_undir_value + hun_sr_norm, data = mdf3))

attr(mdf3, "names")[5:9] <- c("M_coagg", "EGK coagg", "M_coagg MNE", "M_coagg domestic", "M_coagg MNE x domestic")

stargazer(
  coagg_m03,
  coagg_mne_m03,
  coagg_local_m03,
  coagg_mixed_m03,
  omit.stat=c("f", "ser"),
  #column.labels = c("M_coagg", "M_coagg", "M_coagg", "M_coagg"),
  dep.var.labels = attr(mdf3, "model.varnames")[6:9],
  dep.var.caption = c(""),
  covariate.labels = c("IO connections", "Labor flow"),
  out = "../outputs/regression_tables/coagg_mne_local_versions.tex"
)


summary(iv_coagg_m03 <- ivreg::ivreg(coagg ~ log_undir_value + hun_sr_norm | log_undir_swe_io + swe_sr_norm, data = mdf3))
coff_m03 <- coeftest(iv_coagg_m03, vcov = vcovCL, cluster = ~ind_pair_id)
summary(iv_coagg_mne_m03 <- ivreg::ivreg(coagg_mne ~ log_undir_value + hun_sr_norm | log_undir_swe_io + swe_sr_norm, data = mdf3))
coff_mne_m03 <- coeftest(iv_coagg_mne_m03, vcov = vcovCL, cluster = ~ind_pair_id)
summary(iv_coagg_local_m03 <- ivreg::ivreg(coagg_local ~ log_undir_value + hun_sr_norm | log_undir_swe_io + swe_sr_norm, data = mdf3))
coff_local_m03 <- coeftest(iv_coagg_local_m03, vcov = vcovCL, cluster = ~ind_pair_id)
summary(iv_coagg_mne_local_m03 <- ivreg::ivreg(coagg_mne_local ~ log_undir_value + hun_sr_norm | log_undir_swe_io + swe_sr_norm, data = mdf3))
coeff_mne_local_m03 <- coeftest(iv_coagg_mne_local_m03, vcov = vcovCL, cluster = ~ind_pair_id)

stargazer(,
          out = "../outputs/regression_outputs/coagg_undir_full_model3_iv_clusterred_coeffs.txt")


coeftest(coagg_m03, vcov = vcovCL, cluster = ~ind_pair_id)
coeftest(coagg_mne_m03, vcov = vcovCL, cluster = ~ind_pair_id)
coeftest(coagg_local_m03, vcov = vcovCL, cluster = ~ind_pair_id)
coeftest(coagg_mixed_m03, vcov = vcovCL, cluster = ~ind_pair_id)





# correlation of coagg measures
mne_valid <- subset(mdf3, is.na(coagg_mne) == 0)
cor(mne_valid$coagg, mne_valid$coagg_mne)
cor(mne_valid$coagg, mne_valid$coagg_local)
cor(mne_valid$coagg, mne_valid$coagg_mne_local)



