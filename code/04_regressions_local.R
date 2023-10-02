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
#region <- "nuts4"
region <- "nuts4"
ind <- 3
year <- 2017
version <- ""
manuf_focus <- FALSE
manuf_and_services <- FALSE
drop_agric <- FALSE
drop_public <- FALSE
wiot_iv <- FALSE
us_iv <- TRUE
io_iv_country_code <- "CZE"
#io_iv_country_code <- "USA"
#io_iv_country_code <- "SWE"
#io_iv_country_code <- "HUN"
#io_iv_country_code <- "SVK"


# file from OC
mdf3 <- fread(paste0("../data/oc10_2023_sep/oc_mdf3_", region, "_", year, "_based.csv"), sep = ";")


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

mdf3$coagg <- scale(mdf3$coagg)
mdf3$coagg_mne <- scale(mdf3$coagg_mne)
mdf3$coagg_local <- scale(mdf3$coagg_local)
mdf3$coagg_mne_local <- scale(mdf3$coagg_mne_local)

mdf3$io_standard <- scale(mdf3$io_norm)
mdf3$swe_io_standard <- scale(mdf3$swe_io_norm)
mdf3$lab_standard <- scale(mdf3$sr_norm)
mdf3$swe_lab_standard <- scale(mdf3$swe_sr_norm)

mdf3$io01 <- ifelse(mdf3$io_norm > 0, 1, 0)
mdf3$labor01 <- ifelse(mdf3$sr_norm > 0, 1, 0)

mdf3$ind_pair_id <- paste0(mdf3$ind1, "_", mdf3$ind2)




# add IO IVs from wiot
if(wiot_iv == TRUE){
  iv_el <- fread("../outputs/wiot_edgelist_2_digit.csv")
  iv_el <- subset(iv_el, c_code == io_iv_country_code)
  
  mdf3$ind1_2d <- mdf3$ind1 %/% 10
  mdf3$ind2_2d <- mdf3$ind2 %/% 10
  
  mdf3 <- merge(
    mdf3,
    iv_el,
    by.x = c("ind1_2d", "ind2_2d"),
    by.y = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  mdf3$iv_io_standard <- scale(mdf3$iv_io_norm)
}

# add more countries
#cze_iv_io_standard <- mdf3$iv_io_standard
#usa_iv_io_standard <- mdf3$iv_io_standard
#swe_iv_io_standard <- mdf3$iv_io_standard
#hun_iv_io_standard <- mdf3$iv_io_standard
#mdf3$cze_iv_io_standard <- cze_iv_io_standard
#mdf3$usa_iv_io_standard <- usa_iv_io_standard
#mdf3$swe_iv_io_standard <- swe_iv_io_standard
#mdf3$hun_iv_io_standard <- swe_iv_io_standard
# summary(wiot_test <- lm(usa_iv_io_standard ~ hun_iv_io_standard, data = mdf3))



# add US supply IV
if(us_iv == TRUE){
  iv_us <- fread("../outputs/us_supply_3digit_nace_nace.csv")
  
  mdf3 <- merge(
    mdf3,
    dplyr::select(iv_us, ind1, ind2, iv_io_norm),
    by = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  mdf3$iv_io_standard <- scale(mdf3$iv_io_norm)
}




# drop rows with NAs -- create an equal sample
mdf3 <- subset(mdf3, is.na(coagg_porter_mne)==0 & is.na(coagg_porter_local)==0)


# industry filters
if(manuf_focus == TRUE){
  mdf3 <- subset(mdf3, (ind1 > 100) & (ind1 < 350))
  mdf3 <- subset(mdf3, (ind2 > 100) & (ind2 < 350))
}

if(manuf_and_services == TRUE){
  mdf3m <- subset(mdf3, (ind1 > 100) & (ind1 < 350))
  mdf3m <- subset(mdf3m, (ind2 > 100) & (ind2 < 350))
  
  mdf3s <- subset(mdf3, (ind1 > 490) & (ind1 < 840))
  mdf3s <- subset(mdf3s, (ind2 > 490) & (ind2 < 840))
  
  mdf3ms1 <- subset(mdf3, (ind1 > 100) & (ind1 < 350))
  mdf3ms1 <- subset(mdf3ms1, (ind2 > 490) & (ind2 < 840))
  mdf3ms2 <- subset(mdf3, (ind1 > 490) & (ind1 < 840))
  mdf3ms2 <- subset(mdf3ms2, (ind2 > 100) & (ind2 < 350))
  
  mdf3 <- rbind(mdf3m, mdf3s, mdf3ms1, mdf3ms2)
}


if(drop_public == TRUE){
  mdf3 <- subset(mdf3, (ind1 < 840))
  mdf3 <- subset(mdf3, (ind2 < 840))
}

if(drop_agric == TRUE){
  mdf3 <- subset(mdf3, (ind1 > 100))
  mdf3 <- subset(mdf3, (ind2 > 100))
}



# baseline models -- EGK
summary(egk_m01 <- lm(egk_coagg ~ io_standard, data = mdf3))
summary(egk_m02 <- lm(egk_coagg ~ lab_standard, data = mdf3))
summary(egk_m03 <- lm(egk_coagg ~ io_standard + lab_standard, data = mdf3))
#summary(egk_m04 <- lm(egk_coagg ~ io_standard + lab_standard + log_nr_firms1 + log_nr_firms2, data = mdf3))


# baseline models -- Porter
summary(porter_m01 <- lm(coagg_porter ~ io_standard, data = mdf3))
summary(porter_m02 <- lm(coagg_porter ~ lab_standard, data = mdf3))
summary(porter_m03 <- lm(coagg_porter ~ io_standard + lab_standard, data = mdf3))


stargazer(egk_m01,
          egk_m02,
          egk_m03,
          porter_m01,
          porter_m02,
          porter_m03,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_egk_porter_baseline_", year, "_", region, version, ".html"))


# set up the IV part here
# summary(lm(lab_standard ~ swe_lab_standard, data = mdf3))

summary(lm(io_standard ~ iv_io_standard, data = mdf3))
summary(lm(io_standard ~ swe_io_standard, data = mdf3))
summary(lm(lab_standard ~ swe_lab_standard, data = mdf3))


summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard | iv_io_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard | swe_io_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3))

# summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | io_standard + swe_lab_standard, data = mdf3))
# summary(iv_egk <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | io_standard + swe_lab_standard, data = mdf3))




summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard | swe_io_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ lab_standard | swe_lab_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ sr_norm | swe_sr_norm, data = mdf3))
# summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | io_standard + swe_lab_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard + lab_standard | swe_io_standard + swe_lab_standard, data = mdf3))
coeftest(iv_egk, vcov = vcovCL, cluster = ~ind_pair_id)
#stargazer(coeftest(iv_egk, vcov = vcovCL, cluster = ~ind_pair_id),
#          out = paste0("../outputs/regression_tables/local_egk_iv_", year, "_", region, version, ".html"))


summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_norm + sr_norm | swe_io_norm + swe_sr_norm, data = mdf3))

summary(iv_egk <- ivreg::ivreg(coagg_porter ~ io_standard | swe_io_standard, data = mdf3))
summary(iv_porter <- ivreg::ivreg(coagg_porter ~ lab_standard | swe_lab_standard, data = mdf3))
summary(iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | swe_io_standard + swe_lab_standard, data = mdf3))

summary(base00 <- lm(coagg_porter ~ io_standard, data = mdf3))
summary(iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard | iv_io_standard, data = mdf3))
summary(iv_porter <- ivreg::ivreg(coagg_porter ~ lab_standard | swe_lab_standard, data = mdf3))
summary(iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | swe_io_standard + swe_lab_standard, data = mdf3))
summary(iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | iv_io_standard + swe_lab_standard, data = mdf3))


summary(iv_porter <- ivreg::ivreg(coagg_porter ~ io_standard + lab_standard | swe_io_standard + swe_lab_standard, data = subset(mdf3, (io_norm != -1) & (swe_io_norm != -1))))

coeftest(iv_porter, vcov = vcovCL, cluster = ~ind_pair_id)
#stargazer(coeftest(iv_porter, vcov = vcovCL, cluster = ~ind_pair_id),
#          out = paste0("../outputs/regression_tables/local_porter_iv_", year, "_", region, version, ".html"))


# multiple countries
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ lab_standard | swe_lab_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard | cze_iv_io_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ io_standard | cze_iv_io_standard + usa_iv_io_standard + swe_iv_io_standard, data = mdf3))
summary(iv_egk <- ivreg::ivreg(egk_coagg ~ lab_standard + io_standard | swe_lab_standard + cze_iv_io_standard + usa_iv_io_standard + swe_iv_io_standard, data = mdf3))







# mne / domestic models -- EGK
summary(egk_mm01 <- lm(egk_coagg ~ io_standard + lab_standard, data = mdf3))
summary(egk_mm02 <- lm(egk_coagg_mne ~ io_standard + lab_standard, data = mdf3))
summary(egk_mm03 <- lm(egk_coagg_local ~ io_standard + lab_standard, data = mdf3))

stargazer(egk_mm01,
          egk_mm02,
          egk_mm03,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_egk_mne_", year, "_", region, version, ".html"))


# mne / domestic models -- porter
summary(porter_mm01 <- lm(coagg_porter ~ io_standard + lab_standard, data = mdf3))
summary(porter_mm02 <- lm(coagg_porter_mne ~ io_standard + lab_standard, data = mdf3))
summary(porter_mm03 <- lm(coagg_porter_local ~ io_standard + lab_standard, data = mdf3))
summary(porter_mm04 <- lm(coagg_porter_mne_local ~ io_standard + lab_standard, data = mdf3))

stargazer(porter_mm01,
          porter_mm02,
          porter_mm03,
          porter_mm04,
          dep.var.labels = c("Porter coagg", "MNE-MNE coagg", "Dom-dom coagg", "MNE-dom coagg"),
          dep.var.caption = "",
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt",
          covariate.labels = c("IO connections",
                               "Labor flow"),
          out = paste0("../outputs/regression_tables/local_porter_mne_", year, "_", region, version, ".html"))




# interactions -- baseline -- EGK and Porter
summary(inter01 <- lm(egk_coagg ~ io_standard * lab_standard, data = mdf3))
summary(inter02 <- lm(coagg_porter ~ io_standard * lab_standard, data = mdf3))
#summary(inter03 <- lm(egk_coagg ~ io01 * labor01, data = mdf3))
#summary(inter04 <- lm(coagg_porter ~ io01 * labor01, data = mdf3))


stargazer(inter01,
          inter02,
          dep.var.labels = c("EGK coagg", "Porter coagg"),
          dep.var.caption = "",
          omit.stat=c("f", "ser"),
          column.sep.width = "10pt",
          covariate.labels = c("IO connections",
                               "Labor flow",
                               "IO connections x Labor flow"),
          out = paste0("../outputs/regression_tables/local_egk_porter_interacttion", year, "_", region, version, ".html"))







# interactions for mne / domestic -- EGK
summary(egk_inter01 <- lm(egk_coagg ~ io_standard * lab_standard, data = mdf3))
summary(egk_inter02 <- lm(egk_coagg_mne ~ io_standard * lab_standard, data = mdf3))
summary(egk_inter03 <- lm(egk_coagg_local ~ io_standard * lab_standard, data = mdf3))

stargazer(egk_inter01,
          egk_inter02,
          egk_inter03,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_egk_interaction_", year, "_", region, version, ".html"))

summary(egk_inter04 <- lm(egk_coagg ~ io01 * labor01, data = mdf3))
summary(egk_inter05 <- lm(egk_coagg_mne ~ io01 * labor01, data = mdf3))
summary(egk_inter06 <- lm(egk_coagg_local ~ io01 * labor01, data = mdf3))

stargazer(egk_inter04,
          egk_inter05,
          egk_inter06,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_egk_interaction01_", year, "_", region, version, ".html"))


# interactions for mne / domestic -- porter
summary(p_inter01 <- lm(coagg_porter ~ io_standard * lab_standard, data = mdf3))
summary(p_inter02 <- lm(coagg_porter_mne ~ io_standard * lab_standard, data = mdf3))
summary(p_inter03 <- lm(coagg_porter_local ~ io_standard * lab_standard, data = mdf3))
summary(p_inter04 <- lm(coagg_porter_mne_local ~ io_standard * lab_standard, data = mdf3))

stargazer(p_inter01,
          p_inter02,
          p_inter03,
          p_inter04,
          dep.var.caption = "",
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Porter coagg", "MNE-MNE coagg", "Dom-dom coagg", "MNE-dom coagg"),
          column.sep.width = "10pt",
          covariate.labels = c("IO connections",
                               "Labor flow",
                               "IO connections x Labor flow"),
          out = paste0("../outputs/regression_tables/local_porter_interaction_", year, "_", region, version, ".html"))

summary(p_inter05 <- lm(coagg_porter ~ io01 * labor01, data = mdf3))
summary(p_inter06 <- lm(coagg_porter_mne ~ io01 * labor01, data = mdf3))
summary(p_inter07 <- lm(coagg_porter_local ~ io01 * labor01, data = mdf3))
summary(p_inter08 <- lm(coagg_porter_mne_local ~ io01 * labor01, data = mdf3))

stargazer(p_inter05,
          p_inter06,
          p_inter07,
          p_inter08,
          omit.stat=c("f", "ser"),
          out = paste0("../outputs/regression_tables/local_porter_interaction01_", year, "_", region, version, ".html"))



# for interplots -- EGK
summary(egk_interplot <- lm(egk_coagg ~ io_norm * sr_norm, data = mdf3))

# interplot 1 
title <- "interplot_SR_transactions"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = egk_interplot, var1 = "sr_norm", var2 = "io_norm", size=3) +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nlabor flow") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "interplot_transactions_SR"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = egk_interplot, var1 = "io_norm", var2 = "sr_norm", size=3) +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# for interplots -- porter
summary(porter_interplot <- lm(coagg_porter ~ io_norm * sr_norm, data = mdf3))

# interplot 1 
title <- "interplot_porter_SR_transactions"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_interplot, var1 = "sr_norm", var2 = "io_norm", size=3) +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nlabor flow") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "interplot_porter_transactions_SR"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_interplot, var1 = "io_norm", var2 = "sr_norm", size=3) +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# for interplots -- porter MNE-MNE
summary(porter_mne_interplot <- lm(coagg_porter_mne ~ io_norm * sr_norm, data = mdf3))

# interplot 1 
title <- "interplot_porter_SR_transactions_mne"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_mne_interplot, var1 = "sr_norm", var2 = "io_norm", size=3) +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nlabor flow") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "interplot_porter_transactions_SR_mne"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_mne_interplot, var1 = "io_norm", var2 = "sr_norm", size=3) +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# for interplots -- porter domestic-domestic
summary(porter_local_interplot <- lm(coagg_porter_local ~ io_norm * sr_norm, data = mdf3))

# interplot 1 
title <- "interplot_porter_SR_transactions_domestic"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_local_interplot, var1 = "sr_norm", var2 = "io_norm", size=3) +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nlabor flow") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "interplot_porter_transactions_SR_domestic"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_local_interplot, var1 = "io_norm", var2 = "sr_norm", size=3) +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()



# for interplots -- porter MNE-domestic
summary(porter_mne_local_interplot <- lm(coagg_porter_mne_local ~ io_norm * sr_norm, data = mdf3))

# interplot 1 
title <- "interplot_porter_SR_transactions_mne_domestic"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_mne_local_interplot, var1 = "sr_norm", var2 = "io_norm", size=3) +
  xlab("IO transactions") +
  ylab("Estimated coefficient for\nlabor flow") +
  #theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()


# interplot 2
title <- "interplot_porter_transactions_SR_mne_domestic"
file_name <- paste0("../figures/", title, "_", region, ".png")
png(file_name, width=600, height=600, units = 'px')

interplot(m = porter_mne_local_interplot, var1 = "io_norm", var2 = "sr_norm", size=3) +
  xlab("Labor flow") +
  ylab("Estimated coefficient for\nIO transactions") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1.5) +
  theme_cowplot(12) +
  theme(axis.text = element_text(size=30), axis.title=element_text(size=40))
dev.off()






# baseline models -- coagg -- NONSENSE
summary(coagg_m01 <- lm(coagg ~ io_standard, data = mdf3))
summary(coagg_m02 <- lm(coagg ~ lab_standard, data = mdf3))
summary(coagg_m03 <- lm(coagg ~ io_standard + lab_standard, data = mdf3))

