# coagglomeration based on Mcp -- regressions
# by sandorjuhasz


library(data.table)
library(dplyr)
library(stargazer)

# parameters
reg <- "megye"
ind <- 3
ye <- 18


# read data -- prepared in m01_data_prep.R
mdf2 <- fread("../data/oc_2023_january/oc_m01_data_output_megye.csv")


# variable manipulation
mdf2$log_total_value[is.na(mdf2$log_total_value)==1] <- 0

mdf2$log_nr_conn[is.na(mdf2$log_nr_conn)==1] <- 0
mdf2$log_nr_conn_mne[is.na(mdf2$log_nr_conn_mne)==1] <- 0
mdf2$log_nr_conn_local[is.na(mdf2$log_nr_conn_local)==1] <- 0
mdf2$log_nr_conn_mixed[is.na(mdf2$log_nr_conn_mixed)==1] <- 0

mdf2$log_nr_firms1 <- log10(mdf2$nr_firms1)
mdf2$log_nr_firms2 <- log10(mdf2$nr_firms2)
mdf2$log_nr_mnes1 <- log10(mdf2$nr_mnes1)
mdf2$log_nr_mnes2 <- log10(mdf2$nr_mnes2)
mdf2$log_nr_locals1 <- log10(mdf2$nr_locals1)
mdf2$log_nr_locals2 <- log10(mdf2$nr_locals2)





mod1 <- lm(coagg ~ log_nr_conn, data = mdf2)
mod2 <- lm(coagg ~ relatedness, data = mdf2)
mod1c <- lm(coagg ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = mdf2)
mod2c <- lm(coagg ~ relatedness+ log_nr_firms1 + log_nr_firms2, data = mdf2)
mod3 <- lm(coagg ~ log_nr_conn + relatedness+ log_nr_firms1 + log_nr_firms2, data = mdf2)

stargazer(mod1,
          mod2,
          mod1c,
          mod2c,
          mod3,
          dep.var.labels = "Coagglomeration",
          dep.var.caption = "",
          covariate.labels = c("Nr transaction ties (log)", "Skill relatedness", "Nr firms1", "Nr firms2"),
          omit.stat = c("f", "ser"),
          out="../outputs/m02_regressions-01.html")



mne_mod1 <- lm(coagg_mne ~ log_nr_conn_mne, data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0))
mne_mod2 <- lm(coagg_mne ~ relatedness, data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0))
mne_mod1c <- lm(coagg_mne ~ log_nr_conn_mne + log_nr_mnes1 + log_nr_mnes2, data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0))
mne_mod2c <- lm(coagg_mne ~ relatedness + log_nr_mnes1 + log_nr_mnes2, data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0))
mne_mod3 <- lm(coagg_mne ~ log_nr_conn_mne + relatedness + log_nr_mnes1 + log_nr_mnes2, data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0))

stargazer(mne_mod1,
          mne_mod2,
          mne_mod1c,
          mne_mod2c,
          mne_mod3,
          dep.var.labels = "Coagglomeration of MNEs",
          dep.var.caption = "",
          covariate.labels = c("Nr transactions betw MNEs (log)", "Skill relatedness", "Nr MNE1", "Nr MNE2"),
          omit.stat = c("f", "ser"),
          out="../outputs/m02_regressions-02-mne.html")


dom_mod1 <- lm(coagg_local ~ log_nr_conn_local, data = subset(mdf2, nr_locals1 > 0 & nr_locals2 > 0))
dom_mod2 <- lm(coagg_local ~ relatedness, data = subset(mdf2, nr_locals1 > 0 & nr_locals2 > 0))
dom_mod1c <- lm(coagg_local ~ log_nr_conn_local + log_nr_locals1 + log_nr_locals2, data = subset(mdf2, nr_locals1 > 0 & nr_locals2 > 0))
dom_mod2c <- lm(coagg_local ~ relatedness + log_nr_locals1 + log_nr_locals2, data = subset(mdf2, nr_locals1 > 0 & nr_locals2 > 0))
dom_mod3 <- lm(coagg_local ~ log_nr_conn_local + relatedness + log_nr_locals1 + log_nr_locals2, data = subset(mdf2, nr_locals1 > 0 & nr_locals2 > 0))

stargazer(dom_mod1,
          dom_mod2,
          dom_mod1c,
          dom_mod2c,
          dom_mod3,
          dep.var.labels = "Coagglomeration of domestic firms",
          dep.var.caption = "",
          covariate.labels = c("Nr transactions betw dom (log)", "Skill relatedness", "Nr dom1", "Nr dom2"),
          omit.stat = c("f", "ser"),
          out="../outputs/m02_regressions-03-domestic.html")


mix_mod1 <- lm(coagg_mne_local ~ log_nr_conn_mixed, data = mdf2)
mix_mod2 <- lm(coagg_mne_local ~ relatedness, data = mdf2)
mix_mod1c <- lm(coagg_mne_local ~ log_nr_conn_mixed + log_nr_firms1 + log_nr_firms2, data = mdf2)
mix_mod2c <- lm(coagg_mne_local ~ relatedness+ log_nr_firms1 + log_nr_firms2, data = mdf2)
mix_mod3 <- lm(coagg_mne_local ~ log_nr_conn_mixed + relatedness+ log_nr_firms1 + log_nr_firms2, data = mdf2)

stargazer(mix_mod1,
          mix_mod2,
          mix_mod1c,
          mix_mod2c,
          mix_mod3,
          dep.var.labels = "Coagglomeration mixed",
          dep.var.caption = "",
          covariate.labels = c("Nr transactions betw dom and MNE (log)", "Skill relatedness", "Nr firms1", "Nr firms2"),
          omit.stat = c("f", "ser"),
          out="../outputs/m02_regressions-04-mixed.html")







# baseline correlations
summary(lm(coagg ~ log_nr_conn, data = mdf2))
summary(lm(coagg ~ relatedness, data = mdf2))

# controlls
summary(lm(coagg ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg ~ relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))

# final-like
summary(lm(coagg ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))

# mne version
summary(lm(coagg_mne ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne ~ log_nr_conn_mne + log_nr_mnes1 + log_nr_mnes2,
           data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0)))

summary(lm(coagg_mne ~ relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))

summary(lm(coagg_mne ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne ~ log_nr_conn_mne + relatedness + log_nr_firms1 + log_nr_firms2,
           data = subset(mdf2, nr_mnes1 > 0 & nr_mnes2 > 0)))



summary(lm(coagg_local ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_local ~ log_nr_conn_local + log_nr_firms1 + log_nr_firms2, data = mdf2))

summary(lm(coagg_local ~ relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_local ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_local ~ log_nr_conn_local + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))

summary(lm(coagg_mne_local ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne_local ~ log_nr_conn_mixed + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne_local ~ relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne_local ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne_local ~ log_nr_conn_mixed + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))










# baseline correlations
summary(lm(coagg ~ log_total_value, data = mdf2))
summary(lm(coagg ~ relatedness, data = mdf2))

# from controlls to full models
summary(lm(coagg ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg ~ relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))


# MNE version
summary(lm(coagg_mne ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))

summary(lm(coagg_local ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_local ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))

summary(lm(coagg_mne_local ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = mdf2))
summary(lm(coagg_mne_local ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = mdf2))








# test -- EGK coagglomeration and Mpp coagg correlation
egk_table <- fread("../outputs/coagglomeration_nace3d_megye_totalemp.csv")

test <- merge(
  mpp_norm_df,
  egk_table,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
summary(lm(test$coagg ~ test$coagglomeration))
test2 <- subset(test, is.na(coagglomeration)==0)
cor(test2$coagg, test2$coagglomeration)








test_df$coagg_mne[is.na(test_df$coagg_mne) == 1] <- 0
test_df$coagg_local[is.na(test_df$coagg_local) == 1] <- 0
test_df$coagg_mne_local[is.na(test_df$coagg_mne_local) == 1] <- 0
cor(test_df$coagg, test_df$coagg_mne)
cor(test_df$coagg, test_df$coagg_local)
cor(test_df$coagg_mne, test_df$coagg_local)
cor(test_df$coagg, test_df$coagg_mne_local)

















# Mcp matrix based on RCA01
merleg <- data.table(read.dta(paste0("../data/merleg", ye, "_latin_sznev.dta")))

# rearrange and filter the original merleg data
firms <- reshape_merleg_data(merleg)
rm(merleg)
gc()

# add geographic info to firms -- with name corrections -- agglom_data is the required table
telecodes <- fread("../data/teruleti_kodok.csv")
agglom_data <- fread("../data/agglomerations_data.csv")
firms <- location_info_to_firm_data(firms, telecodes, agglom_data, include_agglomeration = TRUE)

# drop jaras_kod 999 (OR jaras_nev "") if relevant
if(reg == "jaras"){
  firms <- subset(firms, jaras_kod != 999)
} else
{
  firms <- firms
}

# keep firms in agglomerations if relevant
if(reg == "agglom"){
  firms <- subset(firms, is.na(agglom_name) != 1)
} else
{
  firms <- firms
}



# MNE identification
firms$mne <- ifelse((firms$emp >= mne_min_emp) & (firms$foreign_share >= mne_min_foreign_share), 1, 0)
firms$mne[is.na(firms$mne) == 1] <- 0




# create industry-region table with aggregate info
full_ir_df <- create_indreg_fullcomb(firms, region_level = reg, industry_level = ind)
ir_df <- create_indreg_aggregate_table(firms, region_level = reg, industry_level = ind)
colnames(ir_df)[1:2] <- c("reg", "ind")

ir_df <- merge(
  full_ir_df,
  ir_df,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)

# calculate RCA values
ir_df[is.na(ir_df)==1] <- 0
ir_df$rca <- rca_calculation(ir_df, ind_var = "ind", reg_var = "reg", emp_var = "total_emp")
ir_df$rca01 <- ifelse(ir_df$rca >= 1, 1, 0)



# Mcp matrix
mcp <- create_industry_region_matrix(ir_df,
                                     region_col = "reg",
                                     industry_col = "ind",
                                     weight_col = "rca01")

# raw mcc
mpp_raw <- t(mcp) %*% mcp
mpp_raw_df <- subset(data.table(reshape2::melt(mpp_raw)), value > 0)
colnames(mpp_raw_df) <- c("ind1", "ind2", "coagg")

# normalized mcc
mpp_norm <- as.matrix(coo_normalized(mcp))
#mpp_norm_df <- subset(data.table(reshape2::melt(mpp_norm)), value > 0)
mpp_norm_df <- data.table(reshape2::melt(mpp_norm))
colnames(mpp_norm_df) <- c("ind1", "ind2", "coagg")




# test -- EGK coagglomeration and Mpp coagg correlation
egk_table <- fread("../outputs/coagglomeration_nace3d_megye_totalemp.csv")

test <- merge(
  mpp_norm_df,
  egk_table,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
summary(lm(test$coagg ~ test$coagglomeration))
test2 <- subset(test, is.na(coagglomeration)==0)
cor(test2$coagg, test2$coagglomeration)



# MNE only
mnes <- subset(firms, mne == 1)

set.seed(421)
mne_ids <- c(mnes$azonosito)
mnes_sample <- sample(mne_ids, sample_size, replace = FALSE)
mnes_sample_df <- subset(firms, azonosito %in% mnes_sample)

full_ir_df <- create_indreg_fullcomb(firms, region_level = reg, industry_level = ind)
#mne_ir_df <- create_indreg_aggregate_table(mnes, region_level = reg, industry_level = ind)
mne_ir_df <- create_indreg_aggregate_table(mnes_sample_df, region_level = reg, industry_level = ind)
colnames(mne_ir_df)[1:2] <- c("reg", "ind")

mne_ir_df <- merge(
  full_ir_df,
  mne_ir_df,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)

# calculate RCA values
mne_ir_df[is.na(mne_ir_df)==1] <- 0
mne_ir_df$rca <- rca_calculation(mne_ir_df, ind_var = "ind", reg_var = "reg", emp_var = "total_emp")
mne_ir_df$rca01 <- ifelse(mne_ir_df$rca >= 1, 1, 0)



# Mcp matrix
mne_mcp <- create_industry_region_matrix(mne_ir_df,
                                         region_col = "reg",
                                         industry_col = "ind",
                                         weight_col = "rca01")


# normalized mcc
mne_mpp_norm <- as.matrix(coo_normalized(mne_mcp))
#mne_mpp_norm_df <- subset(data.table(reshape2::melt(mne_mpp_norm)), value > 0)
mne_mpp_norm_df <- data.table(reshape2::melt(mne_mpp_norm))
colnames(mne_mpp_norm_df) <- c("ind1", "ind2", "coagg")



# local only
locals <-subset(firms, mne == 0)

set.seed(421)
local_ids <- c(locals$azonosito)
locals_sample <- sample(local_ids, 2500, replace = FALSE)
locals_sample_df <- subset(firms, azonosito %in% locals_sample)


full_ir_df <- create_indreg_fullcomb(firms, region_level = reg, industry_level = ind)
#local_ir_df <- create_indreg_aggregate_table(locals, region_level = reg, industry_level = ind)
local_ir_df <- create_indreg_aggregate_table(locals_sample_df, region_level = reg, industry_level = ind)
colnames(local_ir_df)[1:2] <- c("reg", "ind")

local_ir_df <- merge(
  full_ir_df,
  local_ir_df,
  by = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)

# calculate RCA values
local_ir_df[is.na(local_ir_df)==1] <- 0
local_ir_df$rca <- rca_calculation(local_ir_df, ind_var = "ind", reg_var = "reg", emp_var = "total_emp")
local_ir_df$rca01 <- ifelse(local_ir_df$rca >= 1, 1, 0)



# Mcp matrix
local_mcp <- create_industry_region_matrix(local_ir_df,
                                           region_col = "reg",
                                           industry_col = "ind",
                                           weight_col = "rca01")


# normalized mcc
local_mpp_norm <- as.matrix(coo_normalized(local_mcp))
#local_mpp_norm_df <- subset(data.table(reshape2::melt(local_mpp_norm)), value > 0)
local_mpp_norm_df <- data.table(reshape2::melt(local_mpp_norm))
colnames(local_mpp_norm_df) <- c("ind1", "ind2", "coagg")





# mixed -- mne %*% local
ml_mpp_norm <- as.matrix(coo_mixed_normalized(mne_mcp, local_mcp, assoc))
#mne_mpp_norm_df <- subset(data.table(reshape2::melt(mne_mpp_norm)), value > 0)
ml_mpp_norm_df <- data.table(reshape2::melt(ml_mpp_norm))
colnames(ml_mpp_norm_df) <- c("ind1", "ind2", "coagg")






# test correlations
test_df <- merge(
  mpp_norm_df,
  mne_mpp_norm_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("", "_mne")
)
test_df <- merge(
  test_df,
  local_mpp_norm_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("", "_local")
)
test_df <- merge(
  test_df,
  ml_mpp_norm_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("", "_mne_local")
)



test_df$coagg_mne[is.na(test_df$coagg_mne) == 1] <- 0
test_df$coagg_local[is.na(test_df$coagg_local) == 1] <- 0
test_df$coagg_mne_local[is.na(test_df$coagg_mne_local) == 1] <- 0
cor(test_df$coagg, test_df$coagg_mne)
cor(test_df$coagg, test_df$coagg_local)
cor(test_df$coagg_mne, test_df$coagg_local)
cor(test_df$coagg, test_df$coagg_mne_local)


# industry - industry transactions
el18 <- fread("../data/transaction_edgelist2018.csv")

# keep firms in agglomerations if relevant
if(reg == "agglom"){
  firms <- subset(firms, is.na(agglom_name) != 1)
} else
{
  firms <- firms
}


# ADD firm info to edgelist
edges <- merge(el18, firms, by.x = "firm1", by.y = "azonosito", all.x = TRUE, all.y = FALSE)
edges <- merge(edges, firms, by.x = "firm2", by.y = "azonosito", suffixes = c(1, 2), all.x = TRUE, all.y = FALSE)

# drop 128 transactions w/ negative buy value
edges <- subset(edges, buy_value > 0)

# drop rows with missing industry and region information
edges <- subset(edges, (is.na(nace2d1) != 1) & (is.na(nace2d2) != 1))
edges <- subset(edges, (is.na(megye_kod1) != 1) & (is.na(megye_kod2) != 1))
edges <- subset(edges, jaras_nev1 != "" & jaras_nev2 != "")



tr_indind <- ind_ind_edgelist_aggregation(edges, ind_var1 = "nace3d1", ind_var2 = "nace3d2")
tr_indind$zscore_value <- z_score(tr_indind$total_value)
tr_indind$log_total_value <- log10(tr_indind$total_value)
tr_indind$log_nr_conn <- log10(tr_indind$nr_connection)

# add nr firms
indreg_df <- firms %>%
  group_by(nace3d) %>%
  summarise(nr_firms = n_distinct(azonosito)) %>%
  data.table()


# add skill relatedness magic
sr_data <- fread("../data/SR_avg.csv")
rel_df <- create_3digit_SR_table(sr_data)
rel_df$ind1 <- as.integer(rel_df$ind1)
rel_df$ind2 <- as.integer(rel_df$ind2)
rel_df <- unique(data.table(rbind(rel_df, dplyr::rename(rel_df, ind1 = ind2, ind2 = ind1))))


# combine everything
test_df2 <- merge(
  test_df,
  tr_indind,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)
test_df2 <- merge(
  test_df2,
  indreg_df,
  by.x = "ind1",
  by.y = "nace3d",
  all.x = TRUE,
  all.y = FALSE
)
test_df2 <- merge(
  test_df2,
  indreg_df,
  by.x = "ind2",
  by.y = "nace3d",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("1", "2")
)
test_df2 <- merge(
  test_df2,
  rel_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)



# regressions
test_df2$log_total_value[is.na(test_df2$log_total_value)==1] <- 0
test_df2$log_nr_conn[is.na(test_df2$log_nr_conn)==1] <- 0
test_df2$log_nr_firms1 <- log10(test_df2$nr_firms1)
test_df2$log_nr_firms2 <- log10(test_df2$nr_firms2)
summary(lm(coagg ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))

summary(lm(coagg_mne ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_mne ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))

summary(lm(coagg_local ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_local ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))

summary(lm(coagg_mne_local ~ log_total_value + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_mne_local ~ log_total_value + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))



summary(lm(coagg ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg ~ relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))

summary(lm(coagg_mne ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_mne ~ relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_mne ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))

summary(lm(coagg_local ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_local ~ relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_local ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))

summary(lm(coagg_mne_local ~ log_nr_conn + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_mne_local ~ relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))
summary(lm(coagg_mne_local ~ log_nr_conn + relatedness + log_nr_firms1 + log_nr_firms2, data = test_df2))





## iv test

swe_sr_df <- fread("../data/relatedness/srnet_nat_3dig_avg_13_19.csv")

mdf3 <- merge(
  mdf2,
  swe_sr_df,
  by.x = c("ind1", "ind2"),
  by.y = c("ind_i", "ind_j"),
  all.x = TRUE,
  all.y = FALSE
)
mdf3$sr_norm[is.na(mdf3$sr_norm)==1] <- 0

summary(mod1 <- lm(coagg ~ relatedness, data = mdf3))
summary(mod1 <- lm(coagg ~ sr_norm, data = mdf3))

library(ivreg)
summary(ivreg(coagg ~ relatedness | sr_norm, data = mdf3))
# 