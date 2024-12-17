###### revision related calculation -- sandorjuhasz



###### 01 patent data ######
###### 02 patent data ######



library(data.table)
library(dplyr)
library(fixest)
source("../code/02_regression_functions.R")



# parameters
focal_year <- 2017
regions <- c("nuts3", "nuts4")



###### 01 patent data ######



###### patents in 2015-2017 in Hungary ######
epo_df <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_IPC.txt")

# focal period
epo_df <- subset(epo_df, (prio_year<=focal_year))
#epo_df <- subset(epo_df, (prio_year>=2015) & (prio_year<=2017))

# applicant region
#app_df <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_App_reg.txt")
app_df <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_Inv_reg.txt")
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
            paste0("../outputs/epo_until_2017_Hungary.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)
rm(list=ls())
gc()





###### citations ######

# EPO patents in Hungary 2015-2017
epo_df <- fread("../outputs/epo_until_2017_Hungary.csv")
epo_df <- dplyr::select(epo_df, appln_id, ctry_code) %>% unique()

# citation table
cdf <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_CITATIONS_202408/202408_EPO_CITATIONS.txt")

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
epo_df <- fread("../outputs/epo_until_2017_Hungary.csv")
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
conc <- fread("../data/patent_data/correspondance_ipc_nace.csv")
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
            paste0("../outputs/cr_norm_citation.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)






###### 02 effect comparison -- IO max and labor average  ######

# data prep
reg_df <- combined_regression_df(regions, export = TRUE)




### --- Table 2 -- clustered SE
m1 <- feols(egk_coagg_stand_nuts3 ~ io_wiot_hun_stand + lab_stand,
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




calculate_effects <- function(model_name, io_var, labor_var, data, model_label) {
  # extract coefficients
  coef_IO <- coef(model_name)[io_var]
  coef_labor <- coef(model_name)[labor_var]
  
  # calculate max and mean values for the variables
  max_IO <- max(data[[io_var]], na.rm = TRUE)
  mean_labor <- mean(data[[labor_var]], na.rm = TRUE)
  
  # calculate the effects
  max_IO_effect <- coef_IO * max_IO
  avg_labor_effect <- coef_labor * mean_labor
  ratio <- max_IO_effect / avg_labor_effect
  
  # save results in a data.table
  results <- data.table(
    model_name = model_label,
    effect_name = c("max_IO_effect", "avg_labor_effect", "ratio"),
    value = c(max_IO_effect, avg_labor_effect, ratio)
  )
  
  return(results)
}



effects <- data.table()
effects <- rbind(effects, calculate_effects(m1, "io_wiot_hun_stand", "lab_stand", reg_df, "m1"))
effects <- rbind(effects, calculate_effects(m2, "io_wiot_hun_stand", "lab_stand", reg_df, "m2"))
effects <- rbind(effects, calculate_effects(m3, "io3_stand", "lab_stand", reg_df, "m3"))
effects <- rbind(effects, calculate_effects(m4, "io3_stand", "lab_stand", reg_df, "m4"))
effects <- rbind(effects, calculate_effects(m5, "io_wiot_hun_stand", "lab_stand", reg_df, "m5"))
effects <- rbind(effects, calculate_effects(m6, "io_wiot_hun_stand", "lab_stand", reg_df, "m6"))
effects <- rbind(effects, calculate_effects(m7, "io3_stand", "lab_stand", reg_df, "m7"))
effects <- rbind(effects, calculate_effects(m8, "io3_stand", "lab_stand", reg_df, "m8"))






