###### patent data manipulation for revision  -- sandorjuhasz



library(data.table)
library(dplyr)
source("../code/02_regression_functions.R")



# parameters
focal_year <- 2017
regions <- c("nuts3", "nuts4")



###### patents in 2015-2017 in Hungary ######
epo_df <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_IPC.txt")

# focal period
#epo_df <- subset(epo_df, (prio_year>=2015) & (prio_year<=2017))

# applicant region
#app_df <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_App_reg.txt")
app_df <- fread("../data/patent_data/OECD_patent_data_2024SEPT/OECD_REGPAT_202401/202401_EPO_Inv_reg.txt")
app_df <- app_df %>%
  filter(ctry_code == "HU") %>%
  select(appln_id, ctry_code) %>%
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
  select(appln_id, ctry_code, IPC4d) %>%
  unique() %>%
  data.table()

write.table(epo_df,
            paste0("../outputs/epo_2015_2017_Hungary.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)
rm(list=ls())
gc()





###### citations ######

# EPO patents in Hungary 2015-2017
epo_df <- fread("../outputs/epo_2015_2017_Hungary.csv")
epo_df <- select(epo_df, appln_id, ctry_code) %>% unique()

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
  select(appln_id_citing = Citing_appln_id, appln_id_cited = Cited_Appln_id)
  
# add tech codes to citation relations in Hungary
epo_df <- fread("../outputs/epo_2015_2017_Hungary.csv")
epo_df <- select(epo_df, appln_id, IPC4d) %>% unique()

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
conc <- select(conc, ipc = IPCV2015, ind) %>% unique()

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
cr_norm <- subset(cr_norm, ind_i < ind_j)

# export
write.table(cr_norm,
            paste0("../outputs/cr_norm_citation.csv"),
            row.names = FALSE,
            col.names = TRUE,
            sep = ";"
)

