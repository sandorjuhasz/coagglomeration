### finding similar MNEs and domestic firms -- OPTEN based exercise ###
# by sandorjuhasz


# packages
library(data.table)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(MatchIt)



# firm level OPTEN info
firm_info <- fread("../data/opten-dataset/mta_cegalap.csv", encoding='Latin-1')

# generate 3-digit NACE codes -- NO leading zeros and many NAs
firm_info <- subset(firm_info, is.na(fotev_kod)==0)
firm_info$nace3d <- ifelse(
  nchar(firm_info$fotev_kod)==5,
  as.numeric(substr(firm_info$fotev_kod, 1, 3)),
  as.numeric(substr(firm_info$fotev_kod, 1, 2))
)

# geo categories
geo_data <- fread("../data/iranyitoszamok_BE.csv")

# companies dataframe with key columns
cdf <- firm_info %>%
  select(oo_cegj_sz, hely, irsz, nace3d, letszam_besz18) %>%
  subset(is.na(irsz) == 0 & is.na(nace3d) == 0 & nace3d > 9) %>%
  subset(letszam_besz18 >= 2) %>%
  dplyr::rename(emp = letszam_besz18) %>%
  data.table()

# join location infos
cdf <- merge(
  cdf,
  unique(select(geo_data, irsz, jaras, megye)),
  by = "irsz",
  all.x = TRUE,
  all.y = FALSE
) %>%
  subset(is.na(jaras) == 0)
  


# mark MNEs
ownerdata <- fread("../data/opten-dataset/mta_tulaj.csv", encoding='Latin-1')

# owners are foreign firms
foreign_firm_owners <- ownerdata %>%
  subset((ceg_orsz != "") & (ceg_orsz != "HU")) %>%
  dplyr::select(oo_cegj_sz, ceg_orsz, ceg_varos) %>%
  data.table() %>%
  unique() %>%
  rename(country = ceg_orsz, city = ceg_varos)

# owners are foreign people -- NOT finished
foreign_person_owners <- ownerdata %>%
  subset((szemely_orsz != "") & (szemely_orsz != "HU")) %>%
  dplyr::select(oo_cegj_sz, szemely_orsz, szemely_varos) %>%
  data.table() %>%
  unique() %>%
  rename(country = szemely_orsz, city = szemely_varos)

foreign_table <- unique(rbind(foreign_firm_owners, foreign_person_owners))

# MNE dataframe
mne_df <- subset(cdf, emp >= 5)

mne_df <- merge(
  mne_df,
  foreign_table,
  by = "oo_cegj_sz",
  all.x = TRUE,
  all.y = FALSE
)
mne_df <- subset(mne_df, is.na(country) ==0)
mne_df$mne01 <- 1



# add MNE -- treated to cdf
cdf <- merge(
  cdf,
  dplyr::select(mne_df, oo_cegj_sz, mne01),
  by = "oo_cegj_sz",
  all.x = TRUE,
  all.y = FALSE
)
cdf[is.na(cdf)==1] <- 0



# fit a nearest neighbor
test_sample <- cdf[1:500]
m01 <- matchit(mne01 ~ hely + emp + nace3d, method = "nearest", distance = "mahalanobis", data = test_sample)
summary(m01)

rows <- c(m01$match.matrix)
test_sample[rows,"oo_cegj_sz"]
test_sample[52]
test_sample[124]
subset(test_sample, mne01 == 1)
