###  (owner) origin of countries -- for KSH incheck -- by sandorjuhasz


# packages
library(data.table)
library(dplyr)
options(dplyr.summarise.inform = FALSE)



# import ownership data
ownerdata <- fread("../data/opten-dataset/mta_tulaj.csv", encoding='Latin-1')


# keep relevant ownership information
odata1 <- ownerdata %>%
  filter(szemely_orsz != "HU" & szemely_orsz != "") %>%
  group_by(oo_cegj_sz) %>%
  mutate(total_szem = n_distinct(szemid)) %>%
  group_by(oo_cegj_sz, total_szem, szemely_orsz) %>%
  summarise(nr_szem_from_country = n_distinct(szemid)) %>%
  mutate(country_share = nr_szem_from_country / total_szem) %>%
  filter(country_share > 0.5) %>%
  group_by(oo_cegj_sz) %>%
  filter(country_share == max(country_share)) %>%
  select(oo_cegj_sz, szemely_orsz) %>%
  distinct() %>%
  rename(owner_country = szemely_orsz) %>%
  data.table()
odata1$otype <- "szem"

odata2 <- ownerdata %>%
  filter(ceg_orsz != "HU" & ceg_orsz != "") %>%
  group_by(oo_cegj_sz) %>%
  mutate(total_owner_ceg = n_distinct(ceg_nev)) %>%
  group_by(oo_cegj_sz, total_owner_ceg, ceg_orsz) %>%
  summarise(nr_ceg_from_country = n_distinct(ceg_nev)) %>%
  mutate(country_share = nr_ceg_from_country / total_owner_ceg) %>%
  filter(country_share > 0.5) %>%
  group_by(oo_cegj_sz) %>%
  filter(country_share == max(country_share)) %>%
  select(oo_cegj_sz, ceg_orsz) %>%
  distinct() %>%
  rename(owner_country = ceg_orsz) %>%
  data.table()
odata2$otype <- "ceg"


# select the final owner
odata <- rbind(odata1, odata2)
odata <- odata %>%
  group_by(oo_cegj_sz) %>%
  mutate(nr_options = n_distinct(owner_country)) %>%
  data.table()
  