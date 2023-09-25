###  (owner) origin of countries -- for KSH incheck -- by sandorjuhasz


# packages
library(data.table)
library(dplyr)
options(dplyr.summarise.inform = FALSE)



# import ownership data
ownerdata <- fread("../data/opten-dataset/mta_tulaj.csv", encoding='Latin-1')


# keep relevant ownership information
odata <- ownerdata %>%
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
  data.table()


