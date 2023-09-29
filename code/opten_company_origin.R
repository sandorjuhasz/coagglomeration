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
  filter(nr_options <= 2) %>%   # 2 firms...
  data.table()


# not-so-elegant solution
otemp <- subset(odata, (nr_options == 2) & (otype == "ceg"))
odata <- subset(odata, nr_options < 2)
odata <- rbind(odata, otemp)
o_final <- unique(select(odata, oo_cegj_sz, owner_country))


# financial for DataBank in-check
beszam <- fread("../data/opten-dataset/mta_beszamolo.csv", encoding='Latin-1')

# data transformation
financial_info <- beszam %>%
  filter((ev==2016) | (ev==2017) | (ev==2018)) %>%
  select(
    -arbev_kat,
    -k_penznem,
    -k_penzegys,
    -k_arbev,
    -k_exportarbev,
    -k_uzemiered,
    -k_ae_ered,
    -k_szem_jell_raf,
    -k_amortiz,
    -k_tar_eszk,
    -k_brutto_hozzaadott_ertek
  ) %>%
  arrange(oo_cegj_sz, ev) %>%
  data.table()

o_final <- merge(
  o_final,
  financial_info,
  by = "oo_cegj_sz",
  all.x = FALSE,
  all.y = TRUE
)
o_final <- subset(o_final, is.na(owner_country) != 1)


# KSH fake ID
id_table <- data.table(unique(o_final$oo_cegj_sz))
id_table$firm_id <- seq(1, nrow(id_table), 1)
colnames(id_table) <- c("oo_cegj_sz", "firm_id")

# save the switch table
write.table(id_table, "../outputs/ksh_owner_opten_switcher.csv", sep = ";", row.names = FALSE)



# save the KSH nodelist table
o_final <- merge(
  o_final,
  id_table,
  by = "oo_cegj_sz",
  all.x = TRUE,
  all.y = FALSE
)
o_final <- select(relocate(o_final, firm_id, ev), -oo_cegj_sz)

# save the final table
write.table(o_final, "../outputs/ksh_owner_country.csv", sep = ";", row.names = FALSE)


