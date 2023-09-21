### single plant firms -- for KSH incheck -- by sandorjuhasz


# packages
library(data.table)
library(dplyr)
options(dplyr.summarise.inform = FALSE)




# firm locations data
firm_info <- fread("../data/opten-dataset/mta_cegalap.csv", encoding='Latin-1')
plant_table <- fread("../data/opten-dataset/mta_telep_fiok.csv", encoding='Latin-1')


# clean headquarter table
table1 <- firm_info %>%
  select(oo_cegj_sz, irsz, hely, teru) %>%
  data.table()


# clean plants table
table2 <- plant_table %>%
  select(oo_cegj_sz, irsz, hely, teru) %>%
  data.table()


# rbind tables with all company locations
location_table <- rbind(table1, table2)


# number of unique locations by company
nr_locations <- location_table %>%
  unique() %>%
  group_by(oo_cegj_sz) %>%
  summarise(nr_locations = n()) %>%
  data.table()


# option 1 / option 2 -- based on nr locations
loc1 <- subset(nr_locations, nr_locations == 1)
#loc2 <- subset(nr_locations, nr_locations > 1)




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


# add financial information for location tables for different years
years <- c(2016, 2017, 2018)
temp <- list()
for(y in 1:length(years)){
  temp[[y]] <- merge(
    loc1,
    subset(financial_info, ev==years[y]),
    by = "oo_cegj_sz",
    all.x = TRUE,
    all.y = FALSE
  )
  
  # drop rows w/ many NA
  temp[[y]] <- temp[[y]][complete.cases(temp[[y]][ , c("ev", "ae_ered")]), ]
}
loc1_fin_info <- rbindlist(temp)




# fake ID for in-check
id_table <- data.table(unique(loc1$oo_cegj_sz))
id_table$firm_id <- seq(1, nrow(id_table), 1)
colnames(id_table) <- c("oo_cegj_sz", "firm_id")

# save the switch table
write.table(id_table, "../outputs/ksh_single_plant_opten_switcher.csv", sep = ";", row.names = FALSE)

# save the KSH nodelist table
ksh_loc1 <- merge(
  loc1_fin_info,
  id_table,
  by = "oo_cegj_sz",
  all.x = TRUE,
  all.y = FALSE
)
ksh_loc1 <- select(relocate(ksh_loc1, firm_id, ev), -oo_cegj_sz)

# save the final table
write.table(ksh_loc1, "../outputs/ksh_single_plant.csv", sep = ";", row.names = FALSE)

