# wiot experiments -- sandorjuhasz


library(data.table)
library(dplyr)
library(tidyr)


# data from WIOD home page
load("../data/WIOT2014_October16_ROW.RData")


# key table
key_table <- fread("../data/key_table_nace2_wiot.csv")


# available countries - minus TOTAL and Rest-of-the-World
countries <- unique(wiot$Country)
countries <- countries[1:(length(countries)-2)]


# industry codes
ind_vector <- unique(wiot$IndustryCode)[1:56]


# edgelist style data transformation for each country
cdf <- list()
for(c in 1:length(countries))
{
  # transformation by rows
  temp <- wiot %>%
    rename(ind_code = IndustryCode) %>%
    filter((Country == countries[c]) & (ind_code %in% ind_vector)) %>%
    data.table()
  
  # column name filter
  temp <- temp[, c(grep(countries[c], names(temp)), 1), with = FALSE]
  
  # pivot and clean
  cdf[[c]] <- temp %>%
    pivot_longer(!ind_code, names_to = "country_industry", values_to = "value") %>%
    separate(country_industry, into = c("c_code", "ind"), sep = 3, remove = FALSE) %>%
    mutate(ind = as.integer(ind)) %>%
    filter(ind <= 56) %>%
    select(-country_industry) %>%
    rename(ind1 = ind_code) %>%
    data.table()
}
cdf <- rbindlist(cdf)


# rename edge ind2 and combine
name_table <- data.table(unique(cdf$ind1), c(1:56))
colnames(name_table) <- c("ind2", "ind")

cdf <- merge(
  cdf,
  name_table,
  by = "ind",
  all.x = TRUE,
  all.y = FALSE
)


# clean up
cdf <- cdf %>%
  select(c_code, ind1, ind2, value, -ind) %>%
  data.table()


# the final edgelist
full_el <- data.table(expand.grid(c(key_table$ind_2dig), c(key_table$ind_2dig)))
colnames(full_el) <- c("ind1", "ind2")


# add wiot keys
full_el <- merge(
  full_el,
  select(key_table, ind_2dig, ind_wiot),
  by.x = "ind1",
  by.y = "ind_2dig",
  all.x = TRUE,
  all.y = FALSE
)
full_el <- merge(
  full_el,
  select(key_table, ind_2dig, ind_wiot),
  by.x = "ind2",
  by.y = "ind_2dig",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("1", "2")
)


# add values
el_list <- list()
for(c in 1:length(countries))
{
  temp <- subset(cdf, c_code == countries[c])
  el_list[[c]] <- merge(
    full_el,
    temp,
    by.x = c("ind_wiot1", "ind_wiot2"),
    by.y = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
}
el_list <- rbindlist(el_list)
el_list$value[is.na(el_list$value)==1] <- 0


# export
write.table(el_list, "../outputs/wiot_edgelist_2_digit.csv", sep=";", row.names = FALSE)



