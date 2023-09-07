# wiot experiments -- sandorjuhasz


library(data.table)
library(dplyr)
library(tidyr)

# data from WIOD home page
load("../data/WIOT2014_October16_ROW.RData")


# available countries - minus TOTAL and Rest-of-the-World
countries <- unique(wiot$Country)
countries <- countries[1:(length(countries)-2)]


# industry codes
ind_vector <- unique(wiot$IndustryCode)[1:56]


# edgelist style data transformation for each country
cdf <- list()
for(c in 1:length(countries))
{
  temp <- wiot %>%
    rename(ind_code = IndustryCode) %>%
    filter((Country == countries[c]) & (ind_code %in% ind_vector)) %>%
    data.table()

  temp <- temp[, c(grep(countries[c], names(temp)), 1), with = FALSE]
  
  cdf[[c]] <- temp %>%
    pivot_longer(!ind_code, names_to = "ind", values_to = "values") %>%
    separate(ind, into = c("c_code2", "ind2"), sep = 3, remove = FALSE) %>%
    mutate(c_code = countries[c], ind2 = as.integer(ind2)) %>%
    filter(ind2 <= 56) %>%
    data.table()
}

cdf





