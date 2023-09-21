###  (owner) origin of countries -- for KSH incheck -- by sandorjuhasz


# packages
library(data.table)
library(dplyr)
options(dplyr.summarise.inform = FALSE)



# import ownership data
ownerdata <- fread("../data/opten-dataset/mta_tulaj.csv", encoding='Latin-1')
table(ownerdata$ceg_orsz)
