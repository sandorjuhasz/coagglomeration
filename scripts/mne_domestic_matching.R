### finding similar MNEs and domestic firms -- OPTEN based exercise ###
# by sandorjuhasz


# packages
library(data.table)
library(dplyr)
options(dplyr.summarise.inform = FALSE)



# firm level OPTEN info
firm_info <- fread("../data/opten-dataset/mta_cegalap.csv", encoding='Latin-1')

# nace code creation
firm_info$nace2d <- substr(firm_info$fotev_kod, 1, 2)
firm_info$nace3d <- substr(firm_info$fotev_kod, 1, 3)
firm_info$nace4d <- substr(firm_info$fotev_kod, 1, 4)


# geo categories
geo_data <- fread("../data/iranyitoszamok_BE.csv")
