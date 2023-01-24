## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)

# parameters
reg <- "megye"
ind <- 3

# data sources
tr_df <- fread("../data/oc_2022_november/transactions_indreg_nace3d_megye.csv")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv")

# transaction between MNE-local / MNE-local 2*2 matrix

# local supplier MNE-local

# local buyer MNE-local

# relative share of ties to co-agglomerated industries

