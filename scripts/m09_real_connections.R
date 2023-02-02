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



# generate industry-region ids
tr_df$ir_id1 <- paste0(tr_df$megye_kod1, "-", tr_df$nace3d1)
tr_df$ir_id2 <- paste0(tr_df$megye_kod2, "-", tr_df$nace3d2)
mne_df$ir_id <- paste0(mne_df$megye_kod, "-", mne_df$nace3d)



# transaction between MNE-local / MNE-local 2*2 matrix
tr_df <- merge(
  tr_df,
  select(mne_df, ir_id, mne_dom_25, mne_dom_50, mne_dom_75),
  by.x = "ir_id1",
  by.y = "ir_id",
  all.x = TRUE,
  all.y = FALSE
)
tr_df <- merge(
  tr_df,
  select(mne_df, ir_id, mne_dom_25, mne_dom_50, mne_dom_75),
  by.x = "ir_id2",
  by.y = "ir_id",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_1", "_2")
)



# buyer-supplier ties -- aggregation -- buyer category in rownames -- supplier in colnames
table(tr_df$mne_dom_50_1, tr_df$mne_dom_50_2) / sum(table(tr_df$mne_dom_50_1, tr_df$mne_dom_50_2))
table(tr_df$mne_dom_75_1, tr_df$mne_dom_75_2)

subset(tr_df, mne_dom_50_1 == 1 & mne_dom_50_2 == 0)

# local supplier MNE-local

# local buyer MNE-local

# relative share of ties to co-agglomerated industries

