## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz

library(data.table)
library(dplyr)
library(igraph)


# data sources
ir_df <- fread("../data/oc_2022_november/indreg_2018_2020_megye.csv")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv")


# parameters


#  matrix construction
create_industry_region_matrix <- function(data, region_col, industry_col, weight_col)
{
  # select key cols
  cols <- c(region_col, industry_col, weight_col)
  data <- dplyr::select(data, all_of(cols))
  
  # edgelist to matrix
  mat <- as.matrix(reshape(data, idvar = "reg", timevar = "ind", direction = "wide"))
  mat <- mat[,-1]
  rownames(mat) <- unique(edgelist$reg)
  colnames(mat) <- unique(edgelist$ind)
  mat[is.na(mat)] <- 0
  return(mat)
}
  
# Mcp matrix
mat <- create_industry_region_matrix(ir_df, region_col = "reg", industry_col = "ind", weight_col = "nr_firms18")  



# Lc'p matrix

# multiply

