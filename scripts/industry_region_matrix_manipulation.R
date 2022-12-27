## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)
library(Matrix)
library(mefa4)


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
  mat <- as.matrix(reshape(data,
                           idvar = "reg",
                           timevar = "ind",
                           direction = "wide"))
  
  # clean up
  rownames(mat) <- unique(mat[,1])
  mat <- mat[,-1]
  colnames(mat) <- unique(data$ind)
  mat[is.na(mat)] <- 0
  
  return(mat)
}

  
# Mcp matrix
ir_df$rca18d <- ifelse(ir_df$rca18 >= 1, 1, 0)
mat <- create_industry_region_matrix(ir_df,
                                     region_col = "reg",
                                     industry_col = "ind",
                                     weight_col = "rca18d")


# Mcc matrix
mcc_mat <- mat %*% t(mat)


# Mpp matrix
mpp_mat <- t(mat) %*% mat



# association strength
assoc <- function (x, s) {
  drop(crossprod(x^2, s))
}

# co-occurrence
coo_normalized <- function (X, norm = assoc) {
  X <- as(X, "dgCMatrix")
  diag(X) <- 0
  S <- rep(1, nrow(X))
  N <- Diagonal(x = match.fun(norm)(X, S)^-1)
  X <- X %*% N
  X <- crossprod(X)
  diag(X) <- 0
  return(X)
}

coagg_matrix <- coo_normalized(mpp_mat)
coagg_data <- data.table(Melt(coagg_matrix))
colnames(coagg_data) <- c("ind1", "ind2", "coagglomeration")


# coagglomeration -- as relatedness
library(EconGeo)
relatedness(t(mat) %*% mat, method = "association")



# Lc'p matrix


# multiply

