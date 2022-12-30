## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)
library(Matrix)
library(mefa4)



# parameters
reg <- "megye"
ind <- 3



# data sources
ir_df <- fread("../data/oc_2022_november/indreg_2018_2020_megye.csv")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv")
coagg_df <- fread("../data/oc_2022_november/coagglomeration_manufacturing_nace3d_megye_totalemp.csv")


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
                                     weight_col = "total_emp18")




# Lc'p matrix
ir_df
mne_df

# Mcp * Lc'p will give an Mcc' matrix -- but it is connected to coagglormation (?)





# Ellison-Glaeser-Kerr coagglomeration
EGK_coagglomeration <- function(mat)
{
  # input is an region-industry matrix
  mat <- as.matrix(mat)
  Smi <- t(t(mat)/colSums(mat))
  Smi[is.na(Smi)] <- 0  #replace NA with 0
  Xm <- rowSums(Smi)/ncol(mat) # avg share of industries in region m
  counter <- Smi - Xm
  counter <- t(counter) %*% counter
  denominator <- 1 - sum(Xm^2)
  coagglomeration <- counter / denominator
  coagglomeration <- as.matrix(coagglomeration)
  return(coagglomeration)
}


egk_mat <- EGK_coagglomeration(mat)
egk_df <- data.table(Melt(egk_mat))
colnames(egk_df) <- c("ind1", "ind2", "coagglomeration")

coagg_df$ind1 <- as.character(coagg_df$ind1)
coagg_df$ind2 <- as.character(coagg_df$ind2)

check_df <- merge(
  coagg_df,
  egk_df,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)






# association strength
assoc <- function (x, s) {
  drop(crossprod(x^2, s))
}

# co-occurrence
coo_normalized <- function (X, norm = assoc)
{
  X <- as(X, "dgCMatrix")
  diag(X) <- 0
  S <- rep(1, nrow(X))
  N <- Diagonal(x = match.fun(norm)(X, S)^-1)
  X <- X %*% N
  X <- crossprod(X)
  diag(X) <- 0
  return(X)
}

coagg_matrix <- coo_normalized(mat)
coagg_data <- data.table(Melt(coagg_matrix))
colnames(coagg_data) <- c("ind1", "ind2", "coagglomeration")


coagg_df$ind1 <- as.character(coagg_df$ind1)
coagg_df$ind2 <- as.character(coagg_df$ind2)

test_data <- merge(
  coagg_df,
  coagg_data,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE,
)










# Mcc matrix
mcc_mat <- mat %*% t(mat)


# Mpp matrix
mpp_mat <- t(mat) %*% mat














# coagglomeration -- as relatedness
library(EconGeo)
#rel_matrix <- relatedness(t(mat) %*% mat, method = "association")
rel_matrix <- relatedness(mpp_mat)
rel_data <- data.table(Melt(rel_matrix))
colnames(rel_data) <- c("ind1", "ind2", "coagglomeration")

coagg_version <- merge(
  coagg_data,
  rel_data,
  by = c("ind1", "ind2"),
  all.x = TRUE,
  all.y = FALSE
)












# multiply

