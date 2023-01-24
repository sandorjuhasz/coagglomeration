## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)
library(Matrix)
library(mefa4)
library(reshape2)
library(ggplot2)
library(cowplot)
library(RColorBrewer)


# parameters
reg <- "megye"
ind <- 3



# data sources
ir_df <- fread("../data/oc_2022_november/indreg_2018_2020_megye.csv")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv")
coagg_df <- fread("../data/oc_2022_november/coagglomeration_manufacturing_nace3d_megye_totalemp.csv")



# data manipulation
ir_df$rca18d <- ifelse(ir_df$rca18 >= 1, 1, 0)

df <- merge(
  ir_df,
  mne_df[, .(megye_kod, nace3d, mne_emp)],
  by.x = c("reg", "ind"),
  by.y = c("megye_kod", "nace3d"),
  all.x = TRUE,
  all.y = FALSE
)

df$local_emp <- df$total_emp18 - df$mne_emp



#  matrix construction
create_industry_region_matrix <- function(data, region_col, industry_col, weight_col)
{
  # select key cols
  cols <- c(region_col, industry_col, weight_col)
  data <- dplyr::select(data, all_of(cols))
  
  # edgelist to matrix
  mat <- as.matrix(reshape(data,
                           idvar = region_col,
                           timevar = industry_col,
                           direction = "wide"))
  
  # clean up
  rownames(mat) <- unique(mat[,1])
  mat <- mat[,-1]
  colnames(mat) <- c(unique(dplyr::select(data, all_of(industry_col))))[[1]]
  mat[is.na(mat)] <- 0
  
  return(mat)
}



# Mcp matrix
mcp <- create_industry_region_matrix(df,
                                     region_col = "reg",
                                     industry_col = "ind",
                                     weight_col = "total_emp18")

# mne_mat
mne_mcp <- create_industry_region_matrix(df,
                                         region_col = "reg",
                                         industry_col = "ind",
                                         weight_col = "mne_emp")

local_mcp <- create_industry_region_matrix(df,
                                           region_col = "reg",
                                           industry_col = "ind",
                                           weight_col = "local_emp")


# need to introduce a filter..
mne_mcp %*% t(local_mcp)

# Mcp * Lc'p will give an Mcc' matrix -- but it is connected to coagglormation (?)


# mcp
data.table(Melt(mcp))
data.table(reshape2::melt(mcp))
subset(data.table(reshape2::melt(mcp)), value > 0)



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






# Mcp matrix visualization
mcp <- create_industry_region_matrix(df,
                                     region_col = "reg",
                                     industry_col = "ind",
                                     weight_col = "rca18d")

mcp <- mcp[order(rowSums(mcp), decreasing=FALSE),]
mcp <- mcp[,order(colSums(mcp), decreasing=TRUE)]


heatmap(mcp,
        Rowv = NA,
        Colv = NA,
        scale="none",
        xlab = "Industries",
        ylab = "Regions",
        col = brewer.pal(9,"Blues"),
        key = FALSE)


heatmap.2(
  mcp,
  Rowv = NA,
  Colv = NA,
  dendrogram = "none",
  density.info = "none",
  scale="none",
  xlab = "Industries",
  ylab = "Regions",
  col = brewer.pal(9,"Blues")
)




title <- "mcp_heatmap"
file_name <- paste0("../figures/", title, ".png")
png(file_name, width=800, height=700, units = 'px')
heatmap(mcp,
        Rowv = NA,
        Colv = NA,
        xlab = "Industries",
        ylab = "Regions",

        col = brewer.pal(9,"Blues"))
dev.off()






vdf <- dplyr::select(df, reg, ind, rca18d)
colnames(vdf) <- c("reg", "ind", "rca01")

vdf <- vdf %>%
  group_by(ind) %>%
  mutate(nr_reg_rca = sum(rca01)) %>%
  ungroup %>%
  group_by(reg) %>%
  mutate(nr_ind_rca = sum(rca01)) %>%
  arrange(desc(nr_reg_rca), desc(nr_ind_rca)) %>%
  data.table()

temp1 <- data.table(unique(vdf$ind),
                   seq(1, length(unique(vdf$ind)), by = 1))
colnames(temp1) <- c("ind", "new_ind")
temp2 <- data.table(unique(vdf$reg),
                    seq(1, length(unique(vdf$reg)), by = 1))
colnames(temp2) <- c("reg", "new_reg")

vdf <- merge(
  vdf,
  temp1,
  by = "ind",
  all.x = TRUE,
  all.y = FALSE
)
vdf <- merge(
  vdf,
  temp2,
  by = "reg",
  all.x = TRUE,
  all.y = FALSE
)

vdf <- arrange(vdf, new_ind, new_reg)


# ggplot axis text size preset
custom_theme_xrotation <- function(...){
  theme(axis.text = element_text(size=20), axis.title=element_text(size=25)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

ggplot(vdf, aes(x=new_reg, y=new_ind)) +
  geom_tile(aes(fill = rca01)) +
  xlab("Regions") +
  ylab("Industries") +
  scale_fill_gradient(name = "", low = "white", high = "darkblue") +
  theme_cowplot(12) +
  theme(legend.position="none") +
  custom_theme_xrotation()
  




set.seed(123)                                                     # Set seed for reproducibility
data <- matrix(rnorm(100, 0, 10), nrow = 10, ncol = 10)           # Create example data
colnames(data) <- paste0("col", 1:10)                             # Column names
rownames(data) <- paste0("row", 1:10)  










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












## example matrix to ggplot2 heatmap

n_row <- 30
n_col <- 10
# a mtrix with random numbers
dat <- matrix(rnorm(n_row*n_col),ncol=n_col)
dim(dat)

# column and row names 
colnames(dat) <- paste0("S",seq(1,n_col))
rownames(dat) <- paste0("f",seq(1,n_row))

# add signals to matrix
dat[,1:(n_col/2)] <- matrix(rnorm(n_row*n_col/2,mean=50,sd=5),ncol=n_col/2)
dat[,((n_col/2)+1):n_col] <- matrix(rnorm(n_row*n_col/2,mean=70,sd=5),n_col/2)
#colnames(dat) <- paste0("S",c(rep(1,n_col/2),rep(2,n_col/2)))
head(dat)

dat %>%
  as.data.frame() %>%
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "counts")

dat %>% 
  as.data.frame() %>%
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "counts") %>%
  mutate(samples= fct_relevel(samples,colnames(dat))) %>%
  ggplot(aes(x=samples, y=f_id, fill=counts)) + 
  geom_raster() + 
  scale_fill_viridis_c()




# Mcp matrix visualization
mcp <- create_industry_region_matrix(df,
                                     region_col = "reg",
                                     industry_col = "ind",
                                     weight_col = "rca18d")

mcp <- mcp[order(rowSums(mcp), decreasing=T),]
mcp <- mcp[,order(colSums(mcp), decreasing=T)]


mcp %>%
  as.data.frame() %>%
  rownames_to_column("reg") %>%
  pivot_longer(-c(reg), names_to = "ind", values_to = "nr_rca") %>%
  mutate(ind = fct_relevel(ind, colnames(mcp))) %>%
  ggplot(aes(x = ind, y = reg, fill = nr_rca)) + 
  geom_raster() + 
  scale_fill_viridis_c()


# multiply



# IO rel from France
library(haven)
IO_fr <- read_dta("../data/IOrel_France.dta") %>% data.table()


