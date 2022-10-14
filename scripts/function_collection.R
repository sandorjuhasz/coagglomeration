## coagglomeration and MNEs --supporting functions
## developed by sandorjuhasz

library(data.table)
library(dplyr)




# region-industry matrix construction -- NOT WORKING
create_indreg_matrix <- function(data, region_col, industry_col)
{
  # input has an emp column with nr employees
  cols <- c(region_col, industry_col)
  
  temp <- data %>%
    group_by_at(cols) %>%
    summarise(total_emp = sum(emp)) %>%
    data.table()
  
  temp$reg <- temp[,..region_col]
  temp$ind <- temp[,..industry_col]
  
  indreg_mat <- as.matrix(xtabs(total_emp ~ reg + ind, data=temp))
  
  return(indreg_mat)
}




# EGK coagglomeration
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





# functions to normalize variables
norm01 <- function(column){
  # normalize variables to 0-1 scale
  new_var <- (column - min(column)) / (max(column) - min(column))
  return(new_var)
}



z_score <-function(column){
  # z-score of a variable
  new_var <- (column - mean(column)) / sd(column)
  return(new_var)
} 





# ggplot axis text size preset
custom_theme <- function(...)
{
  theme(axis.text = element_text(size=20), axis.title=element_text(size=25))
}



# ggplot axis text size preset
custom_theme_xrotation <- function(...)
{
  theme(axis.text = element_text(size=20), axis.title=element_text(size=25)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}