## function to support exploration of different coagglomeration settings
## related to OC 2022-NOV
## developed by sandorjuhasz


library(data.table)
library(dplyr)


# 



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
  theme(axis.text = element_text(size=20), axis.title=element_text(size=20))
}



# ggplot axis text size preset
custom_theme_xrotation <- function(...)
{
  theme(axis.text = element_text(size=20), axis.title=element_text(size=25)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}