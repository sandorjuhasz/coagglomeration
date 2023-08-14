
# horizontal complexity -- number of suppliers
horizontal_complexity <- function(el, key_cols)
{
  if(length(key_cols) != 3)
  {
    print("Function expects id1, id2, weight data structure")
  }
  
  # select and filter dataframe by key_cols
  el <- el %>%
    dplyr::select(all_of(key_cols)) %>%
    filter(!!as.symbol(key_cols[3]) > 0) %>%
    data.table()

  # horizontal complexity of firms
  hc <- el %>%
    group_by_at(key_cols[1]) %>%
    summarise(hc = n_distinct(!!as.symbol(key_cols[2]))) %>%
    data.table()
  
  return(hc)
}




# number of T2 suppliers per supplier
vertical_complexity <- function(el, key_cols)
{
  if(length(key_cols) != 3)
  {
    print("Function expects id1, id2, weight data structure")
  }
  
  # calculate horizontal complexity
  hc <- horizontal_complexity(el, key_cols)
  
  # partner table
  el <- el %>%
    dplyr::select(all_of(key_cols)) %>%
    filter(!!as.symbol(key_cols[3]) > 0) %>%
    data.table()
  
  # join hc to partners
  vc <- merge(
    el,
    hc,
    by.x = key_cols[2],
    by.y = key_cols[1],
    all.x = TRUE,
    all.y = FALSE
  )
  
  # NAs -- replaced by 0 -- not mean(, na.rm = TRUE)
  vc$hc[is.na(vc$hc)==1] <- 0
  
  # vertical complexity of firms
  vc <- vc %>%
    group_by_at(key_cols[1]) %>%
    summarise(vc = round(mean(hc),3)) %>%
    data.table()
  
  return(vc)
}


# test ground -- KSH
horizontal_complexity(limited_el, key_cols = c("firm1", "firm2", "buy_value"))
vertical_complexity(limited_el, key_cols = c("firm1", "firm2", "buy_value"))


# test ground -- local
library(data.table)
library(dplyr)
tset <- fread("../data/oc1_2022_november/transactions_indreg_nace3d_megye.csv")

horizontal_complexity(tset, key_cols = c("megye_kod1", "megye_kod2", "tb_value_sum"))
vertical_complexity(tset, key_cols = c("megye_kod1", 'megye_kod2', "tb_value_sum"))
