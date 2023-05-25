
# horizontal complexity -- number of suppliers
horizontal_complexity <- function(el, key_cols)
{
  # select and filter dataframe by key cols
  el <- el %>%
    dplyr::select(all_of(key_cols)) %>%
    filter(buy_value > 0) %>%
    data.table()
  #return(el)
  
  # horizontal complexity of firms
  hc <- el %>%
    group_by(firm1) %>%
    summarise(hc = n_distinct(firm2)) %>%
    data.table()
  
  return(hc)
}




# number of T2 suppliers per supplier
vertical_complexity <- function(el, key_cols)
{
  # calculate horizontal complexity
  hc <- horizontal_complexity(el, key_cols)
  
  # partner table
  el <- el %>%
    dplyr::select(all_of(key_cols)) %>%
    # add reference to column from key_cols
    filter(buy_value > 0) %>%
    data.table()
  
  # join hc to partners
  vc <- merge(
    el,
    hc,
    by.x = "firm2",
    by.y = "firm1",
    all.x = TRUE,
    all.y = FALSE
  )
  
  # NAs -- replaced by 0 -- not mean(, na.rm = TRUE)
  vc$hc[is.na(vc$hc)==1] <- 0
  
  # vertical complexity of firms
  vc <- vc %>%
    group_by(firm1) %>%
    summarise(vc = round(mean(hc),3)) %>%
    data.table()
  
  return(vc)
}


# test on limited_el
horizontal_complexity(limited_el, key_cols = c("firm1", "firm2", "buy_value"))
vertical_complexity(limited_el, key_cols = c("firm1", "firm2", "buy_value"))
