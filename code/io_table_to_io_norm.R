# IO / AKM data manipulation and checks -- sandorjuhasz


library(data.table)
library(dplyr)


### HUN ASK test
hu_akm <- fread("../data/io_external/io_elist_2018_hun.csv")
#swe_akm <- fread("../data/io_external/io_elist_2016_swe.csv")
swe_akm <- fread("../outputs/swe_io_2digit_nace.csv")
colnames(hu_akm) <- c("ind_i", "ind_j", "f_ij")
hu_akm$f_ij <- round(hu_akm$f_ij / 1000, 0)
colnames(swe_akm) <- c("ind_i", "ind_j", "f_ij")


industries <- unique(c(hu_akm$ind_i, hu_akm$ind_j))
length(industries)
industries <- unique(c(swe_akm$ind_i, swe_akm$ind_j))
length(industries)

fc <- data.table(expand.grid(industries, industries))
colnames(fc) <- c("ind_i", "ind_j")


# produce raw SR network from national flows across 2013-2019, 2-digit industries
hu_io_norm <-
  hu_akm %>%
  #get SR network
  group_by(ind_i) %>%
  mutate(f_i = sum(f_ij)) %>%
  group_by(ind_j) %>%
  mutate(f_j = sum(f_ij)) %>%
  ungroup() %>%
  mutate(f = sum(f_ij)) %>%
  mutate(io = f_ij / ((f_i * f_j) / f)) %>%
  mutate(io_norm = (io - 1) / (io + 1)) %>%
  data.table()



# produce raw SR network from national flows across 2013-2019, 2-digit industries
swe_io_norm <-
  swe_akm %>%
  #get SR network
  group_by(ind_i) %>%
  mutate(f_i = sum(f_ij)) %>%
  group_by(ind_j) %>%
  mutate(f_j = sum(f_ij)) %>%
  ungroup() %>%
  mutate(f = sum(f_ij)) %>%
  mutate(io = f_ij / ((f_i * f_j) / f)) %>%
  mutate(io_norm = (io - 1) / (io + 1)) %>%
  data.table()

swe_io_norm



hu_io_norm$io[is.na(hu_io_norm$io) == 1] <- 0
hu_io_norm$io_norm[is.na(hu_io_norm$io_norm) == 1] <- -1



# get a complete nodelist as dataframe based on all nodes present in an edgelist.
get_nlist_frame <- function(elist_df){
  
  node_vect <- unique(c(pull(elist_df[, 1]), pull(elist_df[, 2])))
  nlist_df <- data.frame(node_vect)
  colnames(nlist_df) <- "node"
  
  return(nlist_df)
}


# get a complete edgelist, including loops as dataframe based on complete nodelist.
get_elist_frame <- function(nlist_frame_df){
  
  nlist_frame_df$tag <- 1
  
  elist_frame_df <-
    nlist_frame_df %>%
    full_join(nlist_frame_df, by = "tag") %>%
    dplyr::select(-tag) %>%
    rename(node_i = node.x,
           node_j = node.y)
  
  return(elist_frame_df)
}


# add edge ID to edgelist.
add_eid <- function(elist_frame_df){
  
  tech_df1 <-
    elist_frame_df %>%
    filter(elist_frame_df[, 1] <= elist_frame_df[, 2]) %>%
    mutate(eid = seq(from = 1, to = n(), by = 1))
  
  tech_df2 <-
    tech_df1 %>%
    dplyr::select(2, 1, 3)	
  
  colnames(tech_df2) <- colnames(tech_df1)
  
  elist_frame_df <-
    bind_rows(tech_df1, tech_df2) %>%
    distinct() %>%
    arrange(.[1], .[2])
  
  
  return(elist_frame_df)
}


nlist_frame <- get_nlist_frame(hu_akm)
elist_frame <- get_elist_frame(nlist_frame)
elist_frame <- add_eid(elist_frame)




#make undirected
elist2dig <-
  elist_frame %>%
  rename(ind_i = node_i, ind_j = node_j) %>%
  #Merge in SR measures to edgelist frame. Both ind_i -> ind_j, and ind_j -> ind_i gets added this way.
  left_join(srnet2dig, by = c("ind_i", "ind_j")) %>%
  #make undirected
  arrange(eid, ind_i, ind_j) %>%
  group_by(eid) %>%
  mutate(sr_norm = sum(sr_norm, na.rm = TRUE) / 2) %>%
  mutate(tag_drop = ifelse(sum(is.na(sr)) == 2, 1, 0)) %>% # handle the twoway missings here
  ungroup() %>%
  #drop if edge is not identified either way, or loop
  filter(ind_i != ind_j) %>%
  filter(tag_drop != 1) %>%
  #keep necessary variables
  select(ind_i, ind_j, eid, sr_norm)







