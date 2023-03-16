## overlap of network ties and information theoretic approach
## by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)


# parameters


# data sources
ir_el <- fread("../data/oc_2023_march_labor/m01_ir_ir_el_output_OC.csv")
region_codes <- fread("../data/oc_2022_november/region_codes.csv", sep = ";")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv", sep=";") %>%
  rename(reg = megye_kod, ind = nace3d) %>%
  data.table()


# generate industry-region ids
ir_el$ir_id1 <- paste0(ir_el$reg1, "-", ir_el$ind1)
ir_el$ir_id2 <- paste0(ir_el$reg2, "-", ir_el$ind2)
mne_df$ir_id <- paste0(mne_df$reg, "-", mne_df$ind)


# add region names
ir_el <- merge(
  ir_el,
  unique(select(region_codes, megye_kod, megye_nev))[1:20],
  by.x = "reg1",
  by.y = "megye_kod",
  all.x = TRUE,
  all.y = FALSE
)
ir_el <- merge(
  ir_el,
  unique(select(region_codes, megye_kod, megye_nev))[1:20],
  by.x = "reg2",
  by.y = "megye_kod",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("1", "2")
)
ir_el <- rename(ir_el, reg_name1 = megye_nev1, reg_name2 = megye_nev2)





###### network inside regions ######

create_regional_network <- function(el, reg, weight, rca_filter){
  # key cols for the edgelist
  cols <- c("ind1", "ind2", weight)
  
  # option to filter for ties between industries with RCA
  if(rca_filter == TRUE)
  {
    el <- subset(el, rca011 == 1 & rca012 == 1)
  } else
  {
    el <- el
  }

  # filter for weight
  el <- el %>%
    filter(!!as.symbol(weight) > 0)
  
  # create the network
  net <- graph_from_data_frame(el[(reg1 == reg & reg2 == reg), ..cols], directed = TRUE)
  
  # add RCA to network
  
  return(net)
}

# different network version for an example region
io_graph <- create_regional_network(ir_el, reg = 3, weight = "nr_buy_ties", rca_filter = FALSE)
lab_graph <- create_regional_network(ir_el, reg = 3, weight = "nr_labor_ties", rca_filter = FALSE)

# function for Jaccard index -- between igraph networks
jaccard_index <- function(g1, g2) {
  g1 <- get.adjacency(g1)
  g1[g1 > 0.001] <- 1
  g2<-get.adjacency(g2)
  g2[g2 > 0.001] <- 1
  A <- sum(g1 != g2) # edges that changed (0->1 and 1->0)
  B <- sum(g1 * g2) # edges that have a 1 in M1 and 1 in M2, so stayed the same (1->1)
  
  return(round(B / sum(A, B), digits = 2)) # the ratio of stable ties ties (B), compared to all ties who change (A) + stable ties (B)
  on.exit(rm(A,B))
}


full_graph <- make_empty_graph() %>%
  add_vertices(length(unique(c(V(io_graph)$name, V(lab_graph)$name))))
V(full_graph)$name <- unique(c(V(io_graph)$name, V(lab_graph)$name))

io_full <- union(io_graph, full_graph)
lab_full <- union(lab_graph, full_graph)


jaccard_index(io_full, lab_full)




