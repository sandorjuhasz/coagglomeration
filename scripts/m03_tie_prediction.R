## overlap of network ties and information theoretic approach
## by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)
library(infotheo)

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

# function to create regional networks -- from industry-region edgelist
create_regional_network <- function(el, reg, weight, rca_filter)
{
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


# function for Jaccard index -- between igraph networks
jaccard_index <- function(g1, g2)
{
  g1 <- get.adjacency(g1)
  g1[g1 > 0.001] <- 1
  g2<-get.adjacency(g2)
  g2[g2 > 0.001] <- 1
  A <- sum(g1 != g2) # edges that changed (0->1 and 1->0)
  B <- sum(g1 * g2) # edges that have a 1 in M1 and 1 in M2, so stayed the same (1->1)
  
  return(round(B / sum(A, B), digits = 3)) # the ratio of stable ties ties (B), compared to all ties who change (A) + stable ties (B)
  on.exit(rm(A,B))
}


# function to compare two graphs through Jaccard index
jaccard_of_two_graphs <- function(g1, g2)
{
  # create full graph with all nodes
  full_graph <- make_empty_graph() %>%
    add_vertices(length(unique(c(V(g1)$name, V(g2)$name))))
  V(full_graph)$name <- unique(c(V(g1)$name, V(g2)$name))
  
  # make the two networks have the same nodeset
  g1_full <- union(g1, full_graph)
  g2_full <- union(g2, full_graph)
  
  return(jaccard_index(g1_full, g2_full))
}


# different network version for an example region
io_graph <- create_regional_network(ir_el, reg = 3, weight = "nr_buy_ties", rca_filter = FALSE)
lab_graph <- create_regional_network(ir_el, reg = 3, weight = "nr_labor_ties", rca_filter = FALSE)
jaccard_of_two_graphs(io_graph, lab_graph)


# for all regions
regions <- unique(c(ir_el$reg1, ir_el$reg2))
io_lab_jaccard <- c()
for(r in 1:length(regions))
{
  io_graph <- create_regional_network(ir_el, reg = r, weight = "nr_buy_ties", rca_filter = FALSE)
  lab_graph <- create_regional_network(ir_el, reg = r, weight = "nr_labor_ties", rca_filter = FALSE)
  io_lab_jaccard[r] <- jaccard_of_two_graphs(io_graph, lab_graph)
  
}
regions_jaccard_table <- data.table(regions, io_lab_jaccard)


# function to create full edgelist for regions with different edge types
multi_el <- function(g1, g2)
{
  # edgelist with all nodes present in graph1 and graph2
  all_nodes <- unique(c(V(g1)$name, V(g2)$name))
  full_el <- data.table(expand.grid(ind1 = all_nodes, ind2 = all_nodes))
  
  # io / lab edgelists
  el1 <- data.table(get.edgelist(g1))
  colnames(el1) <- c("ind1", "ind2")
  el1$io_ties <- 1
  el2 <- data.table(get.edgelist(g2))
  colnames(el2) <- c("ind1", "ind2")
  el2$lab_ties <- 1
  
  # merge lab
  full_el <- merge(
    full_el,
    el1,
    by = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  full_el <- merge(
    full_el,
    el2,
    by = c("ind1", "ind2"),
    all.x = TRUE,
    all.y = FALSE
  )
  full_el[is.na(full_el)==1] <- 0
  
  return(full_el)
}

# use the two vectors to compute mutual information
io_graph <- create_regional_network(ir_el, reg = 1, weight = "nr_buy_ties", rca_filter = FALSE)
lab_graph <- create_regional_network(ir_el, reg = 1, weight = "nr_labor_ties", rca_filter = FALSE)
full_el <- multi_el(io_graph, lab_graph)
mutinformation(full_el$io_ties, full_el$lab_ties)


# for all regions
regions <- unique(c(ir_el$reg1, ir_el$reg2))
io_lab_mutinform <- c()
for(r in 1:length(regions))
{
  io_graph <- create_regional_network(ir_el, reg = r, weight = "nr_buy_ties", rca_filter = FALSE)
  lab_graph <- create_regional_network(ir_el, reg = r, weight = "nr_labor_ties", rca_filter = FALSE)
  full_el <- multi_el(io_graph, lab_graph)
  
  io_lab_mutinform[r] <- mutinformation(full_el$io_ties, full_el$lab_ties)
}
regions_mutinform_table <- data.table(regions, io_lab_mutinform)



