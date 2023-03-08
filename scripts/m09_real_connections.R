## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)
library(ggraph)   
library(graphlayouts)

# parameters
reg <- "megye"
ind <- 3

# data sources
ir_el <- fread("../data/oc_2023_march_labor/m01_ir_ir_el_output_OC.csv")
indreg_gr <- fread("../data/oc_2022_november/indreg_2018_2020_megye.csv")
region_codes <- fread("../data/oc_2022_november/region_codes.csv", sep = ";")

# generate industry-region ids
ir_el$ir_id1 <- paste0(ir_el$reg1, "-", ir_el$ind1)
ir_el$ir_id2 <- paste0(ir_el$reg2, "-", ir_el$ind2)
indreg_gr$ir_id <- paste0(indreg_gr$reg, "-", indreg_gr$ind)

# rca01 for indreg growth dataframe
indreg_gr$rca01 <- ifelse(indreg_gr$rca18 >=1, 1, 0)

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
io_graph_rca <- create_regional_network(ir_el, reg = 3, weight = "nr_buy_ties", rca_filter = TRUE)
lab_graph_rca <- create_regional_network(ir_el, reg = 3, weight = "nr_labor_ties", rca_filter = TRUE)


# graph stats for all regional networks
reg_codes <- unique(c(ir_el$reg1, ir_el$reg2))
rn_table <- list()

for(r in 1:length(reg_codes))
{
  # create io graphs
  io_graph <- create_regional_network(ir_el, reg = r, weight = "nr_buy_ties", rca_filter = FALSE)
  random_io <- erdos.renyi.game(vcount(io_graph), ecount(io_graph), type = "gnm", directed = TRUE, loops = TRUE)
  io_graph_rca <- create_regional_network(ir_el, reg = r, weight = "nr_buy_ties", rca_filter = TRUE)
  random_io_rca <- erdos.renyi.game(vcount(io_graph_rca), ecount(io_graph_rca), type = "gnm", directed = TRUE, loops = TRUE)
  
  # io table with network statistics
  io_table <- data.table(
    reg = r,
    io_nodes = vcount(io_graph),
    io_edges = ecount(io_graph),
    io_density = edge_density(io_graph, loops = FALSE),
    io_clustering = transitivity(io_graph, type = "global"),
    io_clustering_random = (transitivity(io_graph, type = "global") / transitivity(random_io, type = "global")),
    
    io_nodes_rca = vcount(io_graph_rca),
    io_edges_rca = ecount(io_graph_rca),
    io_density_rca = edge_density(io_graph_rca, loops = FALSE),
    io_clustering_rca = transitivity(io_graph_rca, type = "global"),
    io_clustering_random_rca = (transitivity(io_graph_rca, type = "global") / transitivity(random_io_rca, type = "global"))
  )
  
  # create labor flow graphs
  lab_graph <- create_regional_network(ir_el, reg = r, weight = "nr_labor_ties", rca_filter = FALSE)
  random_lab <- erdos.renyi.game(vcount(lab_graph), ecount(lab_graph), type = "gnm", directed = TRUE, loops = TRUE)
  lab_graph_rca <- create_regional_network(ir_el, reg = r, weight = "nr_labor_ties", rca_filter = TRUE)
  random_lab_rca <- erdos.renyi.game(vcount(lab_graph_rca), ecount(lab_graph_rca), type = "gnm", directed = TRUE, loops = TRUE)
  
  # labor table with network statistics
  lab_table <- data.table(
    lab_nodes = vcount(lab_graph),
    lab_edges = ecount(lab_graph),
    lab_density = edge_density(lab_graph, loops = FALSE),
    lab_clustering = transitivity(lab_graph, type = "global"),
    lab_clustering_random = transitivity(lab_graph, type = "global") / transitivity(random_lab, type = "global"),
    
    lab_nodes_rca = vcount(lab_graph_rca),
    lab_edges_rca = ecount(lab_graph_rca),
    lab_density_rca = edge_density(lab_graph_rca, loops = FALSE),
    lab_clustering_rca = transitivity(lab_graph_rca, type = "global"),
    lab_clustering_random_rca = transitivity(lab_graph_rca, type = "global") / transitivity(random_lab_rca, type = "global")
  )
  
  # combine
  rn_table[[r]] <- cbind(io_table, lab_table)
  
}

# combine to region network table
rn_table<- rbindlist(rn_table)

# add names
rn_table <- merge(
  rn_table,
  unique(select(region_codes, megye_kod, megye_nev))[1:20],
  by.x = "reg",
  by.y = "megye_kod",
  all.x = TRUE,
  all.y = FALSE
)

# export
write.table(rn_table, "../outputs/region_network_descriptive.csv", sep=";", row.names = FALSE)







###### network visuals ######

# RCA values and colors for nodes
rca_to_net <- data.table(V(io_graph)$name)
colnames(rca_to_net) <- "ind"
rca_to_net$ind <- as.integer(rca_to_net$ind)
rca_to_net <- merge(
  rca_to_net,
  select(subset(indreg_gr, reg == 3), ind, rca01),
  by = "ind",
  all.x = TRUE,
  all.y = FALSE
)
rca_to_net[is.na(rca_to_net)==1] <- 0
rca_to_net$rca_emp_color <- ifelse(rca_to_net$rca01 ==1, "darkgreen", "grey")



# plot the network -- nicely
set.seed(665)
ggraph(io_graph, layout = "nicely") +
  geom_edge_link0(width = 0.5, colour = "grey") +
  geom_node_point(col = "darkgreen", size = 5) +
  theme_graph()

set.seed(665)
ggraph(lab_graph, layout = "nicely") +
  geom_edge_link0(width = 0.5, colour = "grey") +
  geom_node_point(col = "darkgreen", size = 5) +
  theme_graph()


# plot the network -- stress-full
set.seed(265)
ggraph(io_graph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "#264653") +
  geom_node_point(col = rca_to_net$rca_emp_color, size = 5) +
  theme_graph()

ggraph(sgraph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "#e9c46a") +
  geom_node_point(col = rca_to_net$rca_emp_color, size = 5) +
  theme_graph()






export_el <- tr_el %>%
  filter(megye_kod1 == 3 & megye_kod2 == 3) %>%
  select(nace3d1, nace3d2, tb_count, mne_dom_50_1, mne_dom_50_2, rca01_1, rca01_2) %>%
  rename(Source = nace3d1, Target = nace3d2) %>%
  data.table()


export_nodes <- indreg18 %>%
  filter(reg == 3) %>%
  select(ind, rca01) %>%
  rename(Id = ind) %>%
  data.table()

write.table(export_el, "../outputs/gephi_illustration_edgelist.csv", sep=";", row.names = FALSE)
write.table(export_nodes, "../outputs/gephi_illustration_nodelist.csv", sep=";", row.names = FALSE)









###### transaction between MNE-local / MNE-local 2*2 matrix ######
tr_df <- merge(
  tr_df,
  select(mne_df, ir_id, mne_dom_25, mne_dom_50, mne_dom_75),
  by.x = "ir_id1",
  by.y = "ir_id",
  all.x = TRUE,
  all.y = FALSE
)
tr_df <- merge(
  tr_df,
  select(mne_df, ir_id, mne_dom_25, mne_dom_50, mne_dom_75),
  by.x = "ir_id2",
  by.y = "ir_id",
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_1", "_2")
)



# buyer-supplier ties -- aggregation -- buyer category in rownames -- supplier in colnames
table(tr_df$mne_dom_50_1, tr_df$mne_dom_50_2) / sum(table(tr_df$mne_dom_50_1, tr_df$mne_dom_50_2))
table(tr_df$mne_dom_75_1, tr_df$mne_dom_75_2)

subset(tr_df, mne_dom_50_1 == 1 & mne_dom_50_2 == 0)

# local supplier MNE-local

# local buyer MNE-local

# relative share of ties to co-agglomerated industries





###### network visuals and comparison ######
indreg18$rca01 <- ifelse(indreg18$rca18 >= 1, 1, 0)
tr_el <- merge(
  tr_df,
  select(indreg18, reg, ind, rca01),
  by.x = c("megye_kod1", "nace3d1"),
  by.y = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE
)
tr_el <- merge(
  tr_el,
  select(indreg18, reg, ind, rca01),
  by.x = c("megye_kod2", "nace3d2"),
  by.y = c("reg", "ind"),
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_1", "_2")
)

# function to create regional network
create_regional_network <- function(el, reg, weight, rca_filter){
  # key cols for the edgelist
  cols <- c("nace3d1", "nace3d2", weight)
  
  # option to filter for ties between industries with RCA
  if(rca_filter == TRUE)
  {
    el <- subset(el, rca01_1 == 1 & rca01_2 == 1)
  } else
  {
    el <- el
  }
  
  # create the network
  net <- graph_from_data_frame(el[(megye_kod1 == reg & megye_kod2 == reg), ..cols], directed = TRUE)
  return(net)
}
sgraph <- create_regional_network(tr_el, reg = 3, weight = "tb_count", rca_filter = FALSE)


rca_to_net <- data.table(V(sgraph)$name)
colnames(rca_to_net) <- "ind"
rca_to_net$ind <- as.integer(rca_to_net$ind)
rca_to_net <- merge(
  rca_to_net,
  select(subset(indreg18, reg == 3), ind, rca01),
  by = "ind",
  all.x = TRUE,
  all.y = FALSE
)
rca_to_net$rca_emp_color <- ifelse(rca_to_net$rca01 ==1, "darkgreen", "grey")


# plot the network -- nicely
set.seed(665)
ggraph(sgraph, layout = "nicely") +
  geom_edge_link0(width = 0.5, colour = "grey") +
  geom_node_point(col = "darkgreen", size = 5) +
  theme_graph()

# plot the network -- stress-full
set.seed(265)
p1 <- ggraph(sgraph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "#264653") +
  geom_node_point(col = rca_to_net$rca_emp_color, size = 5) +
  theme_graph()

ggraph(sgraph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "#e9c46a") +
  geom_node_point(col = rca_to_net$rca_emp_color, size = 5) +
  theme_graph()


# rca_to_net$rca_emp_color


rca_sgraph <- create_regional_network(tr_el, reg = 3, weight = "tb_count", rca_filter = TRUE)
p2 <- ggraph(rca_sgraph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "grey") +
  geom_node_point(col = "darkgreen", size = 5) +
  theme_graph()

# compare density
edge_density(sgraph, loops = TRUE)
edge_density(rca_sgraph, loops = TRUE)

# random graph for full network
er_graph <- erdos.renyi.game(vcount(sgraph), ecount(sgraph), type = "gnm", directed = TRUE, loops = TRUE)
edge_density(er_graph)
ggraph(er_graph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "grey") +
  geom_node_point(col = "darkgreen", size = 5) +
  theme_graph()

# random graph for RCA >= 1 network
er_rca_graph <- erdos.renyi.game(vcount(rca_sgraph), ecount(rca_sgraph), type = "gnm", directed = TRUE, loops = TRUE)
edge_density(er_rca_graph)
ggraph(er_rca_graph, layout = "stress") +
  geom_edge_link0(width = 0.25, colour = "grey") +
  geom_node_point(col = "darkgreen", size = 5) +
  theme_graph()


edge_density(rca_sgraph, loops = TRUE) / edge_density(sgraph, loops = TRUE)
transitivity(rca_sgraph, type="global") / transitivity(sgraph, type="global")

transitivity(sgraph, type="global")
transitivity(rca_sgraph, type="global")
transitivity(er_graph, type="global")
transitivity(er_rca_graph, type="global")

rel_trans_sgraph <- transitivity(sgraph, type="global") / transitivity(er_graph, type="global")
rel_trans_rca_sgraph <- transitivity(er_graph, type="global") / transitivity(er_rca_graph, type="global")


rel_trans_rca_sgraph / rel_trans_sgraph




# write for gephi
tr_el$rca01_1[is.na(tr_el$rca01_1)==1] <- 0
tr_el$rca01_2[is.na(tr_el$rca01_2)==1] <- 0


export_el <- tr_el %>%
  filter(megye_kod1 == 3 & megye_kod2 == 3) %>%
  select(nace3d1, nace3d2, tb_count, mne_dom_50_1, mne_dom_50_2, rca01_1, rca01_2) %>%
  rename(Source = nace3d1, Target = nace3d2) %>%
  data.table()


export_nodes <- indreg18 %>%
  filter(reg == 3) %>%
  select(ind, rca01) %>%
  rename(Id = ind) %>%
  data.table()

write.table(export_el, "../outputs/gephi_illustration_edgelist.csv", sep=";", row.names = FALSE)
write.table(export_nodes, "../outputs/gephi_illustration_nodelist.csv", sep=";", row.names = FALSE)


