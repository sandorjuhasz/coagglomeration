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
tr_df <- fread("../data/oc_2022_november/transactions_indreg_nace3d_megye.csv")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv")
indreg18 <- fread("../data/oc_2022_november/indreg_2018_2020_megye.csv")
region_codes <- fread("../data/oc_2022_november/region_codes.csv", sep = ";")



# generate industry-region ids
tr_df$ir_id1 <- paste0(tr_df$megye_kod1, "-", tr_df$nace3d1)
tr_df$ir_id2 <- paste0(tr_df$megye_kod2, "-", tr_df$nace3d2)
mne_df$ir_id <- paste0(mne_df$megye_kod, "-", mne_df$nace3d)



# transaction between MNE-local / MNE-local 2*2 matrix
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





# network visuals and comparison
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


