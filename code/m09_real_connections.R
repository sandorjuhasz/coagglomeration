## industry-region matrix manipulation -- coagglomeration and MNEs
## developed by sandorjuhasz


library(data.table)
library(dplyr)
library(igraph)
library(ggraph)   
library(graphlayouts)
library(stargazer)

# parameters
reg <- "megye"
ind <- 3

# data sources
ir_el <- fread("../data/oc_2023_march_labor/m01_ir_ir_el_output_OC.csv")
indreg_gr <- fread("../data/oc_2022_november/indreg_2018_2020_megye.csv")
region_codes <- fread("../data/oc_2022_november/region_codes.csv", sep = ";")
mne_df <- fread("../data/oc_2022_november/mne_share_nace3d_megye.csv", sep=";") %>%
  rename(reg = megye_kod, ind = nace3d) %>%
  data.table()



# generate industry-region ids
ir_el$ir_id1 <- paste0(ir_el$reg1, "-", ir_el$ind1)
ir_el$ir_id2 <- paste0(ir_el$reg2, "-", ir_el$ind2)
indreg_gr$ir_id <- paste0(indreg_gr$reg, "-", indreg_gr$ind)
mne_df$ir_id <- paste0(mne_df$reg, "-", mne_df$ind)

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

create_regional_network <- function(el, reg, weight, rca_filter, directed){
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
  
  if(directed == 1)
  {
    dir = TRUE
  } else
  {
    dir = FALSE
  }
  
  # create the network
  net <- graph_from_data_frame(el[(reg1 == reg & reg2 == reg), ..cols], directed = dir)
  
  # add RCA to network
  
  return(net)
}

# different network version for an example region
io_graph <- create_regional_network(ir_el, reg = 3, weight = "nr_buy_ties", rca_filter = FALSE, directed = TRUE)
lab_graph <- create_regional_network(ir_el, reg = 3, weight = "nr_labor_ties", rca_filter = FALSE, directed = TRUE)
io_graph_rca <- create_regional_network(ir_el, reg = 3, weight = "nr_buy_ties", rca_filter = TRUE, directed = TRUE)
lab_graph_rca <- create_regional_network(ir_el, reg = 3, weight = "nr_labor_ties", rca_filter = TRUE, directed = TRUE)


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





###### gephi export ######
export_el <- ir_el %>%
  filter(reg1 == 3 & reg2 == 3) %>%
  select(ind1, ind2, nr_buy_ties, nr_labor_ties) %>%
  rename(Source = ind1, Target = ind2) %>%
  data.table()

export_nodes <- unique(data.table(Id = unique(c(export_el$Source, export_el$Target))))
export_nodes <- merge(
  export_nodes,
  unique(select(filter(ir_el, reg1 == 3), ind1, rca011, mne_count1)),
  by.x = "Id",
  by.y = "ind1",
  all.x = TRUE,
  all.y = FALSE
)
export_nodes <- merge(
  export_nodes,
  unique(select(filter(ir_el, reg2 == 3), ind2, rca012, mne_count2)),
  by.x = "Id",
  by.y = "ind2",
  all.x = TRUE,
  all.y = FALSE
)
export_nodes$rca <- ifelse(export_nodes$rca011 == 1 | export_nodes$rca012 == 1, 1, 0)
export_nodes$mne <- ifelse(export_nodes$mne_count1 > 0 | export_nodes$mne_count2 > 0, 1, 0)
export_nodes <- select(export_nodes, Id, rca, mne)


write.table(subset(export_el, nr_buy_ties > 0), "../outputs/gephi_illustration_buy_edgelist.csv", sep=";", row.names = FALSE)
write.table(subset(export_el, nr_labor_ties > 0), "../outputs/gephi_illustration_labor_edgelist.csv", sep=";", row.names = FALSE)
write.table(export_nodes, "../outputs/gephi_illustration_nodelist.csv", sep=";", row.names = FALSE)




###### industry-region level table of network indicators

# baseline regression
indreg_gr$log_emp18 <- log10(indreg_gr$total_emp18)
indreg_gr$log_emp20 <- log10(indreg_gr$total_emp20)
indreg_gr$emp_growth <- indreg_gr$log_emp20 / indreg_gr$log_emp18
indreg_gr$log_nr_firms18 <- log10(indreg_gr$nr_firms18)
indreg_gr <- merge(
  indreg_gr,
  select(mne_df, ir_id, mne_count, mne_emp, mne_dom_25, mne_dom_50),
  by = "ir_id",
  all.x = TRUE,
  all.y = FALSE
)
summary(m01 <- lm(emp_growth ~ mne_dom_25 + rca01 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr))


create_industry_region_network <- function(el, weight_col)
{
  # key cols for the edgelist
  cols <- c("indreg1", "indreg2", weight_col)
  
  # filter for weight
  el <- el %>%
    filter(!!as.symbol(weight_col) > 0)
  
  # create the network
  net <- graph_from_data_frame(el[, ..cols], directed = TRUE)
  
  return(net)
}


# full industry-region network
io_irnet <- create_industry_region_network(ir_el, weight_col = "nr_buy_ties")
full_io_irnet_table <- data.table(
  indreg_id = V(io_irnet)$name,
  full_degree = degree(io_irnet, mode = "all"),
  full_degree_cent = degree(io_irnet, mode = "all", normalized = TRUE),
  full_indegree = degree(io_irnet, mode = "in"),
  full_indegree_cent = degree(io_irnet, mode = "in", normalized = TRUE),
  full_outdegree = degree(io_irnet, mode = "out"),
  full_outdegree_cent = degree(io_irnet, mode = "out", normalized = TRUE)
)
lab_irnet <- create_industry_region_network(ir_el, weight_col = "nr_labor_ties")
full_lab_irnet_table <- data.table(
  indreg_id = V(lab_irnet)$name,
  full_degree = degree(lab_irnet, mode = "all"),
  full_degree_cent = degree(lab_irnet, mode = "all", normalized = TRUE),
  full_indegree = degree(lab_irnet, mode = "in"),
  full_indegree_cent = degree(lab_irnet, mode = "in", normalized = TRUE),
  full_outdegree = degree(lab_irnet, mode = "out"),
  full_outdegree_cent = degree(lab_irnet, mode = "out", normalized = TRUE)
)
full_network_table <- merge(
  full_io_irnet_table,
  full_lab_irnet_table,
  by = "indreg_id",
  all.x = TRUE,
  all.y = TRUE,
  suffixes = c("_io", "_lab")
)


# regressions
indreg_gr2 <- merge(
  indreg_gr,
  full_network_table,
  by.x = "ir_id",
  by.y = "indreg_id",
  all.x = TRUE,
  all.y = FALSE
)
summary(m02 <- lm(emp_growth ~ full_degree_cent_io + full_degree_cent_lab + mne_dom_25 + rca01 + log_emp18 + log_nr_firms18 + as.factor(reg), data = indreg_gr2))
summary(m02in <- lm(emp_growth ~ full_indegree_cent_io + full_indegree_cent_lab + mne_dom_25 + rca01 + log_emp18 + log_nr_firms18 + as.factor(reg), data = indreg_gr2))
summary(m02out <- lm(emp_growth ~ full_outdegree_cent_io + full_outdegree_cent_lab + mne_dom_25 + rca01 + log_emp18 + log_nr_firms18 + as.factor(reg), data = indreg_gr2))

stargazer(m02, m02in, m02out,
          omit = c("reg"),
          omit.labels = ("Region FE"),
          omit.stat=c("f", "ser"),
          dep.var.labels = "Employment growth 2020-2018",
          dep.var.caption = "",
          covariate.labels = c("IO degree cent",
                               "Labor degree cent",
                               "IO IN degree cent",
                               "Labor IN degree cent",
                               "IO OUT degree cent",
                               "Labor OUT degree cent",
                               "MNE dominance",
                               "RCA 0/1",
                               "Log emp 2018",
                               "Log nr firms 2018"),
          out="../outputs/growth_regression_m02.html")





# local industry-region network
local_io_irnet <- create_industry_region_network(
  subset(ir_el, reg1 == reg2),
  weight_col = "nr_buy_ties"
)
local_io_irnet_table <- data.table(
  indreg_id = V(local_io_irnet)$name,
  local_degree = degree(local_io_irnet, mode = "all"),
  local_degree_cent = degree(local_io_irnet, mode = "all", normalized = TRUE),
  local_indegree = degree(local_io_irnet, mode = "in"),
  local_indegree_cent = degree(local_io_irnet, mode = "in", normalized = TRUE),
  local_outdegree = degree(local_io_irnet, mode = "out"),
  local_outdegree_cent = degree(local_io_irnet, mode = "out", normalized = TRUE)
)
local_lab_irnet <- create_industry_region_network(
  subset(ir_el, reg1 == reg2),
  weight_col = "nr_labor_ties"
)
local_lab_irnet_table <- data.table(
  indreg_id = V(local_lab_irnet)$name,
  local_degree = degree(local_lab_irnet, mode = "all"),
  local_degree_cent = degree(local_lab_irnet, mode = "all", normalized = TRUE),
  local_indegree = degree(local_lab_irnet, mode = "in"),
  local_indegree_cent = degree(local_lab_irnet, mode = "in", normalized = TRUE),
  local_outdegree = degree(local_lab_irnet, mode = "out"),
  local_outdegree_cent = degree(local_lab_irnet, mode = "out", normalized = TRUE)
)
local_network_table <- merge(
  local_io_irnet_table,
  local_lab_irnet_table,
  by = "indreg_id",
  all.x = TRUE,
  all.y = TRUE,
  suffixes = c("_io", "_lab")
)


# regressions
indreg_gr3 <- merge(
  indreg_gr,
  local_network_table,
  by.x = "ir_id",
  by.y = "indreg_id",
  all.x = TRUE,
  all.y = FALSE
)
summary(m03 <- lm(emp_growth ~ local_degree_cent_io + local_degree_cent_lab + mne_dom_25 + rca01 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr3))
summary(m03in <- lm(emp_growth ~ local_indegree_cent_io + local_indegree_cent_lab + mne_dom_25 + rca01 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr3))
summary(m03out <- lm(emp_growth ~ local_outdegree_cent_io + local_outdegree_cent_lab + mne_dom_25 + rca01 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr3))

stargazer(m03, m03in, m03out,
          omit = c("reg"),
          omit.labels = ("Region FE"),
          omit.stat=c("f", "ser"),
          dep.var.labels = "Employment growth 2020-2018",
          dep.var.caption = "",
          covariate.labels = c("Local IO degree cent",
                               "Local labor degree cent",
                               "Local IO IN degree cent",
                               "Local labor IN degree cent",
                               "Local IO OUT degree cent",
                               "Local labor OUT degree cent",
                               "MNE dominance",
                               "RCA 0/1",
                               "Log emp 2018",
                               "Log nr firms 2018"),
          out="../outputs/growth_regression_m03.html")







# local RCA industry-region network
rca_io_irnet <- create_industry_region_network(
  subset(ir_el, reg1 == reg2 & rca011 == 1 & rca012 == 1),
  weight_col = "nr_buy_ties"
)
rca_io_irnet_table <- data.table(
  indreg_id = V(rca_io_irnet)$name,
  rca_degree = degree(rca_io_irnet, mode = "all"),
  rca_degree_cent = degree(rca_io_irnet, mode = "all", normalized = TRUE),
  rca_indegree = degree(rca_io_irnet, mode = "in"),
  rca_indegree_cent = degree(rca_io_irnet, mode = "in", normalized = TRUE),
  rca_outdegree = degree(rca_io_irnet, mode = "out"),
  rca_outdegree_cent = degree(rca_io_irnet, mode = "out", normalized = TRUE)
)
rca_lab_irnet <- create_industry_region_network(
  subset(ir_el, reg1 == reg2 & rca011 == 1 & rca012 == 1),
  weight_col = "nr_labor_ties"
)
rca_lab_irnet_table <- data.table(
  indreg_id = V(rca_lab_irnet)$name,
  rca_degree = degree(rca_lab_irnet, mode = "all"),
  rca_degree_cent = degree(rca_lab_irnet, mode = "all", normalized = TRUE),
  rca_indegree = degree(rca_lab_irnet, mode = "in"),
  rca_indegree_cent = degree(rca_lab_irnet, mode = "in", normalized = TRUE),
  rca_outdegree = degree(rca_lab_irnet, mode = "out"),
  rca_outdegree_cent = degree(rca_lab_irnet, mode = "out", normalized = TRUE)
)
rca_network_table <- merge(
  rca_io_irnet_table,
  rca_lab_irnet_table,
  by = "indreg_id",
  all.x = TRUE,
  all.y = TRUE,
  suffixes = c("_io", "_lab")
)



# regressions
indreg_gr4 <- merge(
  indreg_gr,
  rca_network_table,
  by.x = "ir_id",
  by.y = "indreg_id",
  all.x = TRUE,
  all.y = FALSE
)
indreg_gr4 <- subset(indreg_gr4, rca01 == 1)
summary(m04 <- lm(emp_growth ~ rca_degree_cent_io + rca_degree_cent_lab + mne_dom_25 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr4))
summary(m04in <- lm(emp_growth ~ rca_indegree_cent_io + rca_indegree_cent_lab + mne_dom_25 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr4))
summary(m04out <- lm(emp_growth ~ rca_outdegree_cent_io + rca_outdegree_cent_lab + mne_dom_25 + log_emp18 + nr_firms18 + as.factor(reg), data = indreg_gr4))


stargazer(m04, m04in, m04out,
          omit = c("reg"),
          omit.labels = ("Region FE"),
          omit.stat=c("f", "ser"),
          dep.var.labels = "Employment growth 2020-2018 RCA == 1",
          dep.var.caption = "",
          covariate.labels = c("IO degree cent to RCA",
                               "Labor degree cent to RCA",
                               "IO IN degree cent to RCA",
                               "Labor IN degree cent to RCA",
                               "IO OUT degree cent to RCA",
                               "Labor OUT degree cent to RCA",
                               "MNE dominance",
                               "Log emp 2018",
                               "Log nr firms 2018"),
          out="../outputs/growth_regression_m04.html")







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


