# distance decay plot experiments -- sandorjuhasz


library(data.table)
library(dplyr)
library(betapart)


# distance table - to - matrix
dist <- fread("../outputs/nuts4_nuts4_distance.csv")
dist_mat <- as.matrix(xtabs(distance ~ region_code1 + region_code2, data = dist))


# IO and labor table - to - matrix
io_el <- fread("../data/oc_el_transactions_nace3d_NUTS4_2019_1.csv")
io_table <- io_el %>%
  group_by(reg1, reg2) %>%
  summarise(w = sum(nr_firm_to_firm)) %>%
  data.table()
io_mat <- as.matrix(xtabs(w ~ reg1 + reg2, data = io_table))

labor_el <- fread("../data/oc_el_labor_flows_nace3d_NUTS4_2019_1.csv")
labor_table <- labor_el %>%
  group_by(reg1, reg2) %>%
  summarise(w = sum(nr_firm_to_firm)) %>%
  data.table()
labor_mat <- as.matrix(xtabs(w ~ reg1 + reg2, data = labor_table))



io_sim <- beta.pair(io_mat)$beta.sor



decay <- decay.model(y=io_mat, x=dist_mat, y.type="sim", model.type="power")




# presence/absence tables for longhorn beetles of South and North Europe
data(ceram.s)
data(ceram.n)

# spatial coordinates of territories in South and North Europe
data(coords.s)
data(coords.n)

# dissimilarity matrices
ceram.s.sim <- beta.pair(ceram.s)$beta.sim
ceram.n.sim <- beta.pair(ceram.n)$beta.sim

# spatial distances in km
distgeo.s <- dist(coords.s[,1:2])
distgeo.n <- dist(coords.n[,1:2])

# Negative exponential models for the decay of similarity with spatial distance
decay.south <- decay.model(y=1-ceram.s.sim, x=distgeo.s, y.type="sim", model.type="power")
decay.north <- decay.model(y=1-ceram.n.sim, x=distgeo.n, y.type="sim", model.type="exp")

# Plot the decay models
plot.decay(decay.south, col="red", cex.lab=1.5, cex.axis=1.5, axes=FALSE)
plot.decay(decay.north, col="blue", add=TRUE)
axis(2)
axis(1)

# Equivalent models for the increase of dissimilarity with spatial distance
increase.south <- decay.model(y=ceram.s.sim, x=distgeo.s, y.type="dissim", model.type="exp")
increase.north <- decay.model(y=ceram.n.sim, x=distgeo.n, y.type="dissim", model.type="exp")

# Plot the decay models
plot.decay(increase.south, col="red")
plot.decay(increase.north, col="blue", add=TRUE)







# manual reconstruction
io_el <- fread("../data/oc_el_transactions_nace3d_NUTS4_2019_1.csv")
io_table <- io_el %>%
  group_by(reg1, reg2) %>%
  summarise(
    w = sum(nr_firm_to_firm),
    nr_firms1 = sum(nr_firm_1),
    nr_firms2 = sum(nr_firm_2)
  ) %>%
#  mutate(realized = w / ( (nr_firms1 * (nr_firms2 - 1))/2)) %>%
  data.table()

labor_el <- fread("../data/oc_el_labor_flows_nace3d_NUTS4_2019_1.csv")
labor_table <- labor_el %>%
  group_by(reg1, reg2) %>%
  summarise(
    w = sum(nr_firm_to_firm),
    nr_firms1 = sum(nr_firm_1),
    nr_firms2 = sum(nr_firm_2)
  ) %>%
  #  mutate(realized = w / ( (nr_firms1 * (nr_firms2 - 1))/2)) %>%
  data.table()



# add IO and labor to full distance table
dist_table <- merge(
  dist,
  io_table,
  by.x = c("region_code1", "region_code2"),
  by.y = c("reg1", "reg2"),
  all.x = TRUE,
  all.y = FALSE
)
dist_table <- merge(
  dist_table,
  labor_table,
  by.x = c("region_code1", "region_code2"),
  by.y = c("reg1", "reg2"),
  all.x = TRUE,
  all.y = FALSE,
  suffixes = c("_io", "_labor")
)

# variable manipulation
dist_table$w_io[is.na(dist_table$w_io)==1] <- 0
dist_table$w_labor[is.na(dist_table$w_labor)==1] <- 0
dist_table$dist_int <- as.integer(dist_table$dist)
dist_table$dist_int <- dist_table$dist_int %/% 10 * 10

# group by distance bin
plot_table <- dist_table %>%
  group_by(dist_int) %>%
  summarise(
    obs_io = sum(w_io),
    obs_labor = sum(w_labor)
  ) %>%
  mutate(
    log_dist = log10(dist_int + 1),
    log_obs_io = log10(obs_io + 1),
    log_obs_labor = log10(obs_labor + 1)
  ) %>%
  data.table()


plot(plot_table$log_dist, plot_table$log_obs_io, type = "b", pch = 19, col = "red")
lines(plot_table$log_dist, plot_table$log_obs_labor, type = "b", pch = 19, col = "darkgreen")

# add IO to full distance table
labor_dist <- merge(
  dist,
  labor_table,
  by.x = c("region_code1", "region_code2"),
  by.y = c("reg1", "reg2"),
  all.x = TRUE,
  all.y = FALSE
)
labor_dist$w[is.na(labor_dist$w)==1] <- 0
labor_dist$dist_int <- as.integer(labor_dist$dist)


# group by distance bin
labor_dist <- labor_dist %>%
  group_by(dist_int) %>%
  summarise(obs = sum(w)) %>%
  mutate(
    log_dist = log10(dist_int + 1),
    log_obs = log10(obs + 1)
  ) %>%
  data.table()


plot(labor_dist$log_dist, labor_dist$log_obs, type = "b", pch = 19, col = "darkgreen")
lines(labor_dist$log_dist, io_dist$log_obs, type = "b", pch = 19, col = "red")

