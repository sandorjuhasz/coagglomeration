# distance decay plot experiments -- sandorjuhasz


library(data.table)
library(dplyr)



# manual reconstruction
io_el <- fread("../data/oc_el_transactions_nace3d_NUTS4_2019_1.csv")
io_table <- io_el %>%
  group_by(reg1, reg2) %>%
  summarise(
    w = sum(nr_firm_to_firm),
    nr_firms1 = sum(nr_firm_1),
    nr_firms2 = sum(nr_firm_2),
    prob_w = w / ((nr_firms1 * (nr_firms2 - 1)) / 2)
  ) %>%
#  mutate(realized = w / ( (nr_firms1 * (nr_firms2 - 1))/2)) %>%
  data.table()


labor_el <- fread("../data/oc_el_labor_flows_nace3d_NUTS4_2019_1.csv")
labor_table <- labor_el %>%
  group_by(reg1, reg2) %>%
  summarise(
    w = sum(nr_firm_to_firm),
    nr_firms1 = sum(nr_firm_1),
    nr_firms2 = sum(nr_firm_2),
    prob_w = w / ((nr_firms1 * (nr_firms2 - 1)) / 2)
  ) %>%
  #  mutate(realized = w / ( (nr_firms1 * (nr_firms2 - 1))/2)) %>%
  data.table()


# distance table
dist <- fread("../outputs/nuts4_nuts4_distance.csv")


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
dist_table$prob_w_io[is.na(dist_table$prob_w_io)==1] <- 0
dist_table$prob_w_labor[is.na(dist_table$prob_w_labor)==1] <- 0
dist_table$dist_int <- as.integer(dist_table$dist)
dist_table$dist_int <- dist_table$dist_int %/% 10 * 10


# group by distance bin
plot_table <- dist_table %>%
  group_by(dist_int) %>%
  summarise(
    obs_io = sum(w_io),
    obs_labor = sum(w_labor),
    prob_w_io = sum(prob_w_io),
    prob_w_labor = sum(prob_w_labor)
  ) %>%
  mutate(
    log_dist = log10(dist_int + 1),
    log_obs_io = log10(obs_io + 1),
    log_obs_labor = log10(obs_labor + 1),
    log_prob_io = log10(prob_w_io + 1),
    log_prob_labor = log10(prob_w_labor + 1)
  ) %>%
  data.table()


# log version
plot(
  plot_table$log_dist,
  plot_table$log_obs_io,
  type = "b",
  pch = 19,
  col = "red",
  frame.plot=FALSE,
  xlab="Distance (log)",
  ylab="Ties observed (log)",
  cex.lab=2,
  cex.axis=2
)
lines(plot_table$log_dist, plot_table$log_obs_labor, type = "b", pch = 19, col = "darkgreen")


# linear version
plot(plot_table$dist_int, plot_table$log_obs_io, type = "b", pch = 19, col = "red")
lines(plot_table$dist_int, plot_table$log_obs_labor, type = "b", pch = 19, col = "darkgreen")



# all possible / realized connections
plot(plot_table$log_dist, plot_table$log_prob_io, type = "b", pch = 19, col = "red")
lines(plot_table$log_dist, plot_table$log_prob_labor, type = "b", pch = 19, col = "darkgreen")

