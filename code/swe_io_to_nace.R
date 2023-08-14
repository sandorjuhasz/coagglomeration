# 2-digit NACE code based Swedish input-output data construction

library(data.table)
library(dplyr)
library(openxlsx)

# data 
swe_io_df <- fread("../data/io_external/io_elist_2016_swe.csv")
key_table <- data.table(read.xlsx("../data/io_external/ind_swe_hun_corresp.xlsx"))

# count NACE codes by Swedish code
key_table <- key_table %>%
  group_by(ind_swe) %>%
  mutate(nr_swe_code = n()) %>%
  select(-ind_sector) %>%
  data.table()

# join -- allowing duplications
swe_io_df2 <- merge(
  swe_io_df,
  key_table,
  by.x = "ind_i",
  by.y = "ind_swe",
  allow.cartesian = TRUE
)
swe_io_df2 <- merge(
  swe_io_df2,
  key_table,
  by.x = "ind_j",
  by.y = "ind_swe",
  allow.cartesian = TRUE,
  suffixes = c("_i", "_j")
)

# distribute values
swe_io_df2$value_corrected <- swe_io_df2$val / (swe_io_df2$nr_swe_code_i * swe_io_df2$nr_swe_code_j)

# save as a clean edgelist
el <- dplyr::select(swe_io_df2, ind_2dig_i, ind_2dig_j, value_corrected)
write.table(el, "../outputs/swe_io_2digit_nace.csv", sep=";", row.names = FALSE)

