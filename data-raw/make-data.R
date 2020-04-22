dist_lookup_table <- read.csv("raw_data/lookup_table.csv")

usethis::use_data(dist_lookup_table, internal=T, overwrite=T)
