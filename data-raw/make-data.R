dist_lookup_table <- read.csv("data-raw/lookup_table.csv",
                              stringsAsFactors=F)

usethis::use_data(dist_lookup_table, internal=T, overwrite=T)
