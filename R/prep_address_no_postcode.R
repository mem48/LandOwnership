# No postocdes
library(purrr)
source("R/address_functions.R")

freehold_nopc = readRDS("data/UK_freehold_nopc.Rds") # 477,044

# SOme of these have clear addresses

# Remove some problems
freehold_nopc$n_char <- nchar(freehold_nopc$`Property Address`)
#freehold_nopc_good <- freehold_nopc[freehold_nopc$n_char < 84,]

freehold_nopc_good = freehold_nopc[!grepl("substation", freehold_nopc$`Property Address`,ignore.case = TRUE),]
freehold_nopc_good = freehold_nopc_good[!grepl("sub-station", freehold_nopc_good$`Property Address`,ignore.case = TRUE),]
freehold_nopc_good = freehold_nopc_good[!grepl("sub station", freehold_nopc_good$`Property Address`,ignore.case = TRUE),]
freehold_nopc_good = freehold_nopc_good[!grepl("part of", freehold_nopc_good$`Property Address`,ignore.case = TRUE),]

#freehold_nopc_good$AddressLine <- map_chr(freehold_nopc_good$`Property Address`, clean_numbers)

AddressLine <- map(freehold_nopc_good$`Property Address`, split_numbers_try)
reps <- lengths(AddressLine)
AddressLine <- unlist(AddressLine)

freehold_nopc_good2 <- freehold_nopc_good[rep(1:nrow(freehold_nopc_good), times = reps),]
freehold_nopc_good2$AddressLine <- AddressLine

freehold_nopc_bad <- freehold_nopc[!freehold_nopc$Id %in% freehold_nopc_good2$Id,]

saveRDS(freehold_nopc_good2, "data/UK_freehold_nopc_simple.Rds") # 1,830,024
saveRDS(freehold_nopc_bad, "data/UK_freehold_nopc_complex.Rds") # 111,191 - lots of sub stations
