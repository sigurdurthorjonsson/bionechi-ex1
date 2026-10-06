library(tidyverse)

boot <- readRDS("bootResult.rds")
boot %>% filter(para == "SSB", area == "all") -> SSB
saveRDS(SSB$value, file = "ssb-autumn-2025_FINAL.rds")

boot <- readRDS("bootSummary.rds")

paraList <- c("EA", "N", "B", "SSN", "SSB",
  "immN", "immN1", "immN2", "immB", "pSSN3", "pSSB3")

boot %>%
  filter(area == "all", 
    para %in% paraList) %>%
  ungroup() %>%
  select(-area) -> tab
tab <- tab[match(paraList, tab$para),]

## some fixing of units and digits
tab[tab$para == "N", c(2, 4:8)] <- tab[tab$para == "N", c(2, 4:8)]/1e9
tab[tab$para == "B", c(2, 4:8)] <- tab[tab$para == "B", c(2, 4:8)]/1e9
tab[, 2:8] <- round(tab[,2:8], 2)
tab$para <- c("EA", "N", "B", "SSN", "SSB", 
  "ImmN", "ImmN1", "ImmN2", "ImmB", 
  "Prop. N3 in SSN", "Prop. B3 in SSB")

write_csv(tab, "boot_quants.csv")
