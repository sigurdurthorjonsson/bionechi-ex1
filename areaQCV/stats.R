library(tidyverse)

"ssb_haust2020_1e5.rds" %>%
  readRDS() -> cm ########## centi-milli, eitt hundrað þúsund

"ssb_haust2020_1e6.rds" %>%
  readRDS() -> mm ########## milli-milli, ein milljón endurtekninga

#tibble(gr=rep(1:10,length(cm)/10),ssb=cm) %>%
tibble(gr=rep(1:10,rep(1e4,10)),ssb=cm) %>%
  group_by(gr) %>%
  summarize(ssb=mean(ssb)/1e9) %>%
  pull(ssb) %>%
  sd()

#tibble(gr=rep(1:10,length(mm)/10),ssb=mm) %>%
tibble(gr=rep(1:10,rep(1e5,10)),ssb=mm) %>%
  group_by(gr) %>%
  summarize(ssb=mean(ssb)/1e9) %>%
  pull(ssb) %>%
  sd()
