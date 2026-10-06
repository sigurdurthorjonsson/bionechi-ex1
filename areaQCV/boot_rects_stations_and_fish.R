#install.packages("devtools", repos = "https://cran.hafro.is")
#library(devtools)
#install_github("sigurdurthorjonsson/bionechi")

library(bionechi)

library(dplyr) ; library(tidyr) ; library(stringr)
## assume 6 cores are present, adjust as necessary
library(parallel) ; nCores <- floor(detectCores()/2)

## our data:
attach("../work.RData")
## with two objects, rectangle sA- or NASC-values in 'rdata' and
## fish biology on stations in 'fishData', both assigned to 'area'

options(dplyr.summarise.inform = FALSE)

nReps <- 1e3
set.seed(nReps)

## do this in a multi-core lapply 
bootResult <- mclapply(1:nReps, function(x) {

## bootstrap rectangle EA within areas and sum by area

rdata %>%
  group_by(area) %>%
  sample_frac(size = 1, replace = TRUE) %>%
  summarize(EA = sum(EA)) -> eaSum

## bootstrap fish on stations

fishData %>%
  group_by(stod) %>%
  sample_frac(size = 1, replace = TRUE) -> resampledFishData

## bootstrap stations

fishData %>%
  select(area, stod) %>%
  distinct() %>%
  group_by(area) %>%
  sample_frac(size = 1, replace = TRUE) -> resampledStations

## and use bootstrapped fish on the stations picked

data <- left_join(resampledStations, resampledFishData, 
  by = c("area", "stod"))

## average back-scatter and total EA by area

data %>%
  group_by(area) %>%
  summarize(meanSigma = mean(sigma),
    nSampled = n()) %>%
  left_join(eaSum, by = "area") %>%
  right_join(data, by = "area") -> data

## find total number by area and distribute by length/age/maturity

data %>% 
  group_by(area) %>%
  mutate(totNfish = EA/meanSigma,
    nFish = totNfish/nSampled,
    bFish = nFish*w) %>%
  group_by(area, l, a, m) %>%
  summarize(nFish = sum(nFish),
    bFish = sum(bFish)) %>% group_by(area) -> bootAggr

## pick a range of stock parameters, summarized in own bionechi-function 

bootByArea <- pick_stock_para(bootAggr)

## get the EA by area as well 

bootByArea %>% 
  right_join(eaSum, by = "area") -> bootByArea

## get the totals for all parameters

bootByArea %>% 
  summarize_all(sum, na.rm = TRUE) %>% 
  mutate(area = "all") -> bootAll

## bind the totals to the area results (in a sense a no-no, but handy here)

bootByArea %>% mutate(area = as.character(area)) %>%
  bind_rows(bootAll) -> bootBoth

## calculate a few proportions, tidy/wrangle the output also.
## Note we keep zero entries for missing stock components.
## since they may or may not be present in the bootstrap replicates.

bootBoth %>%
  mutate(pimmN1 = immN1/immN, pimmB1 = immB1/immB,
    pimmN2 = immN2/immN, pimmB2 = immB2/immB,
    pSSN2 = SSN2/SSN, pSSB2 = SSB2/SSB,
    pSSN3 = SSN3/SSN, pSSB3 = SSB3/SSB,
    pSSN4 = SSN4/SSN, pSSB4 = SSB4/SSB) %>%
  gather(para, value, -area) %>%
  mutate(value = ifelse(is.na(value), 0, value))
}, mc.cores = nCores)

bootResult <- bind_rows(bootResult)

## 
saveRDS(bootResult, file = "bootResult.rds")
## to read in, e.g.:
## bootResult <- readRDS("bootResult.rds")

## summarize the results, scale to square kilometers
## thousands of tonnes, billions of individuals
## use 'stringr:str_detect' in 'ifelse' to apply appropriate scaling 

bootResult %>%  
  mutate(value = ifelse(para == "EA", value/1e6, value), 
    value = ifelse(str_detect(para, "^imm"), value/1e9, value),
    value = ifelse(str_detect(para, "^SS"), value/1e9, value)) %>%
  group_by(area, para) %>% 
  do(boot_stats(.$value)) -> bootSummary

saveRDS(bootSummary, file = "bootSummary.rds")

## spool out main results

print(
  bootSummary %>% filter(para == "EA") %>% 
    select(area:pt50) %>% select(-pt25))
print(
  bootSummary %>% filter(para == "SSB") %>% 
    select(area:pt50) %>% select(-pt25))
print(
  bootSummary %>% filter(para == "immN") %>% 
    select(area:pt50) %>% select(-pt25))

source("uttak.R")

print("Bittenú, húrra")
