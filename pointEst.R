library(bionechi)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

attach("work.RData")

rdata <- split(rdata, rdata$area)
fishData <- split(fishData, fishData$area)

nAreas <- length(rdata) ## length of split 'rdata' = # areas

eaSum <- numeric(nAreas)

eaSum <- sapply(rdata, function(x) sum(x$EA))
meanSigma <- sapply(fishData, function(x) mean(x$sigma))
totNfish <- eaSum/meanSigma
nSampled <- sapply(fishData, nrow)
nFish <- totNfish/nSampled
fishData <- do.call("rbind", fishData)
fishData$nFish <- nFish[fishData$area]
fishData$bFish <- fishData$nFish*fishData$w

sink("punktmat.txt")
print("Allt svæðið")
print(paste("total B:", round(sum(fishData$bFish)/1e9, 1)))
print(paste("SSB:", round(sum(fishData$bFish[fishData$m==1])/1e9,1)))
print(paste("SSN:", round(sum(fishData$nFish[fishData$m==1])/1e9,1)))
print(paste("Immature N:", round(sum(fishData$nFish[fishData$m==0])/1e9,1)))
print(paste("Immature B:", round(sum(fishData$bFish[fishData$m==0])/1e9,1)))

print("Ókynþroska eftir svæðum")
fishData %>%
  filter(m==0) %>%
  group_by(area) %>%
  summarize(N = sum(nFish)/1e9, B = sum(bFish)/1e9)
print("Kynþroska eftir svæðum")
fishData %>%
  filter(m==1) %>%
  group_by(area) %>%
  summarize(N = sum(nFish)/1e9, B = sum(bFish)/1e9)

print("Ókynþroska eftir aldri")
fishData %>%
  filter(m==0) %>%
  group_by(a) %>%
  summarize(N = sum(nFish)/1e9, B = sum(bFish)/1e9)
print("Kynþroska eftir aldri")
fishData %>%
  filter(m==1) %>%
  group_by(a) %>%
  summarize(N = sum(nFish)/1e9, B = sum(bFish)/1e9)
sink()

fishData %>%
  group_by(area,m,a) %>%
  summarize(N = sum(nFish)/1e9, B = sum(bFish)/1e9) %>%
  write_csv("new_tab.csv")

tabs <- tabulate_byla(fishData)

write.table(tabs$all, "all.txt", na = "", sep = "\t", col.names = NA)
write.table(tabs$mat, "mat.txt", na = "", sep = "\t", col.names = NA)
write.table(tabs$imm, "imm.txt", na = "", sep = "\t", col.names = NA)

save.image("pktEst.RData")
