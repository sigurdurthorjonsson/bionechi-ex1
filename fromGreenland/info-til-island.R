library(tidyverse) # uncomment when running separately
# care needs to be taken as we are sourceing this 
# at the end of the hafvog-script 
# which has to pick old tidyverse packages
# to run on the 'old' Oracle XE on my virtual-win-machine.
# The script now joins IS and GL fishData and station objects.

library(readxl)

"IndividualList2025TA07Version2.xlsx" |>
  read_xlsx() |>
    filter(Species=="CAP") |>
    rename(stod=Station,
      ind_nr=IndividualNumber,
      l=`Length`,
      w=`Weight`,
      s=Sex,
      kt=Maturity,
      gw=GonadWeight,
      a=`Age-GN` ,
      nr=`Oto 1-100`
    ) |>
    mutate(w=w*1000,
      sid=stod,
      leid="TARA5-2024",
      s=ifelse(s=="M",1,
        ifelse(s=="F",2,NA)),
      a=as.numeric(a)) -> f

forig <- f

f |> 
  filter(l<=9.0) -> f0

f |>
  filter(l>9.0) -> f

"IndividualList2025TA07.xlsx" |>
  read_xlsx() |>
    filter(Species!="CAP") |>
    rename(stod=Station,
      ind_nr=IndividualNumber,
      l=`Length`,
      w=`Weight`,
      s=Sex,
      kt=Maturity,
      gw=GonadWeight,
      a=`Age-GN` # ,
#      nr=`Oto. No.`
    ) |>
    mutate(w=w*1000,
      sid=stod,
      leid="TARA5-2024",
      s=ifelse(s=="M",1,
        ifelse(s=="F",2,NA))) -> fother

###################################################
#
# aldur yfirskrifaður með endurlestri í Fornubúðum
#
###################################################

#"IndividualList2025TA07_sea.xlsx" |>
#  read_xlsx(sheet="AgeList") |>
#  filter(`Age-GN` != `Age-HAFRO`) |>
#  select(ind_nr=IndividualNumber, a2=`Age-HAFRO`) -> sea
#
#f |>
#  left_join(sea,by=c("ind_nr")) |>
#  mutate(a = ifelse(!is.na(a2), a2, a)) -> f

"StationList2025TA07.xlsx" |>
  read_xlsx() -> o

"StationList2025TA07.xlsx" |>
  read_xlsx() |>
    rename(stod=Station,
    lat1=PosN1Decimal,
    lon1=PosW1Decimal,
    lat2=PosN2Decimal,
    lon2=PosW2Decimal,
    upphaf=TimeStart,
    endir=TimeEnd,
    togtimi=Duration,
    dypik=FishingDepth1,
    dypih=FishingDepth2,
    vir=WireLength1,
    laopn=DoorSpreadAvg,
    toghradi=GPSSpedAvg) |>
  filter(SubGear=="One Codend") |>
  mutate(sid=stod,
    leid="TARA7-2025",
    dags=as.Date(upphaf),
    vf=parse_number(GearID),
    loopn=NA,
    grandl=NA,
    lat = ifelse(is.na(lat2), lat1, (lat1 + lat2)/2),
    lon = ifelse(is.na(lon2), -1*lon1, -1*(lon1 + lon2)/2)) |>
  select(stod,sid,leid,dags,vf,upphaf,endir,dypik,dypih,vir,laopn,loopn,
    grandl,togtimi,toghradi,lat1,lon1,lat2,lon2,lat,lon) |>
  arrange(stod) -> s

## fix positions

library(geo)

# fix for wrong latitude in position
# o is original version of 'StationList'
## stod 51
o |> filter(Station==51) |> select(PosN1,PosN2) -> tmp
tmp$PosN1 <- tmp$PosN1 - 0.4
tmp$PosN2 <- tmp$PosN2 - 0.4
tmp$lat <- (geoconvert(round(10000*tmp$PosN1))+geoconvert(round(10000*tmp$PosN2)))/2
s$lat[s$stod==51] <- tmp$lat
# stod 44
o |> filter(Station==44) |> select(PosN1,PosN2) -> tmp
tmp$PosN2 <- tmp$PosN2 + 0.4
tmp$lat <- (geoconvert(round(10000*tmp$PosN1))+geoconvert(round(10000*tmp$PosN2)))/2
s$lat[s$stod==44] <- tmp$lat
# stod 49
o |> filter(Station==49) |> select(PosW1,PosW2) -> tmp
tmp$PosW1 <- tmp$PosW1 - 1
tmp$lon <- -1*(geoconvert(round(10000*tmp$PosW1))+geoconvert(round(10000*tmp$PosW2)))/2
s$lon[s$stod==49] <- tmp$lon

# fix weights
# ids <- identify(f$l,f$w)
# > ids
# [1]  71 302 442 455 597
# write_rds(ids,"ids.rds")
ids <- read_rds("ids.rds")
f$w[ids] <- NA

fit <- lm(log(w) ~ log(l), f)
f$w <- ifelse(is.na(f$w),
  exp(predict(fit, f)), f$w)

f$kt <- ifelse(is.na(f$kt),
  ifelse(f$l < 14, 1, 3), f$kt)
f$kt <- ifelse(f$kt==3,
  ifelse(f$l < 14, 1, 3), f$kt)
f$kt <- ifelse(f$kt==0,
  ifelse(f$l<14, 1, 3),f$kt)

geoplot(s,type="n",grid=FALSE);geolines(greenland);geotext(s,z=s$stod,cex=1.25)

write_rds(o,"all_stations_orginal.rds")
write_rds(s,"pt_stations.rds")
write_rds(f,"fish_data.rds")
write_rds(f0,"f0_data.rds")
write_rds(forig,"forig_data.rds")
