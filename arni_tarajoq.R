library(tidyverse)
library(bionechi)

# til að r_within_peri og öll ??_within_peri föllin virki, 
## þarf að finna einhverja
## árans geo-pojection einhvers staðar, keyri því þetta:
pdf(file="null.pdf")
geoplot()
dev.off()

peri <- read_gpx_rte("gpx/peri.gpx")

# Tarajoq
## ts
"nasc/ListUserFile03__F038000_T1_L436.6-3038.1.txt" |>
  read_luf3() -> tarajoq
## stj
#"nasc/ListUserFile03__F038000_T1.txt" |>
#  read_luf3() -> tarajoq

# Árni Frikk

# TS
"nasc/ListUserFile03__F038000_T1_L211.8-3302.1.txt" |>
  read_luf3() -> arniT1
"nasc/ListUserFile03__F038000_T2_L211.8-3302.1.txt" |>
  read_luf3() -> arniT2

arniT2 |>
  bind_rows(arniT1) -> arni

########## á reiti:

# sameina
tarajoq |>
  select(ship:lat,cap) |>
  bind_rows(arni |>
    select(ship:lat,cap))  -> both

## smáaðferðafræðiuppherzla

both <- geoinside(both,peri)

both |>
  mutate(r=d2r(both)) |>
  group_by(r) |>
  summarize(cap=mean(cap)) |>
  mutate(A=rA(r)) |>
  filter(cap > 0) -> rdata
 rdata$p <- r_within_peri(rdata$r,peri)
rdata |>
  mutate(
    EA=cap*p*A) -> rdata

#rbind(seq(9,12,by=0.5),
#  sum(rdata$EA)/ts2sigma(le2ts(seq(9,12,by=0.5),31))/1e9)

r2area <- read.table("r2area.txt", header = TRUE)
# og endilega athuga hvort einhverjir reitir tilheyri ekki svæðum
rdata <- merge(rdata, r2area, both.x = T)

attach("hafvog/hafvogarGogn.RData")

  fishData <- fishData
  stations <- stations
  r <- r

detach()

mean(fishData$w)*sum(rdata$EA)/ts2sigma(le2ts(mean(fishData$l),31))/1e9

####
## vel stöðvar (sleppi einhverju rusli með fáeinar loðnur):
st_used<-c(8, 30, 34, 40, 41, 44, 47, 48, 49, 50, 51,
  359, 366, 369, 371, 372, 375, 388, 395, 
  400, 402, 404, 406, 407, 408, 409, 410)

#fishData <- fishData[fishData$stod %in% st_used, ]
## spái fyrir um bothar þyngdir
# þarf ekki í þetta sinn (sem betur fer), þegar búið á grænlenska settið
#fit <- lm(log(w) ~ log(l), fishData)
## fylli með spáþyngdum þar sem þyngd vantar
#fishData$w <- ifelse(is.na(fishData$w),
#  exp(predict(fit, fishData)), fishData$w)
# set missing kynþroski sem ókynþroska eins og ávbotht
fishData$m <- ifelse(fishData$kt %in% c(0:2, NA), 0, 1)
fishData <- fix_missing_age(fishData)
# heldur spáum
fishData$TS <- le2ts(fishData$l, 31)
fishData$sigma <- ts2sigma(fishData$TS)
st2area <- read.table("st2area.txt", header = TRUE)
### hafa bothar stöðvar inni:
#fishData <- merge(fishData, st2area, both.x = TRUE)
## eða hér til að skilja á milli yfirferða, aðeins tilgreindar
fishData <- merge(fishData, st2area)

#stations <- stations[stations$stod %in% st_used,]
tmp <- table(fishData$stod, fishData$m)
tmp <- sweep(tmp,1,apply(tmp,1,sum),"/")[,2]
stations$pmat <- tmp[match(stations$stod,names(tmp))]
stations <- stations[order(stations$stod), ]
stations$r <- d2r(stations)
st2area <- read.table("st2area.txt", header = TRUE)
## til að sýna bothar stöðvar
#stations <- merge(stations, st2area, both.x = TRUE)
## eða til að skilja á milli yfirferða
stations <- stations[stations$stod %in% st2area$stod, ]
stations <- merge(stations, st2area)

fishData |>
  filter(m==1) |>
  group_by(stod) |>
  summarize(n=n()) |>
  left_join(fishData |>
    filter(m==1,a==3) |>
    group_by(stod) |>
    summarize(n3=n()),by="stod") |>
  mutate(p3=n3/n) -> tmp

stations |>
  left_join(tmp |>
     select(stod,p3),by="stod") |>
  left_join(r, by="stod") -> stations


pdf(file = "figs.pdf")

mylim <- list(lat = c(63.95,72.25), lon = c(-40.05, -8.95))

oldpar <- par(mfrow=c(1,2), omi = c(0, 0, 0.5, 0))

plot(tarajoq$logstart,tarajoq$cap, type = "h")
mtext("Tarajoq")
plot(arni$logstart,arni$cap, type = "h")
mtext("Árni Friðriksson")
mtext("EDSU NASC - 0.1 nmi bins", outer = TRUE)

par(oldpar)

mycol <- c("lavender","palegreen","mistyrose", 
           "rosybrown", "thistle", "pink", "seashell")

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r, col = mycol[1])
gbplot(500, lty = 2, col = "grey")
geolines(peri, col="orange", lwd = 2)
geolines(arni, col = "lightblue")
tmp <- arni[arni$cap > 0,]
geosymbols(tmp, z=tmp$cap, perbars = 0.5,
  col = "blue", maxn = max(both$cap))
geolines(tarajoq, col = "lightgreen")
tmp <- tarajoq[tarajoq$cap > 0,]
geosymbols(tmp, z=tmp$cap, perbars = 0.5,
  col = "green", maxn = max(both$cap))

geoplot(both, type = "n", grid = F, xlim = mylim)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r,col = "white")
gbplot(500, lty = 2, col = "grey")
geopoints(arni, col = "blue")
geopoints(tarajoq, col = "green")
mtext("Position of interpreted data values by vessel")
geolines(peri, col="darkmagenta", lwd = 1)

####### to be completed
#
#geoplot(both.1998, type = "n", grid = F)
#mycol <- topo.colors(nrow(A12.1998))
#geopoints(A12.1998, col = mycol, cex = 1.15)
#mycol <- topo.colors(nrow(B12.1998))
#geopoints(B12.1998, col = mycol, cex = 1.15)
#mtext("A12-1998 & B12-1998 - 14.11 - 27.11")
#
#########

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
gbplot(500, lty = 2, col = "grey") 
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "lightgreen")
geotext(stations,z=stations$stod)
mtext("PT stations")

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
gbplot(500, lty = 2, col = "grey")
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "green")
tmp <- read.table("st2area.txt", header = TRUE)
tmp <- merge(tmp, stations, both = TRUE)
geotext(tmp, z = tmp$area)
mtext("PT area bothocations")

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "green")
gbplot(500, lty = 2, col = "grey")
tmp <- stations
#tmp$pmat <- ifelse(is.na(tmp$pmat), " ", round(tmp$pmat, 2))
geotext(tmp, z = tmp$pmat, digits = 2)
mtext("Pmat on stations")


geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "green")
gbplot(500, lty = 2, col = "grey")
geotext(stations, z = stations$p3, digits = 2)
mtext("Pag3 in SSB on stations")

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "green")
gbplot(500, lty = 2, col = "grey")
geotext(stations, z = stations$roecont, digits = 2)
mtext("Roe content on stations")

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "green")
gbplot(500, lty = 2, col = "grey")
geolines(peri, col="orange", lwd = 2)
geotext(r2d(rdata$r),z=rdata$p, digits = 2, cex = 0.7)
mtext("Rectangle area proportion")

geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
rgrid(rdata$r,fill=T,col=mycol[rdata$area])
rgrid(rdata$r, col = "white")
gbplot(500, lty = 2, col = "grey")
geolines(arni, lty = 2, col = "blue")
geolines(tarajoq, lty = 2, col = "green")
geotext(r2d(rdata$r),z=rdata$cap, cex = 0.75, angle = 45)
geolines(peri,col="orange",lwd=2)
mtext("Rectangle average NASC")

tmp <- both

op <- par("mar")
par(mar=c(0.9,0.9,0,0),mfrow=c(1,1))

lodna.grid <- list(lat = seq(range(tmp$lat)[1], range(tmp$lat)[2],by=0.1),
  lon=seq(range(tmp$lon)[1],range(tmp$lon)[2],by=0.1))

lodna.sa.levels<-c(10,50,100,500,1000,1500)
seven.col <- c("white","cyan","green4","green","yellow","orange","red")
  
vg1 <- list(nugget=0.01,sill=1,range=25)
  
geoplot(ylim=c(63.95,72.55),xlim=c(-40.05,-8.95),grid=F,axlabels=F)
geoaxis(side=1,dlon=4,inside=F,cex=0.8)
geoaxis(side=2,dlat=2,inside=F,cex=0.8)
gbplot(500,col="grey")
  
  
lodna.zgr <- pointkriging(lat=tmp$lat,lon=tmp$lon,z=tmp$cap,
  xgr=lodna.grid,vagram=vg1,option=1,maxdist=25,minnumber=10)
geocontour.fill(lodna.zgr,levels=lodna.sa.levels,white=T,col=seven.col)
geopolygon(island, col = "white")
geolines(island)
geolegend(pos = list(lat = 70.5, lon = -39.5), 
  legend = c("<10", "10-50", "50-100", "100-500", "500-1000", 
    "1000-1500", ">1500"), fill = seven.col, cex = 1, bg= "white")
geolines(greenland)
#geolines(twohmiles,col="black",lwd=2)
geolines(arni, lty = 2, col = "green")
geolines(tarajoq, lty = 2, col = "blue")

par(mar=op)


dev.off()


save.image(file = "work.RData")

pdf(file = "r.pdf")
geoplot(both, type = "n", grid = F, xlim = mylim)
geolines(greenland)
geolines(both, lty = 2, col = "blue")
geolines(peri, lty = 1, col = "orange")
rgrid(rdata$r, col = "grey")
geotext(r2d(rdata$r),z=rdata$r, cex = 1, angle = 45)
dev.off()
