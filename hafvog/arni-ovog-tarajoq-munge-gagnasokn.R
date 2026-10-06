library(tidyverse)
library(ovog)
# also requires:
# remotes::install_github("r-lib/fs")

# 'Survey backups' in a sub-folder rel to the script

#"zips" |>
#  fs::dir_ls() |>
#  hv_import_cruise(collapse_station=TRUE) -> d
"zips" |>
  fs::dir_ls() |>
  hv_import() -> d

# adapt to previous setups recognized by my scripts

d$stodvar |>
  select(sid = synis_id, leid = leidangur, skip, dags,
    stod, vf = fishing_gear_no, upphaf = togbyrjun, endir = togendir,
    dypik = togdypi_kastad, dypih = togdypi_hift,
    vir = vir_uti, loopn = lodrett_opnun,
    laopn = larett_opnun, grandl = grandaralengd,
    togtimi, toghradi, lat1 = kastad_n_breidd, lon1 = kastad_v_lengd,
    lat2 = hift_n_breidd, lon2 = hift_v_lengd) |>
  mutate(lon1 = -1*lon1,
    lon2 = -1*lon2,
    lat = ifelse(is.na(lat2), lat1, (lat1 + lat2)/2),
    lon = ifelse(is.na(lon2), lon1, (lon1 + lon2)/2),
    dags = as.Date(dags)) -> stations

d$skraning |>
  filter(maeliadgerd==1) |>
  left_join(d$stodvar,join_by(synis_id,leidangur)) -> le

# engar talningar/vigtanir enn !!
# d$skraning |>
#  filter(maeliadgerd==10) |>
#  left_join(d$stodvar,join_by(synis_id,leidangur)) -> nu

d$skraning |>
  filter(maeliadgerd==3,
    tegund==31) |>
  left_join(d$stodvar,
    join_by(synis_id,leidangur)) |>
  filter(!is.na(lengd)) |>
  select(sid=synis_id,
    leid=leidangur,
    stod,nr,
    l=lengd,
    w=oslaegt,
    s=kyn,
    kt=kynthroski,
    gw=kynfaeri,
    a=s_aldur) -> fishData

"../fromGreenland/fish_data.rds" |>
 read_rds() |>
  select(sid,leid,stod,nr,l,a,w,s,kt,gw) |>
  bind_rows(fishData) -> fishData

"../fromGreenland/pt_stations.rds" |>
  read_rds() |>
  select(sid,leid,dags,stod,vf:last_col()) |>
  bind_rows(stations) -> stations

fishData %>%
  filter(s == 2) %>%
  mutate(roecont = gw/w) %>%
  group_by(leid,stod) %>%
  summarize(roecont = mean(roecont, na.rm=T)) %>%
  mutate(roecont = round(100*roecont,1)) %>%
  select(leid, stod, roecont) -> r

fishData %>% 
  group_by(leid,stod) %>% 
  summarize(n = n(),
            ml = mean(l),
            mw = mean(w,na.rm=T), 
            sd = sd(w,na.rm=T), 
            sem = sd/sqrt(n()),
            ikg = round(1000/mw)) -> s

save(list = c("d", "le", "fishData", "stations","r","s"),
  file = "hafvogarGogn.RData")
