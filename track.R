library(tidyverse)
library(sf)

source("~/github/bionechi/R/read_luf3.R")

#"track/ListUserFile03__F038000_T1_L0.0-1835.4.txt" |>
"track/ListUserFile03__F038000_T1_L0.0-2193.1.txt" |>
  read_luf3() -> tara

tara |>
  select(X=lon,Y=lat) |>
    as.matrix() |>
    st_linestring() -> tara_sf
 
tara |>
  select(lon,lat) |>
  st_as_sf(coords=c("lon","lat")) |>
  st_set_crs(4326) -> tara_sf

# two above yield identical 'tara_sf'-s!

# https://gdal.org/en/latest/drivers/vector/gpx.html#vector-gpx
# FORCE_GPX_TRACK=[YES/NO]: Defaults to NO. 
# By default when writing a layer whose features are of type wkbLineString, 
# the GPX driver chooses to write them as routes. If YES is specified, 
# they will be written as tracks.
# FORCE_GPX_ROUTE=[YES/NO]: Defaults to NO. By default when writing a layer 
# whose features are of type wkbMultiLineString, the GPX driver chooses 
# to write them as tracks. If YES is specified, they will be written as routes, 
# provided that the multilines are composed of only one single line.

#tara_sf$geometry |>

st_geometry(tara_sf) |>
  st_cast(to="POLYGON") |>
  st_cast(to="MULTILINESTRING") |>
  st_write("tara5-2024.gpx",
#    layer_options=c(#"GPX_USE_EXTENSIONS=YES",
#      "FORCE_GPX_TRACK=YES"),
    delete_dsn=TRUE)

tara_sf |>
  st_sfc(crs=4326) |>
  st_write("tara5-2024.gpx",
    layer_options="FORCE_GPX_TRACK=YES",
    delete_dsn=TRUE)
