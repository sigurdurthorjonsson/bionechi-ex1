# bionechi-example
Example of BIOass and Numbers from ECHo Integration of acoustic survey results
2025-10-06

Prior to committing the whole estimation to gitlab:

Script 'fromGreenland/info-til-island.R' was run in order to make Greenland/GINR biology compatible with Iceland/MFRI
(and fix errors and make some adhoc decision regarding maturity).

Second, run 'hafog/arni-ovog-tarajoq-munge-gagnasokn.R' using Einar's 'ovog' to read from 
the survey (A12-2025) hafvog-zips. Decided against collecting from DB, since we have to 
join with Greenland anyway.

Third, run 'arni_tarajoq.R' reading the necessary settings and data, notably 'r2area.txt'
and 'st2area.txt' and 'gpx/peri.gpx' in additon to the biology discusse above, and naturally,
the contents of folder 'nasc', different nasc with final and alternative interpretations. Note,
this time around, transducer malfunction necessiated saving two report files for Árni.

Fourth, run 'pointEst.R' in order get the point estimate for the survey.

Fifth, run 'areaQCV/boot_rects_and_stations.R' to do the bootstrap uncertainty estimation, 
with 'nRep' number of replicates, 1e5 being the accepted norm for IEGJM capelin.

R-packages that need to be installed for whole analysis:

* tidyverse: 
* geo: MFRI/Hafro old school geographical maps
  - depends on 'maps' and 'mapdata'
* ovog: Our colleague Einar's package enabling data extraction from sampling system zip-files
* bionechi: my utilities for the estimation, various different functions


pak::pak("Hafro/geo")
pak::pak("einarhjorleifsson/ovog")
pak::pak("sigurdurthorjonsson/bionechi")

