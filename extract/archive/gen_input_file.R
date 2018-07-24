.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())

library(feather)
library(dplyr)

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

indi_fam <- 'water'
if (indi_fam == 'water') {
  ptdat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2018_03_08_clean.feather')
  ptdat <- filter(ptdat, !(is.na(lat)))
  
  w_piped_cr <- ptdat
  w_piped_cr <- dplyr::select(w_piped_cr, -unimp, -surface)
  w_piped_cr <- mutate(w_piped_cr, point = 1, weight = 1, w_piped_cr = (piped*N), N = (imp*N))
  w_piped_cr <- rename(w_piped_cr, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_piped_cr <- mutate(w_piped_cr, prop = w_piped_cr/N) %>% 
                select(-imp, -piped) %>%
                filter(N > 0)
  write.csv(w_piped_cr, file = '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped_cr.csv')

  w_imp <- ptdat
  w_imp <- select(w_imp, -surface, -piped, -unimp)
  w_imp <- mutate(w_imp, point = 1, weight = 1, w_imp = (imp*N))
  w_imp <- rename(w_imp, country = iso3, year = year_start, prop = imp, N = N, latitude = lat,
                  longitude = long)
  w_imp <- mutate(w_imp, N = (N))
  write.csv(w_imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv')

  w_unimp_cr <- ptdat
  w_unimp_cr <- select(w_unimp_cr, -surface, -piped)
  w_unimp_cr <- mutate(w_unimp_cr, point = 1, weight = 1, w_unimp_cr = (unimp*N), w_imp = (imp*N))
  w_unimp_cr <- rename(w_unimp_cr, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_unimp_cr <- mutate(w_unimp_cr, N = ((N)) - (w_imp), prop = w_unimp_cr/N) %>%
                select(-imp, -w_imp, -unimp) %>%
                filter(N > 0)
  write.csv(w_unimp_cr, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv')

}

rm(list = ls())
indi_fam <- 'sani'
if (indi_fam == 'sani') {
  ptdat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2018_03_08_clean.feather')
  ptdat <- filter(ptdat, !(is.na(lat)))

  s_imp <- ptdat
  s_imp <- select(s_imp, -od, -unimp)
  s_imp <- mutate(s_imp, point = 1, weight = 1, s_imp = (imp*N))
  s_imp <- rename(s_imp, country = iso3, year = year_start, prop = imp, N = N, latitude = lat,
                  longitude = long)
  s_imp <- mutate(s_imp, N = (N))
  write.csv(s_imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')

  s_unimp_cr <- ptdat
  s_unimp_cr <- select(s_unimp_cr, -od)
  s_unimp_cr <- mutate(s_unimp_cr, point = 1, weight = 1, s_unimp_cr = (unimp*N), s_imp = (imp*N))
  s_unimp_cr <- rename(s_unimp_cr, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  s_unimp_cr <- mutate(s_unimp_cr, N = ((N) - s_imp), prop = s_unimp_cr/N) %>% 
                  select(-s_imp, -imp, -unimp) %>% 
                  filter(N > 0)
  write.csv(s_unimp_cr, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv')

}
rm(list = ls())