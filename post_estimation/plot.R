# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 09/05/2018
# Purpose: Run custom function to create U5M maps
# source("/homes/jfrostad/_code/lbd/u5m_mapping/plot.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/"
  h_root <- "/homes/jfrostad/"
  arg <- commandArgs()[-(1:3)] # First args are for unix use only
  
  if (length(arg)==0) {
    # arg <- c("IND", #current project iteration
    #          "8", #output version
    #          1) #number of cores provided to multicore functions
  }
  

  # necessary to set this option in order to read in a non-english character shapefile on a linux system (cluster)
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
} else {
  j_root <- "J:"
  h_root <- "H:"
  # arg <- c("IND", #current project iteration
  #          "4", #output version
  #          1) #number of cores provided to multicore functions
}

#use cairo to render instead of quartz (quartz causes big slowdowns with geom_sf)
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

## Set core_repo location and indicator group
user            <- Sys.info()['user']
core_repo       <- '/homes/jfrostad/_code/lbd/hap/'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')

#load packages
package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
## Load libraries and  MBG project functions.
.libPaths(package_lib)
pacman::p_load(data.table, scales, ggplot2, RColorBrewer, sf, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_02_07_23_37_07'
indicator_group <- 'cooking'
indicator <- 'hap'
type <- 'mean'
raked <- F
start_year <- 2000
end_year <- 2017
cores <- 10
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/ihme/geospatial/mbg/cooking/pafs', run_date)
# u5m.paths <- data.table(raster=file.path(data.dir, 'died_under5_mean_raked_2000_2017.tif'),
#                         admin1=file.path(data.dir, 'died_under5_mean_raked_ad1.csv'),
#                         admin2=file.path(data.dir, 'died_under5_mean_raked_ad2.csv'))
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))

###Output###
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#map functions#
hap.function.dir <- file.path(core_repo, 'post_estimation/_lib')
#this pulls hap collapse helper functions
file.path(hap.function.dir, '/map_fx.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
#read in the proper annotations (borders, lakes, mask)
annotations <- load_map_annotations()

#combine and save all ad2 level results
dt <-
list.files(data.dir, pattern='ad2_tap_results', full.names = T) %>% 
  lapply(., fread) %>% 
  rbindlist(use.names=T, fill=T) %T>% 
  write.csv(., file.path(data.dir, 'admin_2_summary.csv'), row.names = F)

#merge sr region names/IDs
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns
dt <- merge(dt, locs, by='iso3', all.x=T)

#do some postestimation

#calculate GINI/MAD at country level
dt_ineq <- dt[year %in% c(2000, 2017), .(iso3, year, ADM0_CODE, ADM2_CODE, dfu, super_region_id, super_region_name, region_id, region_name)]
dt_ineq[, gini := gini(dfu), by=.(ADM0_CODE, year)]
dt_ineq[, mad := mad(dfu, center = mean(dfu)), by=.(ADM0_CODE, year)]
dt_ineq[, mean := mean(dfu, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, max := max(dfu, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, min := min(dfu, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, range := max-min]
dt_ineq <- unique(dt_ineq[year==2017], by=c('ADM0_CODE', 'year'))
dt_ineq <- dt_ineq[dt_ineq[,do.call(order, .SD), .SDcols = c('super_region_id', 'mean')]]
dt_ineq[, country := factor(iso3, levels=unique(iso3))]

#plot absolute inequality
ggplot(dt_ineq[year==2017], aes(x=country, y=mean, ymax=max, ymin=min, color=super_region_name)) +
  geom_errorbar() +
  geom_point(stat='identity') + 
  scale_color_brewer(palette='Dark2') +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90)) 
ggsave(filename=file.path(out.dir, 'dfu_inequality_2017.png'),
       width=10, height=6, units='in', dpi=600)

#GINI results
summary(dt_ineq$gini)
dt_ineq[year==2000 & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[year==2017 & gini>mean(gini, na.rm=T), uniqueN(iso3)]
dt_ineq[gini>mean(gini, na.rm=T), table(iso3, year)]

#TODO move this to be at pixel level
#calculate rates of change
these_cols <- c('hap_pct', 'dfu', 'tap_paf')
d_cols <- paste0(these_cols, '_d')
dr_cols <- paste0(these_cols, '_dr')
dt_d <- setkey(dt, ADM0_CODE, ADM2_CODE) %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[, (d_cols) := .SD-data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %>% 
  .[, (dr_cols) := (.SD-data.table::shift(.SD,n=1))/data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] 

write.csv(dt_d, file.path(data.dir, 'admin_2_delta_summary.csv'), row.names = F)

#plot change in HAP share
ggplot(dt_d[year==2017], aes(x=(hap_pct-.5), y=hap_pct_d, color=super_region_name, alpha=log(tap_pc))) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  #scale_size_area('TAP dose', max_size=3) +
  scale_color_brewer(palette='Dark2') +
  xlim(c(-.6, .6)) +
  ylim(c(-.6, .3)) +
  theme_bw() 
ggsave(filename=file.path(out.dir, 'hap_share_change.png'),
       width=15, height=10, units='in', dpi=900)

#facet plot change in HAP share
ggplot(dt[year %in% c(2000,2017)], aes(x=(hap_pct-.5), y=tap_pc, color=super_region_name)) + 
  geom_point() + 
  #geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~year) +
  #scale_size_area('TAP dose', max_size=3) +
  scale_color_brewer(palette='Dark2') +
  xlim(c(-.6, .6)) +
  #ylim(c(-.6, .3))  +
  coord_flip() +
  theme_bw() 
ggsave(filename=file.path(out.dir, 'hap_share_change_facet.png'),
       width=15, height=10, units='in', dpi=900)

#testplots for nature pre-sub
ggplot(dt[year %in% c(2000,2017) & iso3 %in% c('KEN', 'AFG', 'NGA')], aes(x=(hap_pct-.5), y=tap_pc, color=region_name)) + 
  geom_point() + 
  geom_vline(xintercept=0) +
  facet_wrap(~year) +
  #scale_size_area('TAP dose', max_size=3) +
  scale_color_brewer(palette='Dark2') +
  xlim(c(-.6, .6)) +
  #ylim(c(-.6, .3))  +
  coord_flip() +
  theme_bw() 

ggplot(dt[year %in% c(2000,2017) & iso3 %in% c('KEN', 'ZAF', 'NGA')], 
       aes(x=hap_pct, y=tap_paf, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
  geom_point() + 
  geom_line(alpha=.1) +
  geom_vline(xintercept=.5) +
  #scale_size_area('TAP dose', max_size=3) +
  scale_color_brewer(palette='Dark2') +
  scale_shape_manual(values=c(1, 16)) +
  xlim(c(0, 1)) +
  #ylim(c(0, .1))  +
  coord_flip() +
  theme_bw() 


#read in input data and prepare it for mapping
tic('total')
tic('loading data')
data_2017 <- list(
  'hap_pct'=
  load_map_results(indicator, indicator_group, run_date, type='hap_pct', raked, start_year, end_year, single_year=2017,
                         custom_path = hap.paths,
                         use.sf = T,
                         geo_levels=c('admin2'),
                         cores=cores),
  'tap_pc'=
    load_map_results(indicator, indicator_group, run_date, type='tap_pc', raked, start_year, end_year, single_year=2017,
                          custom_path = hap.paths,
                          use.sf = T,
                          geo_levels=c('admin2'),
                          cores=cores),
  'dfu'=
    load_map_results(indicator, indicator_group, run_date, type='dfu', raked, start_year, end_year, single_year=2017,
                       custom_path = hap.paths,
                       use.sf = T,
                       geo_levels=c('admin2'),
                       cores=cores),
  'tap_paf'=
    load_map_results(indicator, indicator_group, run_date, type='tap_paf', raked, start_year, end_year, single_year=2017,
                           custom_path = hap.paths,
                           use.sf = T,
                           geo_levels=c('admin2'),
                           cores=cores)
)
data_2000 <- list(
  'hap_pct'=
    load_map_results(indicator, indicator_group, run_date, type='hap_pct', raked, start_year, end_year, single_year=2000,
                     custom_path = hap.paths,
                     use.sf = T,
                     geo_levels=c('admin2'),
                     cores=cores),
  'tap_pc'=
    load_map_results(indicator, indicator_group, run_date, type='tap_pc', raked, start_year, end_year, single_year=2000,
                            custom_path = hap.paths,
                            use.sf = T,
                            geo_levels=c('admin2'),
                            cores=cores),
  'dfu'=
    load_map_results(indicator, indicator_group, run_date, type='dfu', raked, start_year, end_year, single_year=2000,
                         custom_path = hap.paths,
                         use.sf = T,
                         geo_levels=c('admin2'),
                         cores=cores),
  'tap_paf'=
    load_map_results(indicator, indicator_group, run_date, type='tap_paf', raked, start_year, end_year, single_year=2000,
                             custom_path = hap.paths,
                             use.sf = T,
                             geo_levels=c('admin2'),
                             cores=cores)
)
data_d <- list(
  'hap_pct_d'=
    load_map_results(indicator, indicator_group, run_date, type='hap_pct_d', raked, start_year, end_year, single_year=2017,
                     custom_path = hap.paths.d,
                     use.sf = T,
                     geo_levels=c('admin2'),
                     cores=cores),
  'dfu_d'=
    load_map_results(indicator, indicator_group, run_date, type='dfu_d', raked, start_year, end_year, single_year=2017,
                         custom_path = hap.paths.d,
                         use.sf = T,
                         geo_levels=c('admin2'),
                         cores=cores),
  'tap_paf_d'=
    load_map_results(indicator, indicator_group, run_date, type='tap_paf_d', raked, start_year, end_year, single_year=2017,
                             custom_path = hap.paths.d,
                             use.sf = T,
                             geo_levels=c('admin2'),
                             cores=cores),
  'hap_pct_dr'=
    load_map_results(indicator, indicator_group, run_date, type='hap_pct_dr', raked, start_year, end_year, single_year=2017,
                     custom_path = hap.paths.d,
                     use.sf = T,
                     geo_levels=c('admin2'),
                     cores=cores),
  'dfu_dr'=
    load_map_results(indicator, indicator_group, run_date, type='dfu_dr', raked, start_year, end_year, single_year=2017,
                     custom_path = hap.paths.d,
                     use.sf = T,
                     geo_levels=c('admin2'),
                     cores=cores),
  'tap_paf_dr'=
    load_map_results(indicator, indicator_group, run_date, type='tap_paf_dr', raked, start_year, end_year, single_year=2017,
                    custom_path = hap.paths.d,
                    use.sf = T,
                    geo_levels=c('admin2'),
                    cores=cores)
)
toc()

#define extent of map
zoom.afr <- data.table(x1=-10, x2=50, y1=-20, y2=40)
zoom.global <- data.table(x1=-120, x2=150, y1=-40, y2=55)
#***********************************************************************************************************************

# ---CREATE MAPS--------------------------------------------------------------------------------------------------------
#render and save maps
colors <- magma(10, direction=-1)

#general scale
color_values <- c(seq(0, .3, length.out = 3), seq(.3, .8, length.out = 5), seq(.8, 1, length.out = 4)) %>%
  unique %>%
  rescale

#paf scale
paf_values <- c(seq(0, .2, length.out = 2), seq(.2, .5, length.out = 8), seq(.5, .75, length.out = 2)) %>%
  unique %>%
  rescale

#change scale
d_colors <- RColorBrewer::brewer.pal(11, 'BrBG') %>% rev %>% .[-1]
d_values <- c(seq(-.2, -.1, length.out = 2), seq(-.1, .1, length.out = 8), seq(.1, .2, length.out = 2)) %>%
  unique %>%
  rescale
dr_values <- c(seq(-1, -.5, length.out = 2), seq(-.5, .5, length.out = 8), seq(.5, 1, length.out = 2)) %>%
  unique %>%
  rescale

tic('plotting 2017')
gg_2017 <- list(
  'hap_pct'=plot_map(data_2017$hap_pct$admin2, annotations, limits=c(0, 1), title='2017', 
               legend_colors=colors, legend_color_values=color_values,
               legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
               legend_title='hap/tap', custom_scale=T,
               pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
               zoom=zoom.global,
               debug=F),
  # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu'=plot_map(data_2017$dfu$admin2, annotations, limits=c(0, 1), title='2017', 
           legend_colors=colors, legend_color_values=color_values,
           legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
           legend_title='dfu %', custom_scale=T,
           pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
           zoom=zoom.global,
           debug=F),
  'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, .75), title='2017', 
                     legend_colors=colors, legend_color_values=paf_values,
                     legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
                     legend_title='tap PAF', custom_scale=T,
                     pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                     zoom=zoom.global,
                     debug=F)
)
toc()

tic('plotting 2000')
gg_2000 <- list(
  'hap_pct'=plot_map(data_2000$hap_pct$admin2, annotations, limits=c(0, 1), title='2000', 
                     legend_colors=colors, legend_color_values=color_values,
                     legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
                     legend_title='hap/tap', custom_scale=T,
                     pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                     zoom=zoom.global,
                     debug=F),
  # 'tap_pc'=plot_map(data_2000$tap_pc$admin2, annotations, limits=c(0, 1), title='2000', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu'=plot_map(data_2000$dfu$admin2, annotations, limits=c(0, 1), title='2000', 
                 legend_colors=colors, legend_color_values=color_values,
                 legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
                 legend_title='dfu %', custom_scale=T,
                 pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                 zoom=zoom.global,
                 debug=F),
  'tap_paf'=plot_map(data_2000$tap_paf$admin2, annotations, limits=c(0, .75), title='2000', 
                     legend_colors=colors, legend_color_values=paf_values,
                     legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
                     legend_title='tap PAF', custom_scale=T,
                     pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                     zoom=zoom.global,
                     debug=F)
)
toc()

tic('plotting 2000-2017 change')
gg_d <- list(
  'hap_pct_d'=plot_map(data_d$hap_pct_d$admin2, annotations, limits=c(-.2, .2), title='change from 2000-2017', 
                     legend_colors=d_colors, legend_color_values=d_values,
                     legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
                     legend_title='hap/tap', custom_scale=T,
                     pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                     zoom=zoom.global,
                     debug=F),
  # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu_d'=plot_map(data_d$dfu_d$admin2, annotations, limits=c(-.2, .2), title='change from 2000-2017', 
                 legend_colors=d_colors, legend_color_values=d_values,
                 legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
                 legend_title='dfu %', custom_scale=T,
                 pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                 zoom=zoom.global,
                 debug=F)
  # 'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, 1), title='change from 2000-2017', 
  #                    legend_colors=colors, legend_color_values=paf_values,
  #                    legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #                    legend_title='tap PAF', custom_scale=T,
  #                    pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
  #                    zoom=zoom.global,
  #                    debug=F)
)
toc()

tic('plotting 2000-2017 change rate')
gg_dr <- list(
  'hap_pct_dr'=plot_map(data_d$hap_pct_dr$admin2, annotations, limits=c(-1, 1), title='change rate from 2000-2017', 
                       legend_colors=d_colors, legend_color_values=dr_values,
                       legend_breaks=seq(-1, 1, .2), legend_labels=seq(-1, 1, .2),
                       legend_title='hap/tap', custom_scale=T,
                       pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                       zoom=zoom.global,
                       debug=F),
  # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu_dr'=plot_map(data_d$dfu_dr$admin2, annotations, limits=c(-1, 1), title='change rate from 2000-2017', 
                   legend_colors=d_colors, legend_color_values=dr_values,
                   legend_breaks=seq(-1, 1, .2), legend_labels=seq(-1, 1, .2),
                   legend_title='dfu %', custom_scale=T,
                   pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
                   zoom=zoom.global,
                   debug=F)
  # 'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, 1), title='change from 2000-2017', 
  #                    legend_colors=colors, legend_color_values=paf_values,
  #                    legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #                    legend_title='tap PAF', custom_scale=T,
  #                    pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
  #                    zoom=zoom.global,
  #                    debug=F)
)
toc()

tic('ggsaving 2017')
ggsave(filename=file.path(out.dir, 'hap_pct_2017.png'), plot=gg_2017$hap_pct, 
       width=10, height=6, units='in', dpi=300)

ggsave(filename=file.path(out.dir, 'dfu_2017.png'), plot=gg_2017$dfu, 
       width=10, height=6, units='in', dpi=300)

ggsave(filename=file.path(out.dir, 'tap_paf_2017.png'), plot=gg_2017$tap_paf, 
       width=10, height=6, units='in', dpi=300)
toc()

tic('ggsaving 2000')
ggsave(filename=file.path(out.dir, 'hap_pct_2000.png'), plot=gg_2000$hap_pct, 
       width=10, height=6, units='in', dpi=600)

ggsave(filename=file.path(out.dir, 'dfu_2000.png'), plot=gg_2000$dfu, 
       width=10, height=6, units='in', dpi=600)

ggsave(filename=file.path(out.dir, 'tap_paf_2000.png'), plot=gg_2000$tap_paf, 
       width=10, height=6, units='in', dpi=600)
toc()

tic('ggsaving change')
ggsave(filename=file.path(out.dir, 'hap_pct_d.png'), plot=gg_d$hap_pct_d, 
       width=10, height=6, units='in', dpi=600)

ggsave(filename=file.path(out.dir, 'dfu_d.png'), plot=gg_d$dfu_d, 
       width=10, height=6, units='in', dpi=600)

# ggsave(filename=file.path(out.dir, 'tap_paf_d.png'), plot=gg_2000$tap_paf, 
#        width=10, height=6, units='in', dpi=600)
toc()

tic('ggsaving change rate')
ggsave(filename=file.path(out.dir, 'hap_pct_dr.png'), plot=gg_d$hap_pct_dr, 
       width=10, height=6, units='in', dpi=600)

ggsave(filename=file.path(out.dir, 'dfu_dr.png'), plot=gg_d$dfu_dr, 
       width=10, height=6, units='in', dpi=600)

# ggsave(filename=file.path(out.dir, 'tap_paf_d.png'), plot=gg_2000$tap_paf, 
#        width=10, height=6, units='in', dpi=600)
toc()

# tic('bmp saving')
# png(filename=file.path(out.dir, 'testreg_nosf.bmp'), 
#     units='px', width=2700, height=1500)
# print(gg)
# dev.off()
# toc()

toc()
#***********************************************************************************************************************

# ---SCRAPS-------------------------------------------------------------------------------------------------------------
#testing
tmp <- data_d$dfu_d$admin2 %>% copy
plot_map(tmp[tmp$NAME_0 %like% 'Kenya',], annotations, limits=c(-.2, .2), title='change from 2000-2017', 
         legend_colors=d_colors, legend_color_values=d_values,
         legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
         legend_title='dfu %', custom_scale=T,
         pop.mask=F, lake.mask=T, stage3.mask=T, borders=T,
         zoom=zoom.global,
         debug=F)
