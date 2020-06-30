# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 04/08/2020
# Purpose: Produce HAP maps and figures
# source("/homes/jfrostad/_code/lbd/hap/cooking/rover/spirit.R", echo=T)
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
my_repo       <- '/homes/jfrostad/_code/lbd/hap/'
core_repo       <- '/homes/jfrostad/_code/lbd/hap/'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')

#load packages
package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
## Load libraries and  MBG project functions.
.libPaths(package_lib)
pacman::p_load(data.table, scales, ggplot2, ggpubr, ggridges, ggrepel, gridExtra, isoband, RColorBrewer, 
               sf, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
  mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_05_17_11_40_28'

indicator_group <- 'cooking'
indicator <- 'hap'
type <- 'mean'
raked <- T
start_year <- 2000
end_year <- 2017
cores <- 10
modeling_shapefile_version <- "2019_09_10"
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
data.dir <- file.path('/ihme/geospatial/mbg/cooking/post', run_date)
global_link_dir <- file.path('/home/j/WORK/11_geospatial/admin_shapefiles', modeling_shapefile_version) #TODO make official
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary_children.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))

###Output###
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#PE functions#
file.path(my_repo, '_lib', 'post', 'map_fx.R') %>% source
file.path(my_repo, '_lib', 'post', 'plot_fx.R') %>% source

#gbd fx
gbd.shared.function.dir <- '/ihme/cc_resources/libraries/current/r/'
file.path(gbd.shared.function.dir, 'get_location_metadata.R') %>% source

#helper fx to pull/prep the appropriate files from our list of SDG projection objects
prepCasts <- function(id, type, list=sdg_files, id_dt=NA, id_var=NA) {
  
  #format ID var if necessary
  if(nchar(id)==4) id <- as.character(id) #if the ID is a year, format as character
  
  #helper function to extract the correct object
  extractObj <- ifelse(type!='aroc',
                       function(x) list[[x]][[type]][[id]] %>% as.data.table,
                       function(x) list[[x]][[type]] %>% as.data.table ) #aroc only has one object
  
  #do the formatting and extractions
  lapply(1:length(list), extractObj) %>% 
    rbindlist %>% 
    { if(id_var %>% is.na) cbind(., id_dt[id,]) else .[, (id_var) := id] } %>% 
    return
  
}


#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
#read in the proper annotations (borders, lakes, mask)
#read in the proper annotations (borders, lakes, mask)
annotations_path <- file.path(out.dir, 'annotations.RDs')
check <- file.exists(annotations_path)
annotations <- ifelse(
  check,
  readRDS(annotations_path),
  load_map_annotations()
)
if(!check) saveRDS(annotations, file=annotations_path)

#read in link_table
global_link_table <- file.path(global_link_dir, "lbd_full_link.rds") %>% readRDS %>% as.data.table
adm_links <- global_link_table[, .(ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE)] %>% unique

#read in shps
stage1 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage1_ad2_gadm.shp')
stage2 <- st_read('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/shps_by_stage/stage2_ad2_gadm.shp')
adm2 <- rbind(stage1, stage2)

#read in results for lri/children
dt <- file.path(data.dir, 'admin_2_summary.csv') %>% fread

#merge sr region names/IDs
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns

#create file to crosswalk AD0 to iso3
iso3_map <- dplyr::select(adm2, iso3, ADM0_CODE=gadm_geoid) 
iso3_map$geometry <- NULL
iso3_map <- as.data.table(iso3_map) %>% unique
locs <- merge(locs, iso3_map, by='iso3')

#merge sr region names/IDs
dt <- merge(dt, locs, by='ADM0_CODE', all.x=T)
dt <- merge(dt, adm_links, by=c('ADM0_CODE', 'ADM2_CODE'))

#for some reason its missing TTO
#TODO update centrally
dt[ADM0_CODE==225, `:=` (iso3='TTO',
                         location_name='Trinidad and Tobago',
                         super_region_id=103,
                         super_region_name='Latin America and Caribbean',
                         region_id=120,
                         region_name='Andean Latin America')]

#read in results for lri/children
dt_d <- file.path(data.dir, 'admin_2_delta_summary.csv') %>% fread

#setup the list of top countries
#defined by population in 2018
biggest_countries <- 
  dt[year==max(dt$year), .(sum=sum(pop_total, na.rm=T)), by=.(iso3)] %>%
  .[order(sum)] %>%
  tail(10) %>%
  .[, unique(iso3)]

#defined based on LRI rates/counts
top_countries <- 
  dt[year==min(dt$year), .(mean=weighted.mean(lri_mean, w=pop, na.rm=T)), by=.(iso3)] %>%
  .[order(mean)] %>%
  tail(10) %>%
  .[, unique(iso3)]

top_countries_c <-
  dt[year==min(dt$year), .(sum=sum(lri_c_mean, na.rm=T)), by=.(iso3)] %>%
  .[order(sum)] %>%
  tail(14) %>%
  .[, unique(iso3)]

#top country per GBD region
top_countries_gbdreg <- #defined based on LRI counts
  dt[year==min(dt$year), .(sum=sum(lri_c_mean, na.rm=T)), by=.(iso3,region_name)] %>%
  .[, .SD[which.max(sum)], by=region_name]

#top country per MBG region
regs <- load_adm0_lookup_table() %>% .[,.(mbg_reg_name=reg_name, mbg_reg, iso3=toupper(iso3))] %>% unique
dt <- merge(dt, regs, by='iso3')
top_countries_mbgreg <- #defined based on LRI counts
  dt[year==min(dt$year), .(sum=sum(lri_c_mean, na.rm=T)), by=.(iso3,mbg_reg)] %>%
  .[, .SD[which.max(sum)], by=mbg_reg]

second_countries_reg <- #defined based on LRI counts
  dt[year==min(dt$year), .(sum=sum(lri_c_mean, na.rm=T)), by=.(iso3,mbg_reg_name)] %>% 
  .[!(iso3 %in% unique(top_countries_gbdreg$iso3))] %>% 
  .[, .SD[which.max(sum)], by=mbg_reg_name]

#read in input data and prepare it for mapping
data <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=c(2000:2018),
                   custom_path = hap.paths,
                   geo_levels=c('admin2'),
                   cores=cores)
data_d <-
  load_map_results(indicator, indicator_group, run_date, raked, 
                   year_list=2018,
                   custom_path = hap.paths.d,
                   geo_levels=c('admin2'),
                   cores=cores)

#define extent of map
zoom.afr <- data.table(x1=-10, x2=50, y1=-20, y2=40)
zoom.global <- data.table(x1=-120, x2=150, y1=-40, y2=55)
#***********************************************************************************************************************

# ---FIGURE 1-----------------------------------------------------------------------------------------------------------
#create region colors
reg_colors <- c('4'='#1f78b4',
                '31'='#fb9a99',
                '103'='#ff7f00',
                '137'='#984ea3',
                '158'='#e31a1c',
                '166'='#4daf4a')

#create data to make regional map color legend
reg_data <- dt[year==max(year), .(year, ADM0_CODE, ADM1_CODE, sreg=as.factor(super_region_id))] %>% 
  unique %>% 
  load_map_results(year_list=max(.$year),
                   custom_path = list('admin1'=.),
                   geo_levels=c('admin1'),
                   subvars = 'sreg',
                   cores=cores)

zoom <- data.table(x1=-110, x2=140, y1=-40, y2=55)
annotations_reg <- lapply(annotations, st_crop, xmin=zoom$x1, xmax=zoom$x2, ymin=zoom$y1, ymax=zoom$y2)

#make a map showing all the region colors in place
reg_map <- ggplot() + geom_sf(data = annotations_reg$adm0, lwd=0.1, color = 'black', fill = 'gray90')
reg_map <- reg_map + geom_sf(data = reg_data$admin1, aes(fill = sreg), lwd=0) + coord_sf(datum = NA)
reg_map <- reg_map + geom_sf(data = annotations_reg$adm0, lwd=0.1, color = 'black', fill=NA)
reg_map <- reg_map + geom_sf(data = annotations_reg$lakes, lwd=0, color = 'gray70', fill = 'lightblue')
reg_map <- reg_map + scale_fill_manual(values=reg_colors, guide=F)
reg_map <- reg_map +
  labs(x="", y="", title='') +
  theme_classic() +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5), plot.margin=unit(c(0, 0, 0, 0), "in"))
reg_grob <- ggplotGrob(reg_map)

#set up plot data
dt_ineq <- dt %>% 
  copy %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[year==2017, year := 2018] %>% 
  .[cause=='all' & grouping=='child'] 

dt_ineq[, mean := weighted.mean(dfu_mean, w=pop_total, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, max := max(dfu_mean, na.rm=T), by=.(ADM0_CODE, year)]
dt_ineq[, min := min(dfu_mean, na.rm=T), by=.(ADM0_CODE, year)]

#reorder regions
dt_ineq[, super_region := factor(super_region_id, levels=c(166, 158, 4, 31, 103, 137) %>% rev)]

#range results
dt_ineq <- unique(dt_ineq, by=c('ADM0_CODE', 'year'))
dt_ineq_end <- dt_ineq[year==max(year)]
dt_ineq_end <- dt_ineq_end[dt_ineq_end[,do.call(order, .SD), .SDcols = c('super_region', 'mean')]]
dt_ineq_end[, country := factor(iso3, levels=unique(iso3))]
dt_ineq_start <- dt_ineq[year==min(year)]
dt_ineq_start[, country := factor(iso3, levels=dt_ineq_end$country)]
dt_ineq_start[, country := forcats::fct_rev(country)]

#plot absolute inequality
plot <-
ggplot(dt_ineq_end, aes(x=country, y=mean, ymax=max, ymin=min, color=super_region)) +
  geom_crossbar(data=dt_ineq_start, color='gray60', size=2.2, width=0) +
  geom_crossbar(size=2.2, width=0, position=position_nudge(x=.32)) +
  geom_point(color='#000000', alpha=.5, shape=3, size=2.2,position=position_nudge(x=.32)) +
  scale_color_manual(values=reg_colors, guide=F) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        text = element_text(size=12),
        axis.text.x = element_text(size=9, angle = 90, hjust = .5))

plot <- plot + annotation_custom(grob=reg_grob, ymin=0, ymax=.25, xmin=73, xmax=97)

ggsave(filename=file.path(out.dir, 'dfu_range.png'),
       width=12, height=6, units='in', dpi=600)

#plot inequality as dotplot
plot <-
  ggplot(dt_ineq_end, aes(x=country, y=mean, color=super_region, fill=super_region)) +
  geom_segment(data=dt_ineq_start, aes(xend=country, yend=max), color='gray',
               arrow = arrow(length = unit(0.1, "inches"), type='closed')) +
  geom_segment(data=dt_ineq_start, aes(xend=country, yend=min), color='gray', 
               arrow = arrow(length = unit(0.1, "inches"), type='closed')) +
  geom_segment(aes(xend=country, yend=max), arrow.fill ='black',
               arrow = arrow(length = unit(0.1, "inches"), type='closed')) +
  geom_segment(aes(xend=country, yend=min), arrow.fill='black', 
               arrow = arrow(length = unit(0.1, "inches"), type='closed')) +
  geom_point(alpha=1, size=2.2, shape=21) + 
  geom_point(data=dt_ineq_start, alpha=1, size=2.2, color='gray') + 
  scale_color_manual(values=reg_colors, guide=F) +
  scale_fill_manual(values=reg_colors, guide=F) +
  scale_y_continuous('Dirty Fuel Use', labels = scales::percent) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=12),
        axis.text.x = element_text(size=9, angle = 90, hjust = .5)) 

plot <- plot + annotation_custom(grob=reg_grob, ymin=0, ymax=.25, xmin=2, xmax=20)

ggsave(filename=file.path(out.dir, 'dfu_range_lolli.png'),
       width=12, height=6, units='in', dpi=600)

#Figure 5: Inequality over time
#AID results
#note that AID = gini * 2 * mean
aid.dt <- dt[cause=='all' & grouping=='child', 
             .(iso3, year, ADM0_CODE, ADM0_NAME, ADM2_CODE, ADM2_NAME, dfu_mean, 
               super_region_id, super_region_name, region_id, region_name, pop_total)] %>% 
  na.omit(cols='dfu_mean') %>% 
  .[, mean := weighted.mean(dfu_mean, weights = pop_total, na.rm=T), by=.(ADM0_NAME, year)] %>% 
  .[, .(mean,
        aid=gini(dfu_mean, weights=pop_total) * 2 * mean,
        pop=sum(pop_total, na.rm=T)), 
    by=.(super_region_id, region_name, ADM0_NAME, year)] %>% 
  unique(by=c('ADM0_NAME', 'year')) %>% 
  .[order(aid),] %>% 
  .[year==min(year), aid_start := aid] %>% 
  .[, aid_start := min(aid_start, na.rm=T), by=.(ADM0_NAME)] %>%
  .[, aid_d := (aid-aid_start)] %>% 
  .[, aid_dr := aid_d/aid_start] %>% 
  .[year==min(year), dfu_start := mean] %>% 
  .[, dfu_start := min(dfu_start, na.rm=T), by=.(ADM0_NAME)] %>%
  .[, dfu_d := (mean-dfu_start)] %>% 
  .[, dfu_dr := dfu_d/dfu_start] %>% 
  .[, super_region := factor(super_region_id, levels=c(166, 158, 4, 31, 103, 137) %>% rev)] %>% 
  .[, cfu := (1-mean)] %>% 
  .[, pred := loess(aid~cfu) %>% predict(.SD), by=super_region] %>% 
  .[, resid := aid-pred] %>% 
  .[abs(resid)>.1 & year==max(year), resid_lab := ADM0_NAME] %>% 
  .[ADM0_NAME %like% "Honduras" & year==max(year), resid_lab := ADM0_NAME]

plot <-
  ggplot(aid.dt, aes(x=cfu, y=aid, color=super_region_id %>% as.factor, label=resid_lab)) +
  geom_smooth(method='loess', aes(x=cfu, y=aid, color=super_region_id %>% as.factor), se=T, size=2) +
  geom_point(data=aid.dt[year==max(year)], aes(x=cfu, y=aid, color=super_region_id %>% as.factor)) +
  geom_text_repel(force=2, color='black') +
  scale_color_manual(values=reg_colors, guide=F) +
  scale_y_continuous('Average interpersonal difference') +
  scale_x_continuous('Clean fuel use (%)', limits=c(0, 1)) +
  theme_minimal()
plot <- plot + annotation_custom(grob=reg_grob, ymin=.5, ymax=.65, xmin=.75, xmax=1)
ggsave(filename=file.path(out.dir, 'aid_vs_cfu.png'), plot=plot, 
       width=12, height=8, units='in', dpi=500)
#***********************************************************************************************************************

# ---FIGURE 2-----------------------------------------------------------------------------------------------------------
#setup plot data
plot.dt <- dt %>% 
  copy %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[year==2017, year := 2018] %>% 
  .[cause=='lri' & grouping=='child'] %>% 
  na.omit(., cols=c('hap_pct_mean', 'tap_paf_mean')) %>% 
  .[iso3=='COD', location_name := 'D.R. Congo'] %>% 
  .[, loc := factor(location_name)] %>% 
  .[, loc := forcats::fct_rev(loc)]

custom_countries <- #defined based on aesthetics - all countries in different range
  c('COD', 'KEN', 'IND', 'MNG', 'THA') 

custom_cols <- c('D.R. Congo'='firebrick4', 'Kenya'='indianred2', 'India'='darkgoldenrod', 
                 'Mongolia'='#ff7f00', 'Thailand'='purple4')

master_plot <- 
  ggplot(plot.dt[iso3 %in% custom_countries], 
         aes(x=1-hap_pct_mean, y=tap_paf_mean, color=loc, shape=year %>% as.factor, group=ADM2_CODE)) + 
  annotate("rect", xmin = -.02, xmax = .5, ymin = .05, ymax = .61, fill='steelblue', alpha = .2) +
  annotate("rect", xmin = .5, xmax = 1.02, ymin = .05, ymax = .61, fill='tomato', alpha = .2) +
  annotate("text", x = .32, y = .60, label = "majority household sources") +
  annotate("text", x = .66, y = .60, label = "majority ambient sources") +
  annotate(
    geom = "segment",
    x = .81, 
    xend = .85, 
    y = .60,
    yend = .60,
    colour = "black",
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  annotate(
    geom = "segment",
    x = .16, 
    xend = 0.12,
    y = .60, 
    yend = .60,
    colour = "black",
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  geom_line(alpha=.1) +
  geom_point(size=2) + 
  geom_point(data=plot.dt[iso3 %in% custom_countries & year==2018], size=2) + #overdraw the later years
  geom_vline(xintercept=.5, linetype="dashed") +
  scale_colour_manual('Country', values=custom_cols) +
  scale_shape_manual('Year', values=c(1, 16), guide=F) +
  scale_x_continuous('', limits=c(-.02, 1.02), labels = scales::percent, breaks=c(.25, .5, .75), expand = c(0, 0)) +
  scale_y_continuous("Percent of LRI attributable to TAP (PAF)", 
                     limits=c(0.05,.61), labels = scales::percent, breaks=c(.2, .4, .6), expand = c(0, 0)) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(.05, .25),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    plot.margin = margin(12, 0, 0, 6, "pt"),
    axis.text.y = element_text(angle = 90, hjust=.4),
    axis.title.x = element_blank()
  )

##cloud version with top 14##
#now make a single inset plot for each of the remaining top by region (and add Pakistan to round it out)
inset_countries <- c(top_countries_gbdreg[, iso3]) %>% 
  .[!(. %in% custom_countries)]

inset_countries[inset_countries %like% 'ETH'] <- 'COD' #duplicate one to help guide the reader
  
insets <- sort(inset_countries) %>% 
  lapply(1:length(.), makeInset, loclist=., 
         scale_labels=T, type='cloud_contour')

#arrange into master figure
all_grobs <- copy(insets)
all_grobs[[13]] <- master_plot
lay <- rbind(c(13,13,13,13,1,2,3),
             c(13,13,13,13,4,5,6),
             c(13,13,13,13,7,8,9),
             c(13,13,13,13,10,11,12))
plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
                    # top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
                    #              gp = gpar(fontsize=24)),
                    bottom=textGrob("Percent of TAP contributed by ambient sources", 
                                    gp = gpar(fontsize=17))
) %>% 
  grid.arrange

ggsave(plot=plot, filename=file.path(out.dir, 'fig_5.png'),
       width=12, height=8, units='in', dpi=900)
#***********************************************************************************************************************
 
# ---FIGURE 3-----------------------------------------------------------------------------------------------------------
#SDG projection probabilities
#append the ADM2 files
sdg_files <-
  file.path(data.dir, 'sdg_projections') %>% list.files(pattern='admin_2', full.names = T) %>% 
  lapply(., readRDS)

#extract goal obj to index over
goals <- lapply(1:length(sdg_files), function(x) sdg_files[[x]]$goals) %>% rbindlist %>% unique

#create a dt with all probabilities
probs <- lapply(1:nrow(goals), prepCasts, type='probs', id_dt=goals) %>% 
  rbindlist %>% 
  setnames(c('target_year', 'spatial_idx'), c('year', 'ADM2_CODE')) %>% 
  merge(., adm_links, by='ADM2_CODE')  %>% 
  merge(., locs, by='ADM0_CODE')

#create a dt with all projections
projs <- 
  lapply(c(2018, seq(2020, 2030, 5)), prepCasts, type='proj', id_var='year') %>% 
  rbindlist %>% 
  setnames('spatial_idx', 'ADM2_CODE') %>% 
  melt(measure = patterns("V"), variable.name = "draw", value.name='sev')

#create a dt with aroc and combine
projs <- prepCasts(2018, type='aroc', id_var='year') %>% 
  melt(measure = patterns("V"), variable.name = "draw", value.name='aroc') %>% 
  merge(., projs, by=c('ADM2_CODE', 'year', 'draw'), all.y=T) %>% 
  merge(., adm_links, by='ADM2_CODE') %>% 
  merge(., locs, by='ADM0_CODE')

#add 18 populations for weighting at region level
reg_projs <- merge(projs, 
                   dt[year==max(year) & grouping=='child' & cause=='all', .(pop_total, ADM2_CODE)], 
                   by='ADM2_CODE')
reg_projs <- setkey(reg_projs, draw, year, region_name)[, sev := weighted.mean(sev, w=pop_total, na.rm=T),
                                                      by=key(reg_projs)] %>% 
  unique(., by=key(.)) 

#ridgeplot of probs
plot <-
ggplot(reg_projs[year %in%c(2030)], aes(x = sev, y = fct_reorder(region_name, sev, .fun=mean), 
                                             fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(data=reg_projs[year %in%c(2018)],
                      scale= .85,
                      fill='gray72',
                      alpha=.75,
                      calc_ecdf = T) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  geom_vline(xintercept=.05, linetype='dashed', color='gray10') +
  #facet_wrap(~year) +
  scale_x_continuous('Access to Clean Energy') +
  theme_minimal() +
  labs(x.axis = '') +
  theme(axis.title.y=element_blank(),
        text = element_text(size=16),
        strip.text.x =element_text(size=20))
ggsave(filename=file.path(out.dir, 'regional_sdg_prob_density.png'), plot=plot, 
       width=12, height=6, units='in', dpi=500)

#helper function to generate state level projections for every country
stateProjs <- function(country, dt, pop.dt) {
  
  message('plotting for ', country)
  
  #add 18 populations for weighting at state level
  plot.dt <- merge(dt[iso3==country], pop.dt, by='ADM2_CODE')
  plot.dt <- setkey(plot.dt, draw, year, ADM1_NAME)[, sev := weighted.mean(sev, w=pop_total, na.rm=T), 
                                                        by=key(plot.dt)] %>% 
    unique(., by=key(.))

  #state ridges
  plot <-
    ggplot(plot.dt[year==2030], aes(x = sev, y = fct_reorder(ADM1_NAME, sev, .fun=mean), 
                                                 fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(data=plot.dt[year==2018],
                        scale= .85,
                        fill='gray72',
                        alpha=.75,
                        calc_ecdf = T) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
    scale_fill_viridis_c(name = "Tail probability", direction = -1) +
    geom_vline(xintercept=.01, linetype='dashed', color='gray10') +
    scale_x_continuous('Access to Clean Energy') +
    theme_minimal() +
    labs(title=country) +
    theme(axis.title.y=element_blank())
    
    print(plot)
    return(NA)

}

pdf(paste0(out.dir, '/sdg_projs_all_countries.pdf'),
    height=8, width=12)

lapply(unique(projs$iso3), stateProjs, dt=projs, 
       pop.dt=dt[year==max(year) & grouping=='child' & cause=='all', .(pop_total, ADM2_CODE)])

dev.off()

#TODO which figure will this be
#create plot of the divide by country
target_threshold <- .05
prob_threshold <- .95

plot.dt <- 
probs[target==target_threshold & year==2030] %>% 
  .[, .(fail=sum(absolute_goal_prob<=(1-prob_threshold), na.rm=T), 
        may_fail=sum(absolute_goal_prob<.5, na.rm=T),
        may_succeed=sum(absolute_goal_prob>.5, na.rm=T),
        success=sum(absolute_goal_prob>=prob_threshold, na.rm=T),
        N=.N), by=.(super_region_id, ADM0_NAME)] %>% 
  .[, ratio := fail/success] %>% 
  .[, plot_var := fail/success] %>% 
  .[, may_fail := may_fail-fail] %>% 
  .[, may_succeed := may_succeed-success] %>% 
  .[fail!=0&success!=0] %>% 
  .[order(ratio)] %>% 
  melt(id.vars=c('super_region_id', 'ADM0_NAME', 'ratio', 'N'), 
       measure.vars=c('success', 'may_succeed', 'may_fail', 'fail')) %>% 
  .[variable%like%'fail', value := value * -1] %>% 
  .[, variable := factor(variable, levels=c('success', 'may_succeed', 'fail', 'may_fail'))]

sqrt_signed <- scales::trans_new("signed_log",
                                 transform=function(x) sign(x)*sqrt(abs(x)),
                                 inverse=function(x) sign(x)*(abs(x))^2)

plot <-
plot.dt %>%
  ggplot(aes(x = forcats::fct_reorder(ADM0_NAME, ratio), 
             y = value, alpha = variable, color = variable, fill=super_region_id %>% as.factor)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=reg_colors, guide=F) +
  scale_color_manual(values=c('success'='navy', 'may_succeed'='gray', 'may_fail'='gray', 'fail'='red'), guide=F) +
  scale_alpha_manual(values=c('success'=.8, 'may_succeed'=.3, 'may_fail'=.3, 'fail'=.8), guide=F) +
  scale_x_discrete('') +
  scale_y_continuous('', trans=sqrt_signed) +
  ggtitle('SDG 7.1: Districts Projected to Fail vs. Districts Projected to Succeed', 
          subtitle = 'Based on 95% confidence') +
  coord_flip() +
  theme_minimal()

plot <- plot + annotation_custom(grob=reg_grob, ymin=50, ymax=90, xmin=25, xmax=32)

ggsave(filename=file.path(out.dir, 'sdg_district_ranges.png'), plot=plot, 
       width=12, height=8, units='in', dpi=500)
#***********************************************************************************************************************

# ---FIGURE 4-----------------------------------------------------------------------------------------------------------
#histogram/density plot of LRI deaths vs tap_pc
#setup plot data
plot.dt <- dt %>% 
  copy %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[year==2017, year := 2018] %>% 
  .[grouping=='child' & cause=='lri'] %>% 
  #.[tap_pc_mean>750, tap_pc_mean := 750] %>%  #cap at 750 ug/m3
  na.omit(., cols=c('dfu_mean', 'pop')) %>% 
  .[, .(iso3, ADM0_NAME, ADM2_CODE, year, pop, pop_total, hap_pct_mean, tap_pc_mean, lri_c_mean, tap_lri_c_mean,
        region_id, super_region_id)]

#generate regular sequence
reg_bins <- c(seq(0, 750, length.out = 399), max(plot.dt$tap_pc_mean, na.rm=T))

#generate logarithmically spaced sequence
log_bins <- c(exp(seq(log(.01), log(750), length.out = 399)), max(plot.dt$tap_pc_mean, na.rm=T))

#generate the plots
pop_plot <-
  makeTapDensityPlot(input_dt=plot.dt, locs=biggest_countries, 
                     wt_var='pop_total', 
                     sequence=log_bins,
                     smoother=.1)
death_plot <-
makeTapDensityPlot(input_dt=plot.dt, locs=biggest_countries, 
                   wt_var='tap_lri_c_mean', 
                   sequence=log_bins,
                   smoother=.1)

#extract legend and set coordinates
legend <- get_legend(pop_plot)
legend$vp$x <- unit(0, 'npc')
legend$vp$y <- unit(.69, 'npc')
#define layout
lay <- rbind(c(1,1,1,1),
             c(2,2,2,2))
plot <- arrangeGrob(grobs=list(pop_plot + theme(legend.position = 'none'), 
                               death_plot + theme(legend.position = 'none')), 
                    layout_matrix=lay, 
                    # top=textGrob("Distribution of population (top) and attributable LRI deaths (bottom), 2018 vs 2000", 
                    #              gp = gpar(fontsize=24)),
                    bottom=textGrob("PM2.5 per capita", 
                                    gp = gpar(fontsize=16))
)

#draw plot and legend/annotations
png(paste0(out.dir, '/fig_4.png'),
    height=8, width=12, units='in', res=1200)
grid.arrange(plot)
grid.draw(legend)
grid.text('a', x = unit(0.01, "npc"), y = unit(0.95, "npc"), gp = gpar(fontsize = 24, fontface = "bold"))
grid.text('b', x = unit(0.01, "npc"), y = unit(0.05, "npc"), gp = gpar(fontsize = 24, fontface = "bold"))
grid.text('2000', x = unit(.95, "npc"), y = unit(0.7, "npc"), gp = gpar(fontsize = 24, fontface = "bold"))
grid.text('2000', x = unit(.95, "npc"), y = unit(0.2, "npc"), gp = gpar(fontsize = 24, fontface = "bold"))
grid.text('2018', x = unit(.95, "npc"), y = unit(0.76, "npc"), gp = gpar(fontsize = 24, fontface = "bold"))
grid.text('2018', x = unit(.95, "npc"), y = unit(0.29, "npc"), gp = gpar(fontsize = 24, fontface = "bold"))
dev.off()

#***********************************************************************************************************************

# ---FIGURE 5----------------------------------------------------------------------------------------------------------
#Figure 5: Inequality over time
dt_ineq <- dt[cause=='all' & grouping=='child', 
              .(iso3, year, ADM0_CODE, ADM2_CODE, ADM2_NAME, dfu_mean, 
                super_region_id, super_region_name, region_id, region_name, pop_total)]
#AID results
#note that AID = gini * 2 * mean
aid.dt <-
  na.omit(dt_ineq, cols='dfu_mean') %>% 
  .[, mean := weighted.mean(dfu_mean, weights = pop_total, na.rm=T), by=.(iso3, year)] %>% 
  .[, .(mean,
        aid=gini(dfu_mean, weights=pop_total) * 2 * mean,
        pop=sum(pop_total, na.rm=T)), 
    by=.(super_region_id, region_name, iso3, year)] %>% 
  unique(by=c('iso3', 'year')) %>% 
  .[order(aid),] %>% 
  .[year==min(year), aid_start := aid] %>% 
  .[, aid_start := min(aid_start, na.rm=T), by=.(iso3)] %>%
  .[, aid_d := (aid-aid_start)] %>% 
  .[, aid_dr := aid_d/aid_start] %>% 
  .[year==min(year), dfu_start := mean] %>% 
  .[, dfu_start := min(dfu_start, na.rm=T), by=.(iso3)] %>%
  .[, dfu_d := (mean-dfu_start)] %>% 
  .[, dfu_dr := dfu_d/dfu_start] %>% 
  .[year==max(year), label := iso3]

plot <-
  ggplot(aid.dt, aes(x=1-mean, y=aid, color=super_region_id %>% as.factor, label=label)) +
  geom_text_repel(force=2, color='black') +
  geom_smooth(method='loess', aes(x=1-mean, y=aid, color=super_region_id %>% as.factor), se=T, size=2) +
  #geom_line(data=aid.dt, aes(x=year, y=aid, color=super_region_id %>% as.factor, group=iso3, alpha=.2)) +
  geom_point(data=aid.dt[year==max(year)], aes(x=1-mean, y=aid, color=super_region_id %>% as.factor)) +
  scale_color_manual(values=reg_colors, guide=F) +
  scale_y_continuous('Average interpersonal difference') +
  scale_x_continuous('Clean fuel use (%)', limits=c(0, 1)) +
  theme_minimal()
ggsave(filename=file.path(out.dir, 'aid_vs_cfu.png'), plot=plot, 
       width=12, height=8, units='in', dpi=500)
plot <-
  ggplot(aid.dt, aes(x=dfu_d*-1, y=aid, color=super_region_id %>% as.factor, label=label)) +
  geom_text_repel(force=2, color='black') +
  geom_smooth(method='loess', aes(x=dfu_d*-1, y=aid, color=super_region_id %>% as.factor), se=T, size=2) +
  #geom_line(data=aid.dt, aes(x=year, y=aid, color=super_region_id %>% as.factor, group=iso3, alpha=.2)) +
  geom_point(data=aid.dt[year==max(year)], aes(x=dfu_d*-1, y=aid, color=super_region_id %>% as.factor)) +
  scale_color_manual(values=reg_colors, guide=F) +
  scale_y_continuous('Average interpersonal difference') +
  scale_x_continuous('Uptake of clean fuel use (% change) from 2000-2018', limits=c(0, .4)) +
  theme_minimal()
ggsave(filename=file.path(out.dir, 'aid_vs_dfu_d.png'), plot=plot, 
       width=12, height=8, units='in', dpi=500)

plot <-
ggplot(aid.dt, aes(x=year, y=aid, color=super_region_id %>% as.factor, label=label)) +
  #geom_text_repel(force=2, color='black') +
  #geom_smooth(method='gam', aes(x=year, y=aid, color=super_region_id %>% as.factor, group=region_name), se=F, size=2) +
  #geom_line(data=aid.dt, aes(x=year, y=aid, color=super_region_id %>% as.factor, group=iso3, alpha=.2)) +
  geom_point(data=aid.dt, aes(x=year, y=aid, color=super_region_id %>% as.factor)) +
  scale_color_manual(values=reg_colors, guide=F) +
  facet_wrap(~region_name) +
  scale_y_continuous('Average interpersonal difference') +
  scale_x_continuous('Year') +
  theme_minimal()
ggsave(filename=file.path(out.dir, 'aid_d_vs_dfu_d.png'), plot=plot, 
       width=12, height=8, units='in', dpi=500)
#***********************************************************************************************************************

# ---CREATE MAPS--------------------------------------------------------------------------------------------------------
#render and save maps
colors <- magma(10, direction=-1)
dr_colors <- cividis(12)

#general scale
color_values <- c(seq(0, .3, length.out = 3), seq(.3, .9, length.out = 6), seq(.9, 1, length.out = 3)) %>%
  unique %>%
  rescale

#pm scale
pm_colors <- RColorBrewer::brewer.pal(9, 'YlOrBr') %>% 
  c('#08519c', '#6baed6', .)
pm_values <- c(seq(5, 15, length.out = 2), seq(15, 100, length.out = 5),
               seq(100, 200, length.out = 4), seq(200, 700, length.out = 3)) %>%
  unique %>%
  rescale

#paf scale
paf_values <- c(seq(0, .2, length.out = 2), seq(.2, .5, length.out = 8), seq(.5, .75, length.out = 2)) %>%
  unique %>%
  rescale

#change scale
d_colors <- RColorBrewer::brewer.pal(11, 'BrBG') %>% rev %>% .[-1]
d_colors <- brewer_pal(palette='BrBG')(11) %>% rev 
d_values <- c(seq(-.3, -.1, length.out = 2), seq(-.1, .1, length.out = 8), seq(.1, .3, length.out = 2)) %>%
  unique %>%
  rescale
dr_values <- c(seq(-.6, -.35, length.out = 2), seq(-.25, .25, length.out = 7), seq(.35, .6, length.out = 2)) %>%
  unique %>%
  rescale

global <-
  plot_map(data$admin2, this_var='hap',
           annotations, limits=c(0, 1), title='DFU% in 2017', 
           legend_color_values=color_values,
           legend_title='DFU %',
           #zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'global_dfu.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data$admin2, this_var='tap_pc',
           annotations, limits=c(5, 700), title='Annual per-capita TAP dose in 2017', 
           legend_colors=pm_colors, legend_color_values=pm_values,
           legend_title='PM2.5 ug/m3',
           #zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'global_tap_pc.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data$admin2, this_var='tap_paf',
           annotations, limits=c(0, 1), title='TAP PAF in 2017', 
           legend_title='TAP PAF',
           #zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'global_tap_paf.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data$admin2, this_var='hap_pct',
           annotations, limits=c(0, 1), title='HAP Share in 2017', 
           legend_title='HAP/TAP',
           #zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'global_hap_pct.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data$admin2, this_var='tap_lri',
           annotations, limits=c(0, 4), title='Mortality Rate of LRI Attributable to TAP in 2017', 
           legend_title='Deaths per 1k children attributable to TAP',
           #zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'global_tap_lri.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='dfu_d',
           annotations, limits=c(-.2, .2), title='DFU% Change from 2000-2018', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='dfu%', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_dfu_d.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='dfu_dr',
           annotations, limits=c(-.2, .2), title='DFU% Change from 2000-2018', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='dfu%', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_dfu_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_paf_d',
           annotations, limits=c(-.2, .2), title='PAF Change from 2000-2018', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='PAF', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_paf_d.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_paf_dr',
           annotations, limits=c(-.2, .2), title='PAF Change from 2000-2018', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='PAF', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_paf_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='hap_pct_d',
           annotations, limits=c(-1, 1), title='HAP Share Change from 2000-2018', 
           # legend_colors=d_colors, legend_color_values=d_values,
           # legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='HAP/TAP', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_hap_pct_d.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='hap_pct_dr',
           annotations, limits=c(-1, 1), title='HAP Share Change from 2000-2018', 
           # legend_colors=d_colors, legend_color_values=d_values,
           # legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='HAP/TAP', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_hap_pct_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_pc_dr',
           annotations, limits=c(-.6, .3), title='Annual Per-Capita TAP Dose, change from 2000-2018', 
           legend_colors=d_colors, legend_color_values=dr_values,
           # legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='% change in PM2.5 ug/m3', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_tap_pc_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_paf_dr',
           annotations, limits=c(-.6, .3), title='PAF of LRI attributable to TAP, change from 2000-2018', 
           legend_colors=d_colors, legend_color_values=dr_values,
           legend_title='% change in PAF', 
           subset=list(var='year', value=2018),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_tap_paf_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

#***********************************************************************************************************************

# ---COUNTRY------------------------------------------------------------------------------------------------------------
india <-
  plot_map(data$admin2, this_var='dfu',
           annotations, limits=c(0, 1), title='DFU% in 2017', 
           legend_title='DFU %',
           zoom=T, subset=list(var='iso3', value='IND'),
           debug=F)

ggsave(filename=file.path(out.dir, 'india_dfu2.png'), plot=india, 
       width=6, height=6, units='in', dpi=100)

#testing
ctry.name <- 'Nigeria'
ctry_data <- data$admin2 %>% 
  copy %>% 
  filter(NAME_0==ctry.name)
ctry.zoom <- data.table(x1=1, x2=15, y1=3, y2=15) #NGA

ctry.dfu <-
  plot_map(ctry_data, this_var='dfu',
           annotations, limits=c(0, 1), title='DFU% in 2017', 
           legend_colors=colors, legend_color_values=color_values,
           legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
           legend_title='DFU %', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_dfu.png')), plot=ctry.dfu, 
       width=6, height=6, units='in', dpi=600)

#pct scale
ctry_pct_values <- c(seq(0, .6, length.out = 2), seq(.6, .9, length.out = 8), seq(.9, 1, length.out = 2)) %>%
  unique %>%
  rescale

ctry.pct <- 
  plot_map(ctry_data, this_var='hap_pct',
           annotations, limits=c(0, 1), title='HAP/TAP in 2017', 
           # legend_colors=colors, legend_color_values=ctry_pct_values,
           # legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
           legend_title='HAP/TAP', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_hap_pct.png')), plot=ctry.pct, 
       width=6, height=6, units='in', dpi=600)

#pm25 scale
ctry_pct_values <- c(seq(0, .6, length.out = 2), seq(.6, .9, length.out = 8), seq(.9, 1, length.out = 2)) %>%
  unique %>%
  rescale

ctry.tap <- 
  plot_map(ctry_data, this_var='tap_pc',
           annotations, limits=c(0, 500), title='Annual per-capita TAP dose in 2017', 
           # legend_colors=colors, legend_color_values=ctry_pct_values,
           # legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
           legend_title='PM2.5 ug/m3', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_pc.png')), plot=ctry.tap, 
       width=6, height=6, units='in', dpi=300)

#paf scale
ctry_paf_values <- c(seq(.2, .45, length.out = 2), seq(.45, .55, length.out = 8), seq(.5, .6, length.out = 2)) %>%
  unique %>%
  rescale
  
ctry.paf <- 
  plot_map(ctry_data, this_var='tap_paf',
           annotations, limits=c(0, .75), title='PAF of LRI Attributable to TAP in 2017', 
           legend_colors=colors, legend_color_values=ctry_paf_values,
           legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
           legend_title='TAP PAF', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_paf.png')), plot=ctry.paf, 
       width=6, height=6, units='in', dpi=600)

#attributable LRI rates
ctry_lri_values <- c(seq(0, 1, length.out = 2), seq(1, 5, length.out = 8), seq(5, 10, length.out = 2)) %>%
  unique %>%
  rescale

ctry.lri <- 
  plot_map(ctry_data, this_var='tap_lri',
           annotations, limits=c(0, 10), title='Rate/1000 of LRI Attributable to TAP in 2017', 
           legend_colors=colors, legend_color_values=ctry_lri_values,
           legend_breaks=seq(0, 10, 2), legend_labels=seq(0, 10, 2),
           legend_title='TAP LRI Rate', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_lri.png')), plot=ctry.lri, 
       width=6, height=6, units='in', dpi=600)

ctry.lri <- 
  plot_map(ctry_data, this_var='tap_lri',
           annotations, limits=c(0, 10), title='Rate/1000 of LRI Attributable to TAP in 2017', 
           legend_title='TAP LRI Rate', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_lri_default.png')), plot=ctry.lri, 
       width=6, height=6, units='in', dpi=600)

ctry.lri <- 
  plot_map(ctry_data, this_var='tap_lri',
           annotations, limits=c(0, 10), title='Rate/1000 of LRI Attributable to TAP in 2017', 
           legend_colors=colors, legend_color_values=ctry_lri_values,
           legend_breaks=seq(0, 10, 2), legend_labels=seq(0, 10, 2),
           legend_title='TAP LRI Rate', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_lri.png')), plot=ctry.lri, 
       width=6, height=6, units='in', dpi=600)

ctry.lri <- 
  plot_map(ctry_data, this_var='hap_lri',
           annotations, limits=c(0, 9), title='Rate/1000 of LRI Attributable to HAP in 2017', 
           legend_colors=colors, legend_color_values=ctry_lri_values,
           legend_breaks=seq(0, 9, 2), legend_labels=seq(0, 9, 2),
           legend_title='HAP LRI Rate', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_hap_lri.png')), plot=ctry.lri, 
       width=6, height=6, units='in', dpi=600)

ctry.lri <- 
  plot_map(ctry_data, this_var='aap_lri',
           annotations, limits=c(0, 9), title='Rate/1000 of LRI Attributable to AAP in 2017', 
           legend_colors=colors, legend_color_values=ctry_lri_values,
           legend_breaks=seq(0, 9, 2), legend_labels=seq(0, 9, 2),
           legend_title='AAP LRI Rate', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_aap_lri.png')), plot=ctry.lri, 
       width=6, height=6, units='in', dpi=600)

ctry_lri_values <- c(seq(0, .25, length.out = 2), seq(.25, 1.25, length.out = 8), seq(1.5, 3, length.out = 2)) %>%
  unique %>%
  rescale

ctry.lri <- 
  plot_map(ctry_data, this_var='aap_lri',
           annotations, limits=c(0, 3), title='Rate/1000 of LRI Attributable to AAP in 2017', 
           legend_colors=colors, legend_color_values=ctry_lri_values,
           legend_breaks=seq(0, 3, .5), legend_labels=seq(0, 3, .5),
           legend_title='AAP LRI Rate', legend_flip=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_aap_lri_newscale.png')), plot=ctry.lri, 
       width=6, height=6, units='in', dpi=600)

#plot changes
ctry_d <- data_d$admin2 %>% 
  copy %>% 
  filter(NAME_0==ctry.name & year==2017)

ctry_d_values <- c(seq(-.1, -.05, length.out = 2), seq(-.05, 0, length.out = 8), seq(0, .05, length.out = 2)) %>%
  unique %>%
  rescale

ctry.d.map <- 
  plot_map(ctry_d, this_var='tap_paf_d',
           annotations, limits=c(-.1, .05), title='TAP PAF, Change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=ctry_d_values,
           legend_breaks=seq(-.1, .05, .025), legend_labels=seq(-.1, .05,.025),
           legend_title='Change in PAF of TAP', legend_flip=T,
           pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_paf_d.png')), plot=ctry.d.map, 
       width=6, height=6, units='in', dpi=100)


ctry_d_values <- c(seq(-.5, -.1, length.out = 8), seq(-.1, 0, length.out = 2)) %>%
  unique %>%
  rescale

ctry.d.map <- 
  plot_map(ctry_d, this_var='hap_pct_d',
           annotations, limits=c(-.5, 0), title='HAP Share, Change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=ctry_d_values,
           legend_breaks=seq(-.5, 0, .1), legend_labels=seq(-.5, 0,.1),
           legend_title='Change in HAP/TAP', legend_flip=T,
           pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
           zoom=ctry.zoom,
           debug=F)

ggsave(filename=file.path(out.dir, paste0(ctry.name, '_hap_pct_d.png')), plot=ctry.d.map, 
       width=6, height=6, units='in', dpi=100)

# ctry.dt <- data_2017$tap_paf$admin2 %>% copy
# ctry.map <- 
#   plot_map(ctry.dt[ctry.dt$NAME_0 %like% ctry.name,], annotations, limits=c(0, .75), title='2017', 
#            legend_colors=colors, legend_color_values=paf_values,
#            legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
#            legend_title='tap PAF', custom_scale=T,
#            pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#            zoom=ctry.zoom,
#            debug=F)
# 
# ggsave(filename=file.path(out.dir, paste0(ctry.name, '_tap_paf.png')), plot=ctry.map, 
#        width=6, height=6, units='in', dpi=100)
# 
# plot_map(ctry.dt[ctry.dt$NAME_0 %like% ctry.name,], annotations, limits=c(0, 1), title='2017', 
#          legend_colors=colors, legend_color_values=color_values,
#          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#          legend_title='hap/tap', custom_scale=T,
#          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#          zoom=ctry.zoom,
#          debug=F)

#***********************************************************************************************************************

# ---SCRAPS-------------------------------------------------------------------------------------------------------------
# ##cloud version with top 14##
# #now make a single inset plot for each of the remaining top by region (and add Pakistan to round it out)
# insets <- c(top_countries_gbdreg[, iso3]) %>% 
#   .[!(. %in% custom_countries)] %>% 
#   sort %>% 
#   lapply(1:length(.), makeInset, loclist=., scale_labels=T, type='cloud_alpha')
# 
# #arrange into master figure
# all_grobs <- copy(insets)
# all_grobs[[13]] <- master_plot
# lay <- rbind(c(13,13,13,13,1,2,3),
#              c(13,13,13,13,4,5,6),
#              c(13,13,13,13,7,8,9),
#              c(13,13,13,13,10,11,12))
# plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
#                     top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
#                                  gp = gpar(fontsize=24)),
#                     bottom=textGrob("Percent of TAP contributed by ambient sources", 
#                                     gp = gpar(fontsize=17))
# ) %>% 
#   grid.arrange
# 
# ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2_cloud_alpha.png'),
#        width=12, height=8, units='in', dpi=900)
# 
# #make for all countries
# makeFigure2 <- function(country) {
#   
#   message('plotting ', country)
#   
#   plot <- 
#     ggplot(plot.dt[iso3 %in% country], 
#            aes(x=hap_pct, y=tap_paf, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
#     geom_point() + 
#     geom_line(alpha=.1) +
#     geom_vline(xintercept=.5) +
#     scale_color_brewer('Country', palette='Dark2') +
#     scale_shape_manual('Year', values=c(1, 16)) +
#     scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
#     scale_y_continuous("Population Attributable Fraction of LRI to TAP", limits=c(0,1)) +
#     ggtitle(country) +
#     coord_flip() +
#     theme_bw(base_size = 16) +
#     theme(
#       legend.position = c(.10, .90),
#       legend.justification = c("right", "top"),
#       legend.box.just = "right",
#       legend.margin = margin(6, 6, 6, 6)
#     )
#   
#   print(plot)
#   
#   return(NULL)
#   
# }
# 
# pdf(paste0(out.dir, '/presub_figure_2_all_countries.pdf'),
#     height=8, width=12)
# 
# lapply(unique(plot.dt$iso3), makeFigure2)
# 
# dev.off()
# 
# tic('plotting 2017')
# gg_2017 <- list(
#   'hap_pct'=plot_map(data_2017$hap_pct$admin2, annotations, limits=c(0, 1), title='2017', 
#                      legend_colors=colors, legend_color_values=color_values,
#                      legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#                      legend_title='hap/tap', custom_scale=T,
#                      pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                      zoom=zoom.global,
#                      debug=F),
#   # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
#   #          legend_colors=colors, legend_color_values=color_values,
#   #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#   #          legend_title='tap PM2.5 per capita', custom_scale=T,
#   #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#   #          zoom=zoom.global,
#   #          debug=F),
#   'dfu'=plot_map(data_2017$dfu$admin2, annotations, limits=c(0, 1), title='2017', 
#                  legend_colors=colors, legend_color_values=color_values,
#                  legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#                  legend_title='dfu %', custom_scale=T,
#                  pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                  zoom=zoom.global,
#                  debug=F),
#   'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, .75), title='2017', 
#                      legend_colors=colors, legend_color_values=paf_values,
#                      legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
#                      legend_title='tap PAF', custom_scale=T,
#                      pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                      zoom=zoom.global,
#                      debug=F)
# )
# toc()
# 
# tic('plotting 2000')
# gg_2000 <- list(
#   'hap_pct'=plot_map(data_2000$hap_pct$admin2, annotations, limits=c(0, 1), title='2000', 
#                      legend_colors=colors, legend_color_values=color_values,
#                      legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#                      legend_title='hap/tap', custom_scale=T,
#                      pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                      zoom=zoom.global,
#                      debug=F),
#   # 'tap_pc'=plot_map(data_2000$tap_pc$admin2, annotations, limits=c(0, 1), title='2000', 
#   #          legend_colors=colors, legend_color_values=color_values,
#   #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#   #          legend_title='tap PM2.5 per capita', custom_scale=T,
#   #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#   #          zoom=zoom.global,
#   #          debug=F),
#   'dfu'=plot_map(data_2000$dfu$admin2, annotations, limits=c(0, 1), title='2000', 
#                  legend_colors=colors, legend_color_values=color_values,
#                  legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#                  legend_title='dfu %', custom_scale=T,
#                  pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                  zoom=zoom.global,
#                  debug=F),
#   'tap_paf'=plot_map(data_2000$tap_paf$admin2, annotations, limits=c(0, .75), title='2000', 
#                      legend_colors=colors, legend_color_values=paf_values,
#                      legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
#                      legend_title='tap PAF', custom_scale=T,
#                      pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                      zoom=zoom.global,
#                      debug=F)
# )
# toc()
# 
# tic('plotting 2000-2017 change')
# gg_d <- list(
#   'hap_pct_d'=plot_map(data_d$hap_pct_d$admin2, annotations, limits=c(-.2, .2), title='change from 2000-2017', 
#                        legend_colors=d_colors, legend_color_values=d_values,
#                        legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
#                        legend_title='hap/tap', custom_scale=T,
#                        pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                        zoom=zoom.global,
#                        debug=F),
#   # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
#   #          legend_colors=colors, legend_color_values=color_values,
#   #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#   #          legend_title='tap PM2.5 per capita', custom_scale=T,
#   #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#   #          zoom=zoom.global,
#   #          debug=F),
#   'dfu_d'=plot_map(data_d$dfu_d$admin2, annotations, limits=c(-.2, .2), title='change from 2000-2017', 
#                    legend_colors=d_colors, legend_color_values=d_values,
#                    legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
#                    legend_title='dfu %', custom_scale=T,
#                    pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                    zoom=zoom.global,
#                    debug=F)
#   # 'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, 1), title='change from 2000-2017', 
#   #                    legend_colors=colors, legend_color_values=paf_values,
#   #                    legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#   #                    legend_title='tap PAF', custom_scale=T,
#   #                    pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#   #                    zoom=zoom.global,
#   #                    debug=F)
# )
# toc()
# 
# tic('plotting 2000-2017 change rate')
# gg_dr <- list(
#   'hap_pct_dr'=plot_map(data_d$hap_pct_dr$admin2, annotations, limits=c(-1, 1), title='change rate from 2000-2017', 
#                         legend_colors=d_colors, legend_color_values=dr_values,
#                         legend_breaks=seq(-1, 1, .2), legend_labels=seq(-1, 1, .2),
#                         legend_title='hap/tap', custom_scale=T,
#                         pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                         zoom=zoom.global,
#                         debug=F),
#   # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
#   #          legend_colors=colors, legend_color_values=color_values,
#   #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#   #          legend_title='tap PM2.5 per capita', custom_scale=T,
#   #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#   #          zoom=zoom.global,
#   #          debug=F),
#   'dfu_dr'=plot_map(data_d$dfu_dr$admin2, annotations, limits=c(-1, 1), title='change rate from 2000-2017', 
#                     legend_colors=d_colors, legend_color_values=dr_values,
#                     legend_breaks=seq(-1, 1, .2), legend_labels=seq(-1, 1, .2),
#                     legend_title='dfu %', custom_scale=T,
#                     pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#                     zoom=zoom.global,
#                     debug=F)
#   # 'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, 1), title='change from 2000-2017', 
#   #                    legend_colors=colors, legend_color_values=paf_values,
#   #                    legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
#   #                    legend_title='tap PAF', custom_scale=T,
#   #                    pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
#   #                    zoom=zoom.global,
#   #                    debug=F)
# )
# toc()
# 
# tic('ggsaving 2017')
# ggsave(filename=file.path(out.dir, 'hap_pct_2017.png'), plot=gg_2017$hap_pct, 
#        width=10, height=6, units='in', dpi=200)
# 
# ggsave(filename=file.path(out.dir, 'dfu_2017.png'), plot=gg_2017$dfu, 
#        width=10, height=6, units='in', dpi=200)
# 
# ggsave(filename=file.path(out.dir, 'tap_paf_2017.png'), plot=gg_2017$tap_paf, 
#        width=10, height=6, units='in', dpi=200)
# toc()
# 
# tic('ggsaving 2000')
# ggsave(filename=file.path(out.dir, 'hap_pct_2000.png'), plot=gg_2000$hap_pct, 
#        width=10, height=6, units='in', dpi=600)
# 
# ggsave(filename=file.path(out.dir, 'dfu_2000.png'), plot=gg_2000$dfu, 
#        width=10, height=6, units='in', dpi=600)
# 
# ggsave(filename=file.path(out.dir, 'tap_paf_2000.png'), plot=gg_2000$tap_paf, 
#        width=10, height=6, units='in', dpi=600)
# toc()
# 
# tic('ggsaving change')
# ggsave(filename=file.path(out.dir, 'hap_pct_d.png'), plot=gg_d$hap_pct_d, 
#        width=10, height=6, units='in', dpi=600)
# 
# ggsave(filename=file.path(out.dir, 'dfu_d.png'), plot=gg_d$dfu_d, 
#        width=10, height=6, units='in', dpi=600)
# 
# # ggsave(filename=file.path(out.dir, 'tap_paf_d.png'), plot=gg_2000$tap_paf, 
# #        width=10, height=6, units='in', dpi=600)
# toc()
# 
# tic('ggsaving change rate')
# ggsave(filename=file.path(out.dir, 'hap_pct_dr.png'), plot=gg_d$hap_pct_dr, 
#        width=10, height=6, units='in', dpi=600)
# 
# ggsave(filename=file.path(out.dir, 'dfu_dr.png'), plot=gg_d$dfu_dr, 
#        width=10, height=6, units='in', dpi=600)
# 
# # ggsave(filename=file.path(out.dir, 'tap_paf_d.png'), plot=gg_2000$tap_paf, 
# #        width=10, height=6, units='in', dpi=600)
# toc()
# 
# # tic('bmp saving')
# # png(filename=file.path(out.dir, 'testreg_nosf.bmp'), 
# #     units='px', width=2700, height=1500)
# # print(gg)
# # dev.off()
# # toc()
# 
# toc()
# 
# plot <- 
#   ggplot(plot.dt[iso3 %in% custom_countries], 
#          aes(x=hap_pct, y=tap_paf*lri*1e3, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
#   geom_point() + 
#   geom_line(alpha=.1) +
#   geom_vline(xintercept=.5) +
#   scale_color_brewer('Country', palette='Dark2') +
#   scale_shape_manual('Year', values=c(1, 16)) +
#   scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
#   scale_y_continuous("Rate/1000 of LRI Attributable to TAP", limits=c(0,4)) +
#   coord_flip() +
#   theme_bw(base_size = 16) +
#   theme(
#     legend.position = c(.90, .30),
#     legend.justification = c("right", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6)
#   )
# 
# ggsave(filename=file.path(out.dir, 'presub_figure_2a.png'),
#        width=16, height=8, units='in', dpi=900)
# 
# master_plot_flip <- 
#   ggplot(plot.dt[iso3 %in% custom_countries], 
#          aes(x=tap_paf, y=1-hap_pct, color=location_name, shape=year %>% as.factor, group=ADM2_CODE)) + 
#   geom_point() + 
#   geom_line(alpha=.1) +
#   geom_hline(yintercept=.5, linetype="dashed") +
#   #scale_color_brewer('Country', palette='Dark2') +
#   scale_colour_manual('Country', values=custom_cols) +
#   scale_shape_manual('Year', values=c(1, 16)) +
#   scale_y_continuous("HAP / TAP Share", limits=c(0, 1), expand = c(0, 0)) +
#   scale_x_continuous("Population Attributable Fraction of LRI", limits=c(.2,.6), expand = c(0, 0)) +
#   annotate("rect", xmin = .5, xmax = 1, ymin = .2, ymax = .6, fill='lightsalmon2', alpha = .2) +
#   annotate("rect", xmin = 0, xmax = .5, ymin = .2, ymax = .6, fill='lightgoldenrod2', alpha = .2) +
#   coord_flip() +
#   theme_bw(base_size = 16) +
#   theme(
#     #legend.position = c(.84, .69),
#     legend.position = c(.82, .41),
#     legend.justification = c("left", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6)
#   )
# 
# ggsave(filename=file.path(out.dir, 'presub_figure_2b.png'),
#        width=12, height=8, units='in', dpi=900)
# 
# #with vertical layout
# lay <- rbind(c(1,2,3,4),
#              c(9,9,9,5),
#              c(9,9,9,6),
#              c(9,9,9,7),
#              c(9,9,9,8))
# plot <- grid.arrange(grobs=insets, layout_matrix=lay)
# plot <- grid.arrange(arrangeGrob(grobs=insets, layout_matrix=lay, 
#                                  bottom=textGrob("Percent of TAP contributed by ambient sources")))
# ggsave(plot=plot, filename=file.path(out.dir, 'presub_figure_2_master.png'),
#        width=12, height=8, units='in', dpi=900)
# 
# #now make a single inset plot for each of the remaining top 10 countries
# insets <- top_countries_c[5:14] %>% 
#   .[!(. %in% custom_countries)] %>% 
#   sort %>% 
#   lapply(., makeInset, scale_labels=T)
# 
# ##scatter version##
# #arrange into master figure
# all_grobs <- copy(insets)
# all_grobs[[9]] <- master_plot
# lay <- rbind(c(9,9,9,9,1,2),
#              c(9,9,9,9,3,4),
#              c(9,9,9,9,5,6),
#              c(9,9,9,9,7,8))
# plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
#                     top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
#                                  gp = gpar(fontsize=24)),
#                     bottom=textGrob("Percent of TAP contributed by ambient sources", 
#                                     gp = gpar(fontsize=17))
# ) %>% 
#   grid.arrange
# 
# ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2.png'),
#        width=12, height=8, units='in', dpi=900)
# 
# ##cloud version##
# #now make a single inset plot for each of the remaining top 10 countries
# insets <- top_countries_c[5:14] %>% 
#   .[!(. %in% custom_countries)] %>% 
#   sort %>% 
#   lapply(., makeInset, scale_labels=T, type='cloud')
# 
# #arrange into master figure
# all_grobs <- copy(insets)
# all_grobs[[9]] <- master_plot
# lay <- rbind(c(9,9,9,9,1,2),
#              c(9,9,9,9,3,4),
#              c(9,9,9,9,5,6),
#              c(9,9,9,9,7,8))
# plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
#                     top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
#                                  gp = gpar(fontsize=24)),
#                     bottom=textGrob("Percent of TAP contributed by ambient sources", 
#                                     gp = gpar(fontsize=17))
# ) %>% 
#   grid.arrange
# 
# ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2_cloud.png'),
#        width=12, height=8, units='in', dpi=900)
# 
# ##cloud version with top 14##
# #now make a single inset plot for each of the remaining top 14 countries
# insets <- top_countries_c %>% 
#   .[!(. %in% custom_countries)] %>% 
#   sort %>% 
#   lapply(., makeInset, scale_labels=T, type='cloud')
# 
# #arrange into master figure
# all_grobs <- copy(insets)
# all_grobs[[13]] <- master_plot
# lay <- rbind(c(13,13,13,13,1,2,3),
#              c(13,13,13,13,4,5,6),
#              c(13,13,13,13,7,8,9),
#              c(13,13,13,13,10,11,12))
# plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
#                     top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
#                                  gp = gpar(fontsize=24)),
#                     bottom=textGrob("Percent of TAP contributed by ambient sources", 
#                                     gp = gpar(fontsize=17))
# ) %>% 
#   grid.arrange
# 
# ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2_cloud_14.png'),
#        width=12, height=8, units='in', dpi=900)
# 
# ggplot(plot.dt[year==2018 & !is.na(country)], aes(x = tap_pc, 
#                                                   y = country,
#                                                   fill = grouping %>% 
#                                                     as.factor)) +
#   geom_density_ridges(aes(height=..density..,  # Notice the additional
#                           weight=pop),     # aes mappings
#                       scale= 1.5,
#                       stat="density") +
#   geom_density_ridges(aes(height=..density..,  # Notice the additional
#                           weight=pop),     # aes mappings
#                       scale= 1.5,
#                       stat="density") +
#   #scale_fill_manual(values=reg_colors, guide=F) +
#   scale_x_sqrt('TAP in PM2.5/person', limits=c(0, 750)) +
#   #facet_grid(~grouping) +
#   theme_ridges() +
#   labs(title = '') +
#   theme(axis.title.y=element_blank())
# 
# ggplot(dt[year==2017], aes(x=tap_pc, y=lri_c)) +
#   geom_density(stat='identity', aes(weight=pop), n=400) +
#   scale_fill_viridis_c() +
#   scale_x_sqrt(limits=c(0,750)) +
#   theme_minimal()
# 
# ggplot(data=plot.dt[year==2018 & iso3 %in% custom_countries], aes(x=pm,  y=log(pop), color=iso3 %>% as.factor)) +
#   #geom_ribbon() + 
#   facet_wrap(~grouping) +
#   geom_line() +
#   #geom_density(stat = "identity") +
#   scale_color_brewer(palette='Dark2') +
#   scale_x_continuous(limits=c(0, 750)) +
#   #scale_y_continuous(limits=c(0, 10)) +
#   #coord_trans(y='sqrt') +
#   ggtitle('2018') +
#   theme_minimal()
# 
# ggplot(data=plot.dt[year==2000 & !is.na(super_region_id)], aes(x=pm,  y=n, fill=hap_share)) +
#   #geom_ribbon() + 
#   facet_wrap(~super_region_name) +
#   geom_bar(stat = "identity") +
#   scale_fill_viridis(option='viridis') +
#   scale_x_continuous(limits=c(0, 750)) +
#   scale_y_continuous(limits=c(0, 20000)) +
#   coord_trans(y='sqrt') +
#   ggtitle('2000') +
#   theme_minimal()
# 
# ggplot(data=plot.dt[year==2018 & !is.na(super_region_id)], aes(x=pm,  y=n, fill=hap_share)) +
#   #geom_ribbon() + 
#   facet_wrap(~super_region_name) +
#   geom_bar(stat = "identity") +
#   scale_fill_viridis(option='viridis') +
#   scale_x_continuous(limits=c(0, 750)) +
#   scale_y_continuous(limits=c(0, 20000)) +
#   coord_trans(y='sqrt') +
#   ggtitle('2018') +
#   theme_minimal()
# 
# # ggplot(data=plot.dt[year==2018], aes(tap_pc,  y = ..count.., weight = lri_c, fill=hap_pct>.5)) +
# #   geom_density(binwidth = 5, position = "stack", alpha=.2) + 
# #   theme_bw()
# 

# #prep data for ridgeplot
# plot.dt[iso3 %in% figure_2_countries, country := ADM0_NAME]
# plot.dt[country %>% is.na, country := super_region_name]
# plot.dt[, mean := weighted.mean(tap_pc_mean, pop), by=region_name]
# plot.dt <- plot.dt[plot.dt[,do.call(order, .SD), .SDcols = c('super_region_id', 'mean')]]
# plot.dt[, reg_fac := factor(region_name, levels=unique(region_name))]
# 
# type.plot.dt <- list (
#   plot.dt %>% copy %>% .[, type:= 'Total'],
#   plot.dt %>% copy %>% .[,tap_pc_mean := tap_pc_mean * hap_pct_mean] %>% .[, type:= 'Household'],
#   plot.dt %>% copy %>% .[,tap_pc_mean := tap_pc_mean * (1-hap_pct_mean)] %>% .[, type:= 'Ambient']
# ) %>% rbindlist
# 
# type.plot.dt[, wt := pop/sum(pop, na.rm=T), by=.(year, reg_fac, type)]
# 
# plot.dt.wt <- copy(type.plot.dt)
# plot.dt.wt[, wt := round(pop/1e3, 0)]
# plot.dt.wt <- plot.dt.wt[rep(seq(.N), wt), !"wt"] #expand rows to create weights
# 
# 
# #type ridgeplot
# plot <-
#   ggplot(plot.dt.wt[year==2018 & !is.na(iso3)], aes(x = tap_pc_mean, 
#                                                     y = reg_fac,
#                                                     fill = super_region_id %>% as.factor)) +
#   geom_density_ridges(data=plot.dt.wt[year==2000 & !is.na(region_name)], 
#                       scale= .95,
#                       fill='gray72',
#                       alpha=.75) +
#   geom_density_ridges(scale= .95,
#                       alpha=.75) +
#   geom_vline(xintercept=10, linetype='dashed', color='gray10') +
#   facet_grid(~type) +
#   scale_fill_manual(values=reg_colors, guide=F) +
#   scale_x_sqrt('TAP in PM2.5/person', limits=c(0, 750)) +
#   theme_minimal() +
#   labs(title = '') +
#   theme(axis.title.y=element_blank(),
#         text = element_text(size=16))
# ggsave(filename=file.path(out.dir, 'tap_distributions.png'), plot=plot, 
#        width=12, height=8, units='in', dpi=500)
# 
# #single density plot
# plot <-
#   ggplot() +
#   geom_density(data=plot.dt.wt[year==2018 & !is.na(iso3) & type=='Total' & iso3!='IND|IDN|NGA'],
#                aes(x = tap_pc_mean, y = ..density..), 
#                position = "stack", n=400, fill='gray', alpha=.3) +
#   geom_density(data=plot.dt.wt[year==2018 & !is.na(iso3) & type=='Total' & iso3=='IND'],
#                aes(x = tap_pc_mean, y = ..density..), 
#                position = "stack", n=400, fill='red', alpha=.3) +
#   geom_density(data=plot.dt.wt[year==2018 & !is.na(iso3) & type=='Total' & iso3=='IDN'],
#                aes(x = tap_pc_mean, y = ..density..), 
#                position = "stack", n=400, fill='blue', alpha=.3) +
#   geom_density(data=plot.dt.wt[year==2018 & !is.na(iso3) & type=='Total' & iso3=='NGA'],
#                aes(x = tap_pc_mean, y = ..density..), 
#                position = "stack", n=400, fill='green', alpha=.3) +
#   geom_vline(xintercept=10, linetype='dashed', color='gray10') +
#   scale_x_sqrt('TAP in PM2.5/person', limits=c(0, 500)) +
#   #scale_fill_manual(values=reg_colors, guide=F) +
#   theme_minimal() +
#   labs(title = '') +
#   theme(axis.title.y=element_blank(),
#         text = element_text(size=16))

# #add 18 populations for weighting at country level for lusophonic countries
# lus_projs <- merge(projs[iso3%in%c('AGO', 'BRA', 'GNB', 'MOZ', 'TLS')], 
#                    dt[year==max(year) & grouping=='child' & cause=='all', .(pop_total, ADM2_CODE)], 
#                    by='ADM2_CODE')
# lus_projs <- setkey(lus_projs, draw, year, ADM1_NAME)[, sev := weighted.mean(sev, w=pop_total, na.rm=T), 
#                                                       by=key(lus_projs)] %>% 
#   unique(., by=key(.))
# 
# #ridgeplot of probs for Brazil
# plot <-
#   ggplot(lus_projs[year==2030], aes(x = sev, y = fct_reorder(ADM0_NAME, sev, .fun=mean), 
#                                     fill = 0.5 - abs(0.5 - stat(ecdf)))) +
#   stat_density_ridges(data=lus_projs[year==2018],
#                       scale= .85,
#                       fill='gray72',
#                       alpha=.75,
#                       calc_ecdf = T) +
#   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
#   scale_fill_viridis_c(name = "Tail probability", direction = -1) +
#   geom_vline(xintercept=.01, linetype='dashed', color='gray10') +
#   scale_x_continuous('Access to Clean Energy') +
#   theme_minimal() +
#   theme(axis.title=element_blank())
# ggsave(filename=file.path(out.dir, 'lus_sdg_prob_density.png'), plot=plot, 
#        width=12, height=6, units='in', dpi=500)