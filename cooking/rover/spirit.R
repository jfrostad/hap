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
core_repo       <- '/homes/jfrostad/_code/lbd/hap/'
commondir       <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')

#load packages
package_lib    <- sprintf('%s_code/_lib/pkg',h_root)
## Load libraries and  MBG project functions.
.libPaths(package_lib)
pacman::p_load(data.table, scales, ggplot2, gridExtra, RColorBrewer, sf, viridis, farver, reldist) 
package_list    <- package_list <- fread('/share/geospatial/mbg/common_inputs/package_list.csv') %>% t %>% c

# Use setup.R functions to load common LBD packages and mbg_central "function" scripts
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
  mbg_setup(package_list = package_list, repos = core_repo)

#capture date
today <- Sys.Date() %>% gsub("-", "_", .)

#options
run_date <- '2020_03_27_13_00_02'
lri_run_date <- '2020_01_10_15_18_27'

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
hap.paths <- data.table(admin2=file.path(data.dir, 'admin_2_summary.csv'))
hap.paths.d <- data.table(admin2=file.path(data.dir, 'admin_2_delta_summary.csv'))

###Output###
out.dir  <- file.path('/ihme/geospatial/mbg/cooking/maps', run_date) %T>% dir.create(recursive = T)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
##function lib##
#PE functions#
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

#read LRI counts
lri_rate <- 'has_lri_admin_2_raked_mortality_summary.csv'
lri_counts <- 'has_lri_c_admin_2_raked_mortality_summary.csv'

has_lri <- file.path('/ihme/geospatial/mbg/lri/has_lri/output', 
                     lri_run_date, 'pred_derivatives/admin_summaries', lri_rate) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri=mean)]

has_lri_c <- file.path('/ihme/geospatial/mbg/lri/has_lri/output', 
                       lri_run_date, 'pred_derivatives/admin_summaries', lri_counts) %>% 
  fread %>% 
  .[, .(ADM0_CODE, ADM2_CODE, year, lri_c=mean)]

#combine and save all ad2 level results
dt <-
  list.files(data.dir, pattern='ad2_tap_results', full.names = T) %>% 
  lapply(., fread) %>% 
  rbindlist(use.names=T, fill=T) %>% 
  merge(has_lri, by=c('ADM0_CODE', 'ADM2_CODE', 'year')) %>% 
  merge(has_lri_c, by=c('ADM0_CODE', 'ADM2_CODE', 'year')) %>% 
  .[, tap_lri := lri * 1000 * tap_paf] %>% #do some postestimation
  .[, hap_lri := lri * 1000 * tap_paf*hap_pct] %>% 
  .[, aap_lri := lri * 1000 * tap_paf*(1-hap_pct)] %>% 
  .[, tap_lri_c := lri_c * tap_paf] %>% #do some postestimation
  .[, hap_lri_c := lri_c * tap_paf*hap_pct] %>% 
  .[, aap_lri_c := lri_c * tap_paf*(1-hap_pct)] %T>% 
  #also output the file for later
  write.csv(., file.path(data.dir, 'admin_2_summary.csv'), row.names = F)

#merge sr region names/IDs
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6) %>% 
  .[, .(iso3=ihme_loc_id, location_name, super_region_id, super_region_name, region_id, region_name)] #subset to relevant columns
dt <- merge(dt, locs, by='iso3', all.x=T)

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
these_cols <- c('hap_pct', 'dfu', 'tap_paf', 'tap_pc')
d_cols <- paste0(these_cols, '_d')
dr_cols <- paste0(these_cols, '_dr')
aroc_cols <- paste0(these_cols, '_aroc')
dt_d <- setkey(dt, ADM0_CODE, ADM2_CODE) %>% 
  .[year %in% c(2000, end_year)] %>% 
  .[, (d_cols) := .SD-data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %>% 
  .[, (dr_cols) := (.SD-data.table::shift(.SD,n=1))/data.table::shift(.SD,n=1), .SDcols=these_cols, by=key(dt)] %T>% 
  .[, (aroc_cols) := (.SD-data.table::shift(.SD,n=1))/(end_year-2000), .SDcols=these_cols, by=key(dt)] %T>% 
  #also output the file for later
  write.csv(., file.path(data.dir, 'admin_2_delta_summary.csv'), row.names = F)

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


#read in input data and prepare it for mapping
data <-  
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=2000, end_year=2017,
                   custom_path = hap.paths,
                   geo_levels=c('admin2'),
                   cores=cores)
data_d <-
  load_map_results(indicator, indicator_group, run_date, raked, 
                   start_year=2000, end_year=2017,
                   custom_path = hap.paths.d,
                   geo_levels=c('admin2'),
                   cores=cores)

#define extent of map
zoom.afr <- data.table(x1=-10, x2=50, y1=-20, y2=40)
zoom.global <- data.table(x1=-120, x2=150, y1=-40, y2=55)
#***********************************************************************************************************************

# ---FIGURE 2----------------------------------------------------------------------------------------------------------
#setup plot data
plot.dt <- dt %>% 
  copy %>% 
  .[year %in% c(2000, 2017)] %>% 
  .[year==2017, year := 2018] %>% 
  na.omit(., cols=c('hap_pct', 'tap_paf'))

#setup the list of top countries
top_countries <- #defined based on LRI rates as suggested by simon
  dt[year==min(dt$year), .(mean=weighted.mean(lri, w=pop, na.rm=T)), by=.(iso3)] %>%
  .[order(mean)] %>%
  tail(10) %>%
  .[, unique(iso3)]

top_countries_c <- #defined based on LRI counts as suggested by simon
  dt[year==min(dt$year), .(sum=sum(lri_c, na.rm=T)), by=.(iso3)] %>%
  .[order(sum)] %>%
  tail(14) %>%
  .[, unique(iso3)]

custom_countries <- #defined based on aesthetics - all countries in different range
  c('ETH', 'KEN', 'IND', 'MNG', 'THA') 

custom_cols <- c('Ethiopia'='midnightblue', 'Kenya'='cornflowerblue', 'India'='darkgoldenrod', 
                 'Mongolia'='firebrick', 'Thailand'='seagreen')

custom_cols <- c('Ethiopia'='firebrick', 'Kenya'='indianred2', 'India'='darkgoldenrod', 
                 'Mongolia'='mediumorchid4', 'Thailand'='olivedrab')

master_plot <- 
  ggplot(plot.dt[iso3 %in% custom_countries], 
         aes(x=1-hap_pct, y=tap_paf, color=location_name, shape=year %>% as.factor, group=ADM2_CODE)) + 
  annotate("rect", xmin = -.02, xmax = .5, ymin = .15, ymax = .60, fill='steelblue', alpha = .2) +
  annotate("rect", xmin = .5, xmax = 1.02, ymin = .15, ymax = .60, fill='tomato', alpha = .2) +
  geom_line(alpha=.1) +
  geom_point(size=2) + 
  geom_vline(xintercept=.5, linetype="dashed") +
  scale_colour_manual('Country', values=custom_cols) +
  scale_shape_manual('Year', values=c(1, 16), guide=F) +
  scale_x_continuous('', limits=c(-.02, 1.02), labels = scales::percent, breaks=c(.25, .5, .75), expand = c(0, 0)) +
  scale_y_continuous("Percent of LRI attributable to TAP (PAF)", 
                     limits=c(.15,.60), labels = scales::percent, breaks=c(.2, .4, .6), expand = c(0, 0)) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(.01, .25),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    plot.margin = margin(12, 0, 0, 6, "pt"),
    axis.text.y = element_text(angle = 90, hjust=.4),
    axis.title.x = element_blank()
  )

ggsave(filename=file.path(out.dir, 'presub_figure_2b.png'),
       width=12, height=8, units='in', dpi=900)

#helper function to create smaller inset versions of this plot
makeInset <- function(loc, type='scatter', scale_labels=F) {
  
  message('plotting ', loc)

  dt <- plot.dt[iso3%in%loc]
  cap <- plot.dt[iso3%in%loc, unique(location_name)]
  if(cap %like% 'Congo') cap <- 'D.R. Congo'
  if(cap %like% 'Tanzania') cap <- 'Tanzania'
  
  #build either a scatterplot or a contoured cloudplot
  if (type=='scatter') {
  
    plot <- ggplot(dt, aes(x=1-hap_pct, y=tap_paf, color=year %>% as.factor, group=ADM2_CODE)) + 
      geom_point(size=4, alpha=.3) + 
      geom_vline(xintercept=.5, linetype="dashed") +
      scale_colour_manual('', values=c('2000'='steelblue', '2018'='midnightblue'), guide=F)

  } else if (type=='cloud') {

    #Egypt needs to be noised slightly in 2018 because hap is so universally low that it cannot be plotted as a contour
    dt[, noise := runif(.N, min=0, max=0.025)]
    dt[iso3=='EGY' & year==2018, hap_pct := hap_pct + noise]

    plot <- ggplot(dt, aes(x=1-hap_pct, y=tap_paf)) + 
      geom_density_2d(data=dt[year==2000], color='steelblue', binwidth=5) +
      #
      geom_density_2d(data=dt[year==2018], color='midnightblue', binwidth=5) 
    
  }
  
  #standard settings
  plot <- plot +
    geom_vline(xintercept=.5, linetype="dashed") +
    scale_x_continuous("", limits=c(-.05, 1.05), labels = scales::percent, breaks=c(.25, .75), expand = c(0, 0)) +
    scale_y_continuous("", limits=c(.15,.6), labels = scales::percent, breaks=c(.3, .5), expand = c(0, 0),
                       position='right') +
    ggtitle(cap) + #if you want the title on the top
    theme_bw(base_size=16) +
    theme(plot.title=element_text(hjust=0.55, size=12, margin=margin(0,0,0,0)),
          axis.title=element_blank())
    
  if (!scale_labels | loc %in% c('AFG', 'AGO', 'COD', 'EGY', 'MMR', 'NER')) {
    
    plot <- plot + theme(axis.text=element_blank(), 
                         axis.ticks=element_blank(),
                         plot.margin=margin(2, 4, 12, 8, "pt"))
    
  } else if (loc %in% c('BGD', 'IDN', 'NGA')) {
    
    plot <- plot + theme(axis.text.x=element_blank(), 
                         axis.ticks.x=element_blank(),
                         axis.text.y.right=element_text(angle = -90, vjust=0, hjust=.4),
                         plot.margin=margin(2, 0, 12, 0, "pt"))
    
  } else if (loc %in% c('PAK', 'PHL')) {
    
    plot <- plot + theme(axis.text.y=element_blank(), 
                         axis.ticks.y=element_blank(),
                         plot.margin=margin(0, 4, 0, 8, "pt"))
    
  } else if (loc == 'TZA') {
    
    plot <- plot + theme(axis.text.y.right=element_text(angle = -90, vjust=0, hjust=.4),
                         plot.margin=margin(0, 0, 0, 0, "pt"))
    
  }
  
  return(plot)

}

#now make a single inset plot for each of the remaining top 10 countries
insets <- top_countries_c[5:14] %>% 
  .[!(. %in% custom_countries)] %>% 
  sort %>% 
  lapply(., makeInset, scale_labels=T)

##scatter version##
#arrange into master figure
all_grobs <- copy(insets)
all_grobs[[9]] <- master_plot
lay <- rbind(c(9,9,9,9,1,2),
             c(9,9,9,9,3,4),
             c(9,9,9,9,5,6),
             c(9,9,9,9,7,8))
plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
                    top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
                                 gp = gpar(fontsize=24)),
                    bottom=textGrob("Percent of TAP contributed by ambient sources", 
                                    gp = gpar(fontsize=17))
                    ) %>% 
  grid.arrange

ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2.png'),
       width=12, height=8, units='in', dpi=900)

##cloud version##
#now make a single inset plot for each of the remaining top 10 countries
insets <- top_countries_c[5:14] %>% 
  .[!(. %in% custom_countries)] %>% 
  sort %>% 
  lapply(., makeInset, scale_labels=T, type='cloud')

#arrange into master figure
all_grobs <- copy(insets)
all_grobs[[9]] <- master_plot
lay <- rbind(c(9,9,9,9,1,2),
             c(9,9,9,9,3,4),
             c(9,9,9,9,5,6),
             c(9,9,9,9,7,8))
plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
                    top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
                                 gp = gpar(fontsize=24)),
                    bottom=textGrob("Percent of TAP contributed by ambient sources", 
                                    gp = gpar(fontsize=17))
) %>% 
  grid.arrange

ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2_cloud.png'),
       width=12, height=8, units='in', dpi=900)

##cloud version with top 14##
#now make a single inset plot for each of the remaining top 14 countries
insets <- top_countries_c %>% 
  .[!(. %in% custom_countries)] %>% 
  sort %>% 
  lapply(., makeInset, scale_labels=T, type='cloud')

#arrange into master figure
all_grobs <- copy(insets)
all_grobs[[13]] <- master_plot
lay <- rbind(c(13,13,13,13,1,2,3),
             c(13,13,13,13,4,5,6),
             c(13,13,13,13,7,8,9),
             c(13,13,13,13,10,11,12))
plot <- arrangeGrob(grobs=all_grobs, layout_matrix=lay, 
                    top=textGrob("Epidemiological transition of air pollution from 2000 to 2018", 
                                 gp = gpar(fontsize=24)),
                    bottom=textGrob("Percent of TAP contributed by ambient sources", 
                                    gp = gpar(fontsize=17))
) %>% 
  grid.arrange

ggsave(plot=plot, filename=file.path(out.dir, 'presub_fig2_cloud_14.png'),
       width=12, height=8, units='in', dpi=900)


#make for all countries
makeFigure2 <- function(country) {
  
  message('plotting ', country)
  
  plot <- 
    ggplot(plot.dt[iso3 %in% country], 
           aes(x=hap_pct, y=tap_paf, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
    geom_point() + 
    geom_line(alpha=.1) +
    geom_vline(xintercept=.5) +
    scale_color_brewer('Country', palette='Dark2') +
    scale_shape_manual('Year', values=c(1, 16)) +
    scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
    scale_y_continuous("Population Attributable Fraction of LRI to TAP", limits=c(0,1)) +
    ggtitle(country) +
    coord_flip() +
    theme_bw(base_size = 16) +
    theme(
      legend.position = c(.10, .90),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  
  print(plot)
  
  return(NULL)
  
}

pdf(paste0(out.dir, '/presub_figure_2_all_countries.pdf'),
    height=8, width=12)

  lapply(unique(plot.dt$iso3), makeFigure2)
  
dev.off()

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
pm_values <- c(seq(0, 10, length.out = 2), seq(10, 100, length.out = 8), seq(100, 700, length.out = 2)) %>%
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
  plot_map(data$admin2, this_var='dfu',
           annotations, limits=c(0, 1), title='DFU% in 2017', 
           legend_color_values=color_values,
           legend_title='DFU %',
           #zoom=zoom.global,
           debug=F)

ggsave(filename=file.path(out.dir, 'global_dfu.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data$admin2, this_var='tap_pc',
           annotations, limits=c(0, 700), title='Annual per-capita TAP dose in 2017', 
           legend_color_values=pm_values,
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
           annotations, limits=c(-.2, .2), title='DFU% Change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='dfu%', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_dfu_d.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='dfu_dr',
           annotations, limits=c(-.2, .2), title='DFU% Change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='dfu%', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_dfu_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_paf_d',
           annotations, limits=c(-.2, .2), title='PAF Change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='PAF', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_paf_d.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_paf_dr',
           annotations, limits=c(-.2, .2), title='PAF Change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=d_values,
           legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='PAF', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_paf_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='hap_pct_d',
           annotations, limits=c(-1, 1), title='HAP Share Change from 2000-2017', 
           # legend_colors=d_colors, legend_color_values=d_values,
           # legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='HAP/TAP', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_hap_pct_d.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='hap_pct_dr',
           annotations, limits=c(-1, 1), title='HAP Share Change from 2000-2017', 
           # legend_colors=d_colors, legend_color_values=d_values,
           # legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='HAP/TAP', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_hap_pct_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_pc_dr',
           annotations, limits=c(-.6, .3), title='Annual Per-Capita TAP Dose, change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=dr_values,
           # legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
           legend_title='% change in PM2.5 ug/m3', 
           subset=list(var='year', value=2017),
           debug=F)

ggsave(filename=file.path(out.dir, 'global_tap_pc_dr.png'), plot=global, 
       width=12, height=8, units='in', dpi=300)

global <-
  plot_map(data_d$admin2, this_var='tap_paf_dr',
           annotations, limits=c(-.6, .3), title='PAF of LRI attributable to TAP, change from 2000-2017', 
           legend_colors=d_colors, legend_color_values=dr_values,
           legend_title='% change in PAF', 
           subset=list(var='year', value=2017),
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

tic('plotting 2017')
gg_2017 <- list(
  'hap_pct'=plot_map(data_2017$hap_pct$admin2, annotations, limits=c(0, 1), title='2017', 
                     legend_colors=colors, legend_color_values=color_values,
                     legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
                     legend_title='hap/tap', custom_scale=T,
                     pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                     zoom=zoom.global,
                     debug=F),
  # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu'=plot_map(data_2017$dfu$admin2, annotations, limits=c(0, 1), title='2017', 
                 legend_colors=colors, legend_color_values=color_values,
                 legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
                 legend_title='dfu %', custom_scale=T,
                 pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                 zoom=zoom.global,
                 debug=F),
  'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, .75), title='2017', 
                     legend_colors=colors, legend_color_values=paf_values,
                     legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
                     legend_title='tap PAF', custom_scale=T,
                     pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
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
                     pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                     zoom=zoom.global,
                     debug=F),
  # 'tap_pc'=plot_map(data_2000$tap_pc$admin2, annotations, limits=c(0, 1), title='2000', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu'=plot_map(data_2000$dfu$admin2, annotations, limits=c(0, 1), title='2000', 
                 legend_colors=colors, legend_color_values=color_values,
                 legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
                 legend_title='dfu %', custom_scale=T,
                 pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                 zoom=zoom.global,
                 debug=F),
  'tap_paf'=plot_map(data_2000$tap_paf$admin2, annotations, limits=c(0, .75), title='2000', 
                     legend_colors=colors, legend_color_values=paf_values,
                     legend_breaks=seq(0, .75, .1), legend_labels=seq(0, .75, .1),
                     legend_title='tap PAF', custom_scale=T,
                     pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
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
                       pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                       zoom=zoom.global,
                       debug=F),
  # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu_d'=plot_map(data_d$dfu_d$admin2, annotations, limits=c(-.2, .2), title='change from 2000-2017', 
                   legend_colors=d_colors, legend_color_values=d_values,
                   legend_breaks=seq(-.2, .2, .025), legend_labels=seq(-.2, .2, .025),
                   legend_title='dfu %', custom_scale=T,
                   pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                   zoom=zoom.global,
                   debug=F)
  # 'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, 1), title='change from 2000-2017', 
  #                    legend_colors=colors, legend_color_values=paf_values,
  #                    legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #                    legend_title='tap PAF', custom_scale=T,
  #                    pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
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
                        pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                        zoom=zoom.global,
                        debug=F),
  # 'tap_pc'=plot_map(data_2017$tap_pc$admin2, annotations, limits=c(0, 1), title='2017', 
  #          legend_colors=colors, legend_color_values=color_values,
  #          legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #          legend_title='tap PM2.5 per capita', custom_scale=T,
  #          pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
  #          zoom=zoom.global,
  #          debug=F),
  'dfu_dr'=plot_map(data_d$dfu_dr$admin2, annotations, limits=c(-1, 1), title='change rate from 2000-2017', 
                    legend_colors=d_colors, legend_color_values=dr_values,
                    legend_breaks=seq(-1, 1, .2), legend_labels=seq(-1, 1, .2),
                    legend_title='dfu %', custom_scale=T,
                    pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
                    zoom=zoom.global,
                    debug=F)
  # 'tap_paf'=plot_map(data_2017$tap_paf$admin2, annotations, limits=c(0, 1), title='change from 2000-2017', 
  #                    legend_colors=colors, legend_color_values=paf_values,
  #                    legend_breaks=seq(0, 1, .1), legend_labels=seq(0, 1, .1),
  #                    legend_title='tap PAF', custom_scale=T,
  #                    pop_mask=F, lake_mask=T, stage3_mask=T, borders=T,
  #                    zoom=zoom.global,
  #                    debug=F)
)
toc()

tic('ggsaving 2017')
ggsave(filename=file.path(out.dir, 'hap_pct_2017.png'), plot=gg_2017$hap_pct, 
       width=10, height=6, units='in', dpi=200)

ggsave(filename=file.path(out.dir, 'dfu_2017.png'), plot=gg_2017$dfu, 
       width=10, height=6, units='in', dpi=200)

ggsave(filename=file.path(out.dir, 'tap_paf_2017.png'), plot=gg_2017$tap_paf, 
       width=10, height=6, units='in', dpi=200)
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

plot <- 
  ggplot(plot.dt[iso3 %in% custom_countries], 
         aes(x=hap_pct, y=tap_paf*lri*1e3, color=iso3, shape=year %>% as.factor, group=ADM2_CODE)) + 
  geom_point() + 
  geom_line(alpha=.1) +
  geom_vline(xintercept=.5) +
  scale_color_brewer('Country', palette='Dark2') +
  scale_shape_manual('Year', values=c(1, 16)) +
  scale_x_continuous("HAP / TAP Share", limits=c(0, 1)) +
  scale_y_continuous("Rate/1000 of LRI Attributable to TAP", limits=c(0,4)) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(.90, .30),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename=file.path(out.dir, 'presub_figure_2a.png'),
       width=16, height=8, units='in', dpi=900)

master_plot_flip <- 
  ggplot(plot.dt[iso3 %in% custom_countries], 
         aes(x=tap_paf, y=1-hap_pct, color=location_name, shape=year %>% as.factor, group=ADM2_CODE)) + 
  geom_point() + 
  geom_line(alpha=.1) +
  geom_hline(yintercept=.5, linetype="dashed") +
  #scale_color_brewer('Country', palette='Dark2') +
  scale_colour_manual('Country', values=custom_cols) +
  scale_shape_manual('Year', values=c(1, 16)) +
  scale_y_continuous("HAP / TAP Share", limits=c(0, 1), expand = c(0, 0)) +
  scale_x_continuous("Population Attributable Fraction of LRI", limits=c(.2,.6), expand = c(0, 0)) +
  annotate("rect", xmin = .5, xmax = 1, ymin = .2, ymax = .6, fill='lightsalmon2', alpha = .2) +
  annotate("rect", xmin = 0, xmax = .5, ymin = .2, ymax = .6, fill='lightgoldenrod2', alpha = .2) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    #legend.position = c(.84, .69),
    legend.position = c(.82, .41),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename=file.path(out.dir, 'presub_figure_2b.png'),
       width=12, height=8, units='in', dpi=900)

#with vertical layout
lay <- rbind(c(1,2,3,4),
             c(9,9,9,5),
             c(9,9,9,6),
             c(9,9,9,7),
             c(9,9,9,8))
plot <- grid.arrange(grobs=insets, layout_matrix=lay)
plot <- grid.arrange(arrangeGrob(grobs=insets, layout_matrix=lay, 
                                 bottom=textGrob("Percent of TAP contributed by ambient sources")))
ggsave(plot=plot, filename=file.path(out.dir, 'presub_figure_2_master.png'),
       width=12, height=8, units='in', dpi=900)
