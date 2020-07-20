#***********************************************************************************************************************

# ---FIGURE 4-----------------------------------------------------------------------------------------------------------
#helper function to create smaller inset versions of the transition plot
makeInset <- function(i, loclist, 
                      colors=c('2000'='#737373', '2018'='#08519c'),
                      type='scatter', scale_labels=F) {
  
  loc <- loclist[i]
  n <- length(loclist)
  message('plotting ', loc)
  
  dt <- plot.dt[iso3%in%loc]
  cap <- plot.dt[iso3%in%loc, unique(location_name)]
  if(cap %like% 'Congo') cap <- 'D.R. Congo'
  if(cap %like% 'Tanzania') cap <- 'Tanzania'
  
  #build either a scatterplot or a contoured cloudplot
  if (type=='scatter') {
    
    plot <- ggplot(dt, aes(x=1-hap_pct_mean, y=tap_paf_mean, color=year %>% as.factor, group=ADM2_CODE)) + 
      geom_point(size=4, alpha=.3) + 
      geom_vline(xintercept=.5, linetype="dashed") +
      scale_colour_manual('', values=colors, guide=F)
    
  } else if (type %like% 'cloud') {
    
    #Egypt needs to be noised slightly in 2018 because hap is so universally low that it cannot be plotted as a contour
    dt[, noise := runif(.N, min=0, max=0.025)]
    dt[iso3=='EGY' & year==2018, hap_pct_mean := hap_pct_mean + noise]
    
    if (type=='cloud_contour') {
      
      plot <- ggplot(dt, aes(x=1-hap_pct_mean, y=tap_paf_mean)) + 
        geom_density_2d(data=dt[year==2000], color=colors[[1]], binwidth=5) +
        geom_density_2d(data=dt[year==2018], color=colors[[2]], binwidth=5)
      
    } else if (type=='cloud_alpha') {
      
      plot <- ggplot(dt, aes(x=1-hap_pct_mean, y=tap_paf_mean)) + 
        stat_density_2d(data=dt[year==2000], aes(alpha = after_stat(level)), geom = "polygon", fill=colors[[1]], contour=TRUE, binwidth=20) +
        stat_density_2d(data=dt[year==2018], aes(alpha = after_stat(level)), geom = "polygon", fill=colors[[2]], contour=TRUE, binwidth=20) +
        scale_alpha(guide=F, range=c(.1, 0.01))
      
    }
    
  }
  
  #standard settings
  plot <- plot +
    geom_vline(xintercept=.5, linetype="dashed") +
    scale_x_continuous("", limits=c(-.05, 1.05), labels = scales::percent, breaks=c(.25, .75), expand = c(0, 0)) +
    scale_y_continuous("", limits=c(.05,.61), labels = scales::percent, breaks=c(.3, .5), expand = c(0, 0),
                       position='right') +
    ggtitle(cap) + #if you want the title on the top
    theme_bw(base_size=16) +
    theme(plot.title=element_text(hjust=0.55, size=12, margin=margin(0,0,0,0)),
          axis.title=element_blank())
  
  #remove the labels for the top left insets or if there are no scale labels requested
  if (!scale_labels | (n==9 & i %in% c(1, 3, 5)) | (n==12 & i %in% c(1:2, 4:5, 7:8))) {
    
    plot <- plot + theme(axis.text=element_blank(), 
                         axis.ticks=element_blank(),
                         plot.margin=margin(2, 4, 12, 8, "pt"))
    
    #remove just the x labels for right insets
  } else if ((n==9 & i %in% c(2,4,6)) | (n==12 & i %in% c(3,6,9))) {
    
    plot <- plot + theme(axis.text.x=element_blank(), 
                         axis.ticks.x=element_blank(),
                         axis.text.y.right=element_text(angle = -90, vjust=0, hjust=.4),
                         plot.margin=margin(2, 0, 12, 0, "pt"))
    
    #remove just th y labels for the bottom left insets  
  } else if ((n-i) %in% (1:2)) {
    
    plot <- plot + theme(axis.text.y=element_blank(), 
                         axis.ticks.y=element_blank(),
                         plot.margin=margin(0, 4, 0, 8, "pt"))
    
    #keep all labels for the bottom right insets  
  } else if (i==n) {
    
    plot <- plot + theme(axis.text.y.right=element_text(angle = -90, vjust=0, hjust=.4),
                         plot.margin=margin(0, 0, 0, 0, "pt"))
    
  }
  
  return(plot)
  
}

#***********************************************************************************************************************

# ---FIGURE 3-----------------------------------------------------------------------------------------------------------
#helper function to give us the intervals for binwidth after cutting
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  chr <- as.character(x)
  
  start <- as.numeric(gsub(pattern,"\\2", chr))
  end <- as.numeric(gsub(pattern,"\\3", chr))
  
  list(start, end)
}

#helper function to create a weighted density plot
makeTapDensityPlot <- function(
  input_dt, wt_var='pop_total', #which variables to plot and dataset
  locs, #list of iso3 to plot separately
  start_year=2000, end_year=2018, #time variables
  tap_cutoff, #where to cut off the y axis
  sequence, #what bin sequence to use
  smoother=.6, #what span to use for the smoother
  nudge=1e-5, #how much to nudge values in order to work in logspace (prevent NaNs for 0)
  plot_quartiles=F, #if wanting to display quartiles as dotted lines
  debug=F
) {
  
  if(debug) browser()
  
  #prepare input data
  dt <- input_dt[type!='TAP'] %>% copy #we will recalculate TAP later
  dt[iso3 %in% locs, loc := ADM0_NAME]
  dt[is.na(loc), loc := "All other LMICs"]
  dt[loc=='Democratic Republic of the Congo', loc := 'D.R. Congo']

  #create data.table from the sequence
  seq.dt <- data.table(tap_bin_id=cut(sequence, sequence, include.lowest = TRUE, labels=F))
  seq.dt[, c('start', 'end') := cut(sequence, sequence, include.lowest = TRUE, dig.lab=4) %>% cut_borders]
  seq.dt[1, `:=` (start=1e-5, end=sequence[1], tap_bin_id=0)]
  
  #expand the dt to include all relevant vars
  skeleton <- expand.grid(year=c(start_year, end_year),
                          type=c('AAP', 'HAP'),
                          loc=unique(dt$loc),
                          stringsAsFactors = F)
  seq.dt <- tidyr::crossing(seq.dt, skeleton) %>% as.data.table
  
  #merge TAP values onto the nearest bin
  dt[, tap_pm := sum(pm_pc_mean), by=.(year, ADM2_CODE)] #first, recalculate TAP
  dt[, tap_bin := cut(tap_pm, sequence, include.lowest = TRUE, dig.lab=4), by=.(loc, year)]
  dt[, tap_bin_id := cut(tap_pm, sequence, include.lowest = TRUE, labels=F), by=.(loc, year)]
  
  #generate sums
  dt[, sum_var := get(wt_var)]
  dt[, sum := sum(sum_var, na.rm=T), by=.(type, year)]
  #split tap by share & collapse to bin/types
  dt[, bin_sum := sum(sum_var*share_mean, na.rm=T), by=.(loc, year, type, tap_bin_id)]

  #split into types
  # dt[, bin_sum ]
  # dt <- tidyr::crossing(dt, data.table(source=factor(c('aap','hap')))) %>% as.data.table
  # dt[source=='hap', bin_sum := sum(sum_var*hap_pct_mean, na.rm=T), by=.(loc, year, tap_bin_id)]
  # dt[source=='aap', bin_sum := sum(sum_var*(1-hap_pct_mean), na.rm=T), by=.(loc, year, tap_bin_id)]
  
  #collapse
  bin.dt <- unique(dt, by=c('loc', 'type', 'year', 'tap_bin')) %>% .[(order(type, year, tap_bin_id))]
  
  #add on the missing bins
  bin.dt <- merge(bin.dt, seq.dt, by=c('loc', 'type', 'year', 'tap_bin_id'), all.y=T, allow.cartesian=T)
  bin.dt[, sum := zoo::na.aggregate(sum), by=.(type, year)]
  bin.dt[is.na(bin_sum), bin_sum := 0]
  
  #calculate density
  bin.dt[, bin_width := end-start]
  bin.dt[, prob := bin_sum/sum]
  bin.dt[, density := prob/bin_width]
  bin.dt[, mid := start+((end-start)/2)]
  
  #make smoothed predictions
  bin.dt[, smooth := loess(log(density+nudge) ~ mid, span=smoother) %>% 
           predict(.SD) %>% 
           exp, 
         by=.(type, loc, year)]
  
  #calculate cumulative density across sample
  stats <- copy(bin.dt) %>% setkey(., mid, year)
  stats[, sum := sum(prob, na.rm=T), by=key(stats)]
  stats <- stats %>% .[, .(sum, mid, year)] %>% unique(., by=key(.))
  stats[, cum := cumsum(sum), by = year]
  
  #define stats to print
  quantiles <- expand.grid(year=c(start_year, end_year),
                            cum=c(.2, 0.25, .4, .5, .6, .75, .8)) %>% as.data.table
  
  quantiles <- stats[quantiles, on=.(year, cum), roll='nearest'] 
  print(quantiles)

  #melt years to make a new variable
  bin.dt <- bin.dt[, .(loc, type, year, mid, smooth)] #drop everything else that varies by year and cleanup dt
  bin.dt[, year := paste0('smooth_', year)]
  bin.dt <- dcast(bin.dt, ...~year, value.var='smooth')
  
  #make color palette
  bin.dt[, loc := factor(loc)]
  if(uniqueN(bin.dt$loc)>2) loc_colors <- brewer.pal(uniqueN(bin.dt$loc)-1, "Paired")
  else loc_colors <- c('#a6cee3', '#fdbf6f')
  loc_colors <- c('#bdbdbd', loc_colors) #force other to grey

  #make plot
  plot <-
    ggplot(data=bin.dt[type!='TAP'], 
           aes(x=mid, y=smooth_2018, fill=loc, color=loc, alpha=type)) +
    #endyear plots
    geom_area() +
    #start year plots
    geom_area(aes(y=smooth_2000*-1)) + #invert for the mirrored comparison
    #thresholds
    geom_vline(xintercept=10, linetype='dashed', color='dodgerblue2') +
    geom_hline(yintercept=0, color='gray10', size=1) +
    #scales
    scale_x_sqrt('', limits=c(0+nudge, 750),
                 breaks=c(10, 25, 50, 100, 200, 400, 750)) +
    scale_color_manual('Country', values=loc_colors) +
    scale_fill_manual('Country', values=loc_colors) +
    scale_alpha_manual(values=c('AAP'=.4, 'HAP'=1), guide=F) +
    theme_minimal() +
    labs(title = '') +
    #annotations
    #theme
    theme(axis.title=element_blank(),
          axis.text.y=element_blank(),
          text = element_text(size=16),
          legend.position = c(.01, .99),
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin = margin(6, 6, 6, 6),
          plot.margin = margin(0, 0, 0, 0, "pt")
          )
  
  if(plot_quartiles) {
    
    plot <- plot +     #quartiles
    geom_segment(aes(y=0, yend=.01, x=quartiles[year==2018 & cum==.25, mid],
                     xend=quartiles[year==2018 & cum==.25, mid]), linetype='dotted', color='gray10') +
    geom_segment(aes(y=0, yend=.01, x=quartiles[year==2018 & cum==.5, mid],
                     xend=quartiles[year==2018 & cum==.5, mid]), linetype='dotted', color='gray10') +
    geom_segment(aes(y=0, yend=.01, x=quartiles[year==2018 & cum==.75, mid],
                     xend=quartiles[year==2018 & cum==.75, mid]), linetype='dotted', color='gray10') +
    geom_segment(aes(y=0, yend=-.01, x=quartiles[year==2000 & cum==.25, mid],
                     xend=quartiles[year==2000 & cum==.25, mid]), linetype='dotted', color='gray10') +
    geom_segment(aes(y=0, yend=-.01, x=quartiles[year==2000 & cum==.5, mid],
                     xend=quartiles[year==2000 & cum==.5, mid]), linetype='dotted', color='gray10') +
    geom_segment(aes(y=0, yend=-.01, x=quartiles[year==2000 & cum==.75, mid],
                     xend=quartiles[year==2000 & cum==.75, mid]), linetype='dotted', color='gray10')
    
  }
  
  return(plot)
  
}
#***********************************************************************************************************************

# ---FIGURE ?-----------------------------------------------------------------------------------------------------------