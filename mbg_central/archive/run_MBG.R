# Submit job to run space-time geostatistical models using INLA

# load packages
library(INLA, lib.loc = package_lib)
library(raster, lib.loc = package_lib)
library(seegMBG, lib.loc = package_lib)
library(data.table, lib.loc = package_lib)

# ~~~~~~~~
# load data

# spatial and temporal meshes
#load(paste0(template_dir, '/meshes.RData'))
load(paste0(root,"/temp/geospatial/U5M_africa/data/clean/meshes.RData"))

# data
df = fread(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/input_data.csv'))

# covariates
covs <- brick(paste0(cov_dir, '/covs_transformed.grd')) # non-varying

# load other temporal covariates. (Make sure they have been GAMed)
message('Time-varying covariate rasters')
for(c in c('lights_new','evi','LST_day','total_pop','edu_0','edu_mean')) # time-varying
   if(c %in% fixed_effects) assign(paste0('tv_',c),brick(paste0(cov_dir, '/time_varying_covs_transformed_',c,'.grd')))

# make periods for all surveys
i <- length(unique(df$year))
periods <- data.frame(group = rep(1:i,5),years = rep(sort(unique(df$year)),5))
df$period <- match(df$year, periods$years) # add these to df


# ~~~~~~~~~~~~~~~
# sort covariates

# extract cluster covariates
df$longitude<-as.numeric(df$longitude)
df$latitude <-as.numeric(df$latitude )

# remove missing values
df <- na.omit(df)

# extract cluster level covariates
cluster_covs <- as.data.frame(extract(covs, df[,c('longitude', 'latitude'),with=F]))

# add to dataframe
df <- cbind(df, cluster_covs)


# add dummy column for time-varying covariates
tvnames = c()
for(l in ls()[grep('tv_',ls())]){
  df[,substr(l,4,nchar(l))]=NA
  tvnames[length(tvnames)+1]=substr(l,4,nchar(l))
}

# loop through time varying covariates to insert them
for (period in sort(unique(periods$group))) {
  for(l in tvnames){
    
    # find matching rows
    l=paste0('tv_',l)
    message(paste0(l,period))
    idx_tv<- which(df$period == period)
    
    # get raster
    craster  <- get(l)[[period]]
    crs(craster)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # extract values
    extr <- extract(craster, df[idx_tv, c('longitude', 'latitude'),with=F])
    
    # add into df, on gam transformed scale (already in it)
    df[[substr(l,4,nchar(l))]][idx_tv]=extr
    
  }
} 


message('saving image')
cores=30
save.image(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, '_tempimage.RData'))  

message('Submitting to the cluster')
                    qsub = paste0('qsub -cwd -pe multi_slot ',
                    cores,' -e /share/temp/sgeoutput/ngraetz/errors -o /share/temp/sgeoutput/ngraetz/output -P proj_injuries',
                      ' -l hosttype=intel -N newcovs_mbg_',indicator,
                    ' ', repo, '/r_shell.sh ', repo, '/mbg_central/mbg_models/run_MBG_bybin_', indicator_family, '.R ', indicator,
                    ' ', repo, ' ', indicator_group, ' ', run_date)
print(qsub) # for log
system(qsub) # submit job

## END

