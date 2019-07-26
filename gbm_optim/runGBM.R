# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/25/2019
# Purpose: Optimize BRT
# source("/homes/jfrostad/_code/lbd/hap/gbm_optim/runGBM.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

#running interactively?
debug <- T
if (debug) warning('debug is set to TRUE - if you did not mean to run interactively then kill job and turn off debug')
debug.args <- c('simulate',
                'command',
                'args',
                'weird',
                'shell?',
                'cooking_fuel_solid_dia_sssa',
                'gp',
                'brt',
                6,
                'test7_2019-07-24',
                'cooking_fuel_solid',
                3,
                0.5,
                F)

#pull args from the job submission if !interactive
args <- ifelse(rep(debug, length(debug.args)), 
               debug.args, 
               commandArgs()) 


jobnum <- args[6]
opt_type <- args[7]
lrnr_type <- args[8]
bounds_version <- args[9]
experiment_version <- args[10]
indicator <- args[11]
cv_fold <- as.numeric(args[12])
bag_fraction <- as.numeric(args[13])
file_addin <- args[14]
train_fraction <- 1-(1/cv_fold)
experiment_num <- 1

message(commandArgs())

#get pkgs
library(seegSDM, lib.loc = '/share/code/geospatial/adesh/r_packages_hf_sing/')
pacman::p_load(data.table, gbm, magrittr, tictoc)


#setup dirs
repo  <- file.path('/homes', Sys.info()['user'], '_code/lbd/hap')
code_dir <- file.path(repo, 'gbm_optim')
in_dir <- '/share/scratch/tmp/jfrostad/output/gbm_optim/'
out_dir <- file.path(in_dir, lrnr_type, experiment_version)

#makedirs
dir.create(out_dir, recursive = T)
file.path(out_dir, 'stats_output') %>% dir.create
file.path(out_dir, 'model_output') %>% dir.create
file.path(out_dir, 'stats_test_output') %>% dir.create
file.path(out_dir, 'stats_train_output') %>% dir.create

#fx lib
paste0(code_dir, '/calcStats.R') %>% source #custom fx

#################################################################
## CONSTRUCT PYTHON SYSTEM COMMAND TO RUN 'optimizers.py'

run_optimizerPy <- function(funcs.file_path,
                            funcs.file,
                            bounds.file_path,
                            bounds.file,
                            data.file_path,
                            data.loc,
                            optimizer,
                            learner,
                            cv_folds,
                            n_calls,
                            jobnum,
                            col_start,
                            col_response,
                            col_n,
                            bag_frac,
                            exp_num)
{
  message(paste0('/snfs1/temp/jco24/miniconda2/envs/optimizer_centos/bin/python ', 
           funcs.file_path, funcs.file,
           bounds.file_path, bounds.file, 
           data.file_path, data.loc,
           optimizer, 
           learner,
           cv_folds,
           n_calls,
           jobnum,
           col_start,
           col_response,
           col_n,
           bag_frac,
           exp_num))
  system(
    paste0('/snfs1/temp/jco24/miniconda2/envs/optimizer_centos/bin/python ', 
           funcs.file_path, funcs.file,
           bounds.file_path, bounds.file, 
           data.file_path, data.loc,
           optimizer, 
           learner,
           cv_folds,
           n_calls,
           jobnum,
           col_start,
           col_response,
           col_n,
           bag_frac,
           exp_num)
  )
}
#################################################################
tic('starting master timer')
# save bounds file for a record
bnds_file <- read.csv(paste0(code_dir, '/space_bounds', bounds_version, '.csv'))
write.csv(bnds_file, paste0(out_dir, '/space_bounds_settings.csv'))

# read in covariates and data
dat_all <- read.csv(paste0(in_dir, 'data/', jobnum, ifelse(file_addin == FALSE, '', paste0('_', file_addin)), '.csv'))

## MAKE .CSV OF OPTIMIZED HPARS

# split to make OoS hold-out data
indx <- sample(nrow(dat_all), 0.80*nrow(dat_all))
data_train <- dat_all[indx,]
data_test <- dat_all[-indx,]
write.csv(data_train, file = paste0(out_dir, '/data_train_', jobnum, '_exp', experiment_num, '.csv'), row.names=F)

# get col names for covs
first_cov_name <- ifelse('access2' %in% names(dat_all), 'access2', 'depratio')
first_cov <- toString(which(names(dat_all) == first_cov_name))
response_col <- toString(which(names(dat_all) == 'rate'))
n_col <- toString(which(names(dat_all) == 'N'))

# run optimization in python via system
tic('running optimizer')
run_optimizerPy(funcs.file_path <- code_dir,
                funcs.file <- '/optimizers.py ',
                bounds.file_path <- code_dir,
                bounds.file <- paste0('/space_bounds', bounds_version, '.csv '),
                data.file_path <- paste0(out_dir, ' '),
                data.loc <- paste0(out_dir, '/data_train_', jobnum, '_exp', experiment_num, '.csv '),
                optimizer <- paste0(opt_type, ' '),
                learner <- paste0(lrnr_type, 'R '),
                cv_folds <- paste0(cv_fold, ' '),
                n_calls <- '150 ',
                jobnum <- paste0(jobnum, ' '),
                col_start <- paste0(first_cov, ' '),
                col_response <- paste0(response_col, ' '),
                col_n <- paste0(n_col, ' '),
                bag_frac <- paste0(bag_fraction, ' '), 
                exp_num <- experiment_num)
toc(log=TRUE)

# delete data file
jobnum <- gsub(' ', '', jobnum)
file.remove(paste0(out_dir, '/data_train_', jobnum, '_exp', experiment_num, '.csv'))


## FORMAT DATA FOR RUNNING BRT MODEL
tic('Formatting data for BRT')
# load optimized hyperparameter values
par <- read.csv(paste0(out_dir, '/best_pars_', jobnum, '_exp', experiment_num, '.csv'))

# format training fit data
data_train <- as.data.table(data_train)
data_train[, response := round(get(indicator), 0)]
data_train[, log_n  := log(N)]
dt_train <- data_train[,first_cov:ncol(data_train)]
data_train[, weights := weight*N]

# format training fit data
data_test <- as.data.table(data_test)
data_test[, response := round(get(indicator), 0)]
data_test[, log_n  := log(N)]
dt_test <- data_test[,first_cov:ncol(data_test)]
data_test[, weights := weight*N]

# format all fit data
dat_all <- as.data.table(dat_all)
dat_all[, response := round(get(indicator), 0)]
dat_all[, log_n := log(N)]
dt_all <- dat_all[,first_cov:ncol(dat_all)]
dat_all[, weights := weight*N]
toc(log=TRUE)


## RUN BRT MODEL
tic('Running BRT on TRAIN data')
# set up the general formula to be used in gbm
cov_names <- paste(names(dt_train[, -c('response', 'log_n')]), collapse = ' + ')
gbm_formula <- formula(paste0('response ~ offset(log_n) + ', cov_names))

# run model fit on the training data
model_train <- gbm(gbm_formula,
                   distribution = 'poisson',
                   data = dt_train,
                   n.trees = par$n.trees, 
                   interaction.depth = par$interaction.depth,
                   shrinkage = par$shrinkage,
                   n.minobsinnode = par$n.minobsinnode,
                   weights = data_train$weights,
                   bag.fraction = bag_fraction,
                   train.fraction = train_fraction,
                   verbose = F)

# get fit statistics for the training data
train.pred <- predict.gbm(model_train,
                          dt_train, 
                          n.trees = model_train$n.trees, 
                          type = 'response')
stats_train <- calcStats(data.frame(data_train$rate, train.pred))

stats_train <- data.frame(as.list(stats_train))
write.csv(stats_train, file = paste0(out_dir,'/stats_train_output/stats_train_', jobnum, '_exp', experiment_num, '.csv'))
toc(log=TRUE)

# run model fit on the testing data
tic('Running BRT on TEST data')
model_test <- gbm(gbm_formula,
                  distribution = 'poisson',
                  data = dt_test, 
                  n.trees = par$n.trees, 
                  interaction.depth = par$interaction.depth,
                  shrinkage = par$shrinkage,
                  n.minobsinnode = par$n.minobsinnode,
                  weights = data_test$weights,
                  bag.fraction = bag_fraction,
                  train.fraction = train_fraction,
                  verbose = F)
toc(log=TRUE)

# get fit statistics for the test data
tic('TEST fit statistics')
test.pred <- predict.gbm(model_test,
                         dt_test, 
                         n.trees = model_test$n.trees, 
                         type = 'response')
stats_test <- calcStats(data.frame(data_test$rate, test.pred))

stats_test <- data.frame(as.list(stats_test))
write.csv(stats_test, file = paste0(out_dir,'/stats_test_output/stats_test_', jobnum, '_exp', experiment_num,'.csv'))
toc(log=TRUE)

# run final model
tic('Running final BRT')
model <- gbm(gbm_formula,
             distribution = 'poisson',
             data = dt_all, 
             n.trees = par$n.trees, 
             interaction.depth = par$interaction.depth,
             shrinkage = par$shrinkage,
             n.minobsinnode = par$n.minobsinnode,
             weights = dat_all$weights,
             bag.fraction = bag_fraction,
             train.fraction = train_fraction,
             verbose = F)
toc(log=TRUE)

## final model results:
# effect curves
tic('Final BRT Results')
effects <- lapply(1:(ncol(dt_all)-2),
                  function(i) {
                    plot(model, i, return.grid = TRUE)
                  })

# get relative influence
relinf <- summary(model, plotit = FALSE)

# get coordinates
coords <- dat_all[, c('latitude', 'longitude')]

# format like seegSDM::runBRT() result
model.list <- list(model = model,
                   effects = effects,
                   relinf = relinf,
                   coords = coords)
save(model.list, file = paste0(out_dir,'/model_output/model_', jobnum, '_exp', experiment_num, '.Rdata'))

# get fit statistics
gbm.pred <- predict.gbm(model,
                        dt_all, 
                        n.trees = model$n.trees, 
                        type = 'response')
stats <- calcStats(data.frame(dat_all$rate, gbm.pred))
stats <- data.frame(as.list(stats))

# save fit statistics and prediction raster
write.csv(stats, file = paste0(out_dir,'/stats_output/stats_', jobnum, '_exp', experiment_num, '.csv'))
toc(log=TRUE)
toc(log=TRUE)