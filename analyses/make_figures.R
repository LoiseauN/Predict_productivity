#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","arm")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#----------------Loading results------------------------

path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"),pattern = ".RData")
data_list = lapply(files, load, .GlobalEnv)

#-------------Loading code for plots---------------

path = (here::here("R_figures"))
setwd(path)
files.source = list.files(here::here("R_figures"))
sapply(files.source, source)

#--------------Loading SHP-------------------------

setwd(here())
nc_map = st_read("shp/NewCaledonia_v7.shp")

#-------------Plots--------------------------------

#Getting some plots
plot_fam_perf(data_prepped)
plot_gen_perf(data_prepped)

#Plot K by size
K_by_size(NC_prod)

#Plot map and classes
plot_classes(NC_management)
map_management(NC_management,nc_map)

#Plotting some model stuff
plot_var_imp(model_test)

#Getting plot probabilities
model_prob(NC_covariates)
