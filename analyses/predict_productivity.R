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

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#--------Run analyses for productivity-----------------------

setwd(here())

data_final = data_prod %>%
  dplyr::rename(Size = "Taille")

#Calculating productivity
NC_prod = calc_prod(data_final,500)
save(NC_prod, file = "outputs/NC_prod.RData")

#Pooling by transect
NC_transect = prod_pool(NC_prod)

#Protection classes
NC_management = data_management(NC_transect)
save(NC_management,file="outputs/NC_management.RData")

#Prepping covariates
NC_covariates = data_covariates(NC_management)
save(NC_covariates,file="outputs/NC_covariates.RData")

#Modelling
model_test = test_model(NC_covariates)
save(model_test, file ="outputs/model_test.RData")
