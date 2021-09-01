#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot","beepr",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","arm","performance")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RData")
data_list = lapply(files, load, .GlobalEnv)

socio <- readRDS(file="RLS_socio_withoutNA.rds")
env   <- readRDS(file="RLS_env_spatio_temporal.rds")
mpa   <- readRDS(file="RLS_mpa.rds")
colnames(mpa)[1] <- "SurveyID"

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

# #Calculating productivity
RLS_prod_all = calc_prod(data_final)

RLS_prod_all = RLS_prod_all %>%
  group_by(SurveyID) %>%
  filter(sum(Biom) < 10000)

save(RLS_prod_all, file = "outputs/RLS_prod_all.RData")

RLS_prod = calc_prod_transect(RLS_prod_all,info)
save(RLS_prod, file = "outputs/RLS_prod.RData")

#Prepping covariates
RLS_Covariates = data_covariates(RLS_prod,env,socio,mpa)
save(RLS_Covariates,file="outputs/RLS_Covariates.Rdata")

#Protection classes
RLS_Management = data_management(RLS_Covariates,0.95,0.25,0.75,0.25)
save(RLS_Management,file="outputs/RLS_Management.RData")

#Modelling
model_test = test_model(RLS_Management)
save(model_test, file ="outputs/model_test.RData")






