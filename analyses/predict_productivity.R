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


data_country = data_prod_brut %>% 
  dplyr::select(SurveyID,Country)

levels(data_prod_management$Effectiveness)  = c("Low","Medium","High","No_Mpa")

data_prod_management$Effectiveness[is.na(data_prod_management$Effectiveness)] = "No_Mpa"

data_prod_management_noNA = data_prod_management %>% 
  left_join(data_country,by="SurveyID") %>% na.omit()

# #Calculating productivity
# NC_prod = calc_prod(data_final,500)
# save(NC_prod, file = "outputs/NC_prod.RData")

# #Pooling by transect
# NC_transect = prod_pool(NC_prod)
# save(NC_transect, file = "outputs/NC_transect.RData")


#Protection classes
RLS_Management = data_management(data_prod_management_noNA,0.95,0.05,0.75,0.25)
save(RLS_Management,file="outputs/RLS_Management.RData")

#Trying it out with RLS data
#Prepping covariates
RLS_Covariates = data_covariates(RLS_Management)
save(RLS_Covariates,file="outputs/RLS_Covariates.Rdata")

#Modelling
model_test = test_model(RLS_Covariates)
save(model_test, file ="outputs/model_test.RData")





