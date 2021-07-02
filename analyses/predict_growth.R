#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","see")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RData")
data_list = lapply(files, load, .GlobalEnv)

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#---------------Test models and predict K and Linf----------

setwd(here())

getwd()

#Prepping data for analysis
data_prepped = data_prep(growth_data)
save(data_prepped, file ="outputs/data_prepped.RData")

#Converting temperature to Kelvin
data_prod = data_prod %>%
  mutate(sst = sst+273.5)

#Testing model performances at predicting growth rates K based on mean adjusted R squared over 100 iterations

#We should keep Diet here
fam_perf = K_fam_perf(data_prepped)
fam_diet_perf = K_fam_diet_perf(data_prepped)

#We should NOT keep Diet here
gen_perf = K_gen_perf(data_prepped)
gen_diet_perf = K_gen_diet_perf(data_prepped)

#Testing Linf predictions model performance
Linf_fam_perf =   Linf_fam_perf(growth_Linf)
Linf_gen_perf = Linf_gen_perf(growth_Linf)

#Saving models
fam_model_K = save_fam_model_K(data_prepped)
gen_model_K = save_gen_model_K(data_prepped)
fish_model_K = save_fish_model_K(data_prepped)
fam_model_Linf = save_fam_model_Linf(growth_Linf)
gen_model_Linf = save_gen_model_Linf(growth_Linf)
fish_model_Linf = save_fish_model_Linf(growth_Linf)

#Predicting K and Linf
data_merged = merge_growth(growth_data_prep,data_prod,gen_model_K)
data_K = predict_K(data_merged,gen_model_K,fam_model_K,fish_model_K)
data_final = predict_Linf(data_K,fam_model_Linf,gen_model_Linf,fish_model_Linf)