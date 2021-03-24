#' Run the Entire Project
#'
#' This script reproduces all analyses and figures of the ___________ article.
#'
#' @author Raphaël SEGUIN, \email{raphael.seguin46@@gmail.com},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'
#' @date 2021/02/17
#' 

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","harrypotter","wesanderson","dichromat")
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

#----------------Run code------------------------

setwd(here())

growth_data_prep = data_prep(growth_data) %>%
  na.omit()%>%
  filter(Family !="Apogonidae") %>%
  filter(!(Family == "Gobiidae" & K == 2.482)) %>%
  filter(!(Family == "Pomacentridae" & K == 4)) %>%
  filter(!(Species == "Salarias patzneri"))%>%
  dplyr::mutate(Species = stringr::str_replace(Species, " ", "_"))

data_prod = data_prod %>%
  dplyr::rename(Size="Taille")%>%
  mutate(sst = sst+273.5)

#Testing model performances at predicting growth rates K based on mean adjusted R squared over 100 iterations

#We should keep Diet here
fam_perf = K_fam_perf(growth_data_prep)
fam_diet_perf = K_fam_diet_perf(growth_data_prep)

#We should keep Diet here
gen_perf = K_gen_perf(growth_data_prep)
gen_diet_perf = K_gen_diet_perf(growth_data_prep)

#Getting some plots
plot_fam_perf(growth_data_prep)
plot_gen_perf(growth_data_prep)

#Testing Linf predictions model performance
Linf_fam_perf =   Linf_fam_perf(growth_Linf)
Linf_gen_perf = Linf_gen_perf(growth_Linf)

#Saving models
fam_model_K = save_fam_model_K(growth_data_prep)
gen_model_K = save_gen_model_K(growth_data_prep)
fish_model_K = save_fish_model_K(growth_data_prep)
fam_model_Linf = save_fam_model_Linf(growth_Linf)
gen_model_Linf = save_gen_model_Linf(growth_Linf)
fish_model_Linf = save_fish_model_Linf(growth_Linf)

#Predicting K and Linf
data_merged = merge_growth(growth_data_prep,data_prod,gen_model_K)
data_K = predict_K(data_merged,gen_model_K,fam_model_K,fish_model_K)
data_final = predict_Linf(data_K,fam_model_Linf,gen_model_Linf,fish_model_Linf)

data_final = data_final %>%
  dplyr::rename(Size = "Taille")

#Calculating productivity
NC_prod = calc_prod(data_final,500)

#Pooling by transect
NC_transect = prod_pool(NC_prod)


