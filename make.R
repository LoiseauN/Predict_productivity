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

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RDS")
growth_data = lapply(files, readRDS) %>% bind_rows()


#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#----------------Run code------------------------

growth_data_prep = data_prep(growth_data)

#Testing model performances at predicting growth rates K based on mean adjusted R squared over 100 iterations
fam_perf = K_fam_perf(growth_data_prep)
fam_diet_perf = K_fam_diet_perf(growth_data_prep)
gen_perf = K_gen_perf(growth_data_prep)
gen_diet_perf = K_gen_diet_perf(growth_data_prep)



