#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot","rfishbase","beepr",
          "rnaturalearth","plotly","ggridges","ggbeeswarm","ggforce","rfishbase","parameters","NbClust","cluster","klaR","beepr","gstat",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","arm","performance","see")
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
files <- list.files(here::here("outputs"),pattern = ".Rdata")
data_list = lapply(files, load, .GlobalEnv)

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

path = (here::here("R_figures"))
setwd(path)
files.source = list.files(here::here("R_figures"))
sapply(files.source, source)

#----------------Run project------------------------

path = (here::here("analyses"))
setwd(path)
files.source = list.files(here::here("analyses"))
sapply(files.source, source)

setwd(here())

#Testing growth rate models and predicting them on RLS data base
predict_growth()

#Calculating productivity on RLS database, selecting transects with covariates, creating management classes and modelling them
predict_productivity()

#Reloading outptus for plots
path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"),pattern = ".Rdata|.RData")
data_list = lapply(files, load, .GlobalEnv)

setwd(here())


#Making all the figures for the paper
make_figures()


