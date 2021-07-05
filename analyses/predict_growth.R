#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","see")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RData|.Rdata")
data_list = lapply(files, load, .GlobalEnv)

socio <- readRDS(file="RLS_socio_withoutNA.rds")
env   <- readRDS(file="RLS_env_spatio_temporal.rds")
mpa   <- readRDS(file="RLS_mpa.rds")
colnames(mpa)[1] <- "SurveyID"

SST <- env[,c("SurveyID","mean_sst_5year")]

#Prepping data
data_fish_Temp <- merge(fish,SST,by="SurveyID",all.x=T)
data_fish_Temp <- left_join(data_fish_Temp,coef,by="TAXONOMIC_NAME") 
data_fish_Temp <- data_fish_Temp %>%  dplyr::rename(lwa="a",lwb="b",Temperature="mean_sst_5year",Family="FAMILY")
data_fish_Temp$Genus  <-  word(data_fish_Temp$TAXONOMIC_NAME, 1)
data_fish_Temp <- data_fish_Temp[,-c(8:12)]
colnames(data_fish_Temp)[3] <- "Species"

#Remove some species without any info
data_fish_Temp <- na.omit(data_fish_Temp) %>% mutate(Species = gsub(" ", "_", Species))

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#---------------Test models and predict K and Linf----------

setwd(here())

#Prepping data for analysis
data_prepped = data_prep(growth_data)
save(data_prepped, file ="outputs/data_prepped.RData")

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
data_merged = merge_growth(data_prepped,data_fish_Temp,gen_model_K)

save(data_merged,file="outputs/data_merged.Rdata")


load("outputs/data_merged.Rdata")

data_traits <- traits %>% 
  dplyr::select(CURRENT_SPECIES_NAME,MaxLength) %>% 
  rename(Species = "CURRENT_SPECIES_NAME") %>% 
  mutate(Species = gsub(" ", "_", Species)) %>%
  right_join(data_merged, by = "Species") %>%
  mutate(InvTkb = 1/((Temperature+273.5)*8.62e-05)) %>%
  arrange(SurveyID)


#Add Max and Lmax from RLS
data_traits$Mmax = data_traits$lwa*data_traits$MaxLength*data_traits$lwb
data_traits$logMmax  = log(data_traits$Mmax)
data_traits$logLmax  = log(data_traits$MaxLength)

data = data_traits %>%
  #Creating four size classes according to fish mean size
  mutate(SizeClass = ifelse(Sizeclass < 20,1,ifelse(Sizeclass >= 20 & Sizeclass < 40, 2,
                                                    ifelse(Sizeclass >= 40 & Sizeclass < 80, 3, ifelse(Sizeclass >= 80, 4,NA)))))%>%
  
  #Adding column with cutting distance
  mutate(CutDist = ifelse(Sizeclass < 20,5,ifelse(Sizeclass >= 20 & Sizeclass < 40, 6,
                                                  ifelse(Sizeclass >= 40 & Sizeclass < 80, 8, ifelse(Sizeclass >= 80, 10,NA)))))%>%
  
  #Calculating Area for each transect 
  mutate(Area=50*5)

#Merging with traits 
# data_merge = left_join(data_cut,Traits_vie,by="Species")
# 
# data <- merge(data,DietRLS,by="Species",all.x=T)

data_K = predict_K(data,gen_model_K,fam_model_K,fish_model_K)
data_final = predict_Linf(data_K,fam_model_Linf,gen_model_Linf,fish_model_Linf)
