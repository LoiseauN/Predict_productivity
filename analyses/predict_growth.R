#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot","rfishbase","beepr",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","see")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RData|.Rdata")
data_list = lapply(files, load, .GlobalEnv)

path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"),pattern = ".RData|.Rdata")
data_list = lapply(files, load, .GlobalEnv)

socio <- readRDS(file="RLS_socio_withoutNA.rds")
env   <- readRDS(file="RLS_env_spatio_temporal.rds")
mpa   <- readRDS(file="RLS_mpa.rds")
colnames(mpa)[1] <- "SurveyID"

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#---------------Test models and predict K and Linf-----------

setwd(here())

# #PREPPING NEW DATA
# abcoef = coef %>%
#   dplyr::rename(Species = "SPECIES_NAME") %>%
#   dplyr::select(Species,a,b)
# 
# taxo = traits %>%
#   dplyr::rename(Species="CURRENT_TAXONOMIC_NAME") %>%
#   dplyr::select(Family,Species)
# 
# diet = traits %>%
#   dplyr::rename(Species="CURRENT_TAXONOMIC_NAME") %>%
#   dplyr::select(Species,Trophic.group) %>%
#   dplyr::rename(Diet = "Trophic.group")
# 
# Valefin = Vale %>% 
#   filter(source == "Reef_services") %>% 
#   left_join(abcoef,by="Species") %>%
#   dplyr::select(-c(lat,lon,Family)) %>%
#   left_join(taxo,by="Species") %>%
#   mutate(Genus = word(Species,1)) %>%
#   dplyr::rename(MaxSize="sizemax",
#                 K = "kmax") %>%
#   dplyr::select(Family,Genus,Species,K,MaxSize,sstmean,a,b)

#Prepping data, removing some errors 
growth_data = Morais %>% 
  dplyr::select(Family,Species,MaxSizeTL,Diet,a,b,Linf,K,sstmean) %>%
  rename(MaxSize = "MaxSizeTL") %>%
  mutate(Genus = word(Species,1)) %>%
  dplyr::select(-Linf) %>%
  dplyr::select(Family,Genus,Species,Diet,K,MaxSize,sstmean,a,b) %>%
  arrange(Genus) %>%
  slice(-c(1:3))

# growth_data = rbind(Moraisfin,Valefin) %>% 
#   left_join(diet,by="Species") %>%
#   arrange(Genus)  %>%
#   slice(-c(1:3)) %>%
#   fill(Diet,.direction="up")


#Prepping data for analysis
data_prepped = data_prep(growth_data)
save(data_prepped, file ="outputs/data_prepped.RData")

length(unique(data_prepped$Species))

#Testing model performances at predicting growth rates K based on mean adjusted R squared over 100 iterations
#We should NOT keep Diet here
fam_perf = K_fam_perf(data_prepped)
# fam_diet_perf = K_fam_diet_perf(data_prepped)

#We should NOT keep Diet here
gen_perf = K_gen_perf(data_prepped)
# gen_diet_perf = K_gen_diet_perf(data_prepped)

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

#Prepping RLS Data
RLS_fish = RLS_data_prep(fish,traits,coef,env)

#Predicting K and Linf
data_merged = merge_growth(data_prepped,RLS_fish,gen_model_K)

# save(data_merged, file = "outputs/data_merged.Rdata")
#AT SPECIES LEVEL USING THIS FORMULAE WE OVERSTIMATE BY A FACTOR 10 
data_K = predict_K(data_merged,gen_model_K,fam_model_K,fish_model_K)

data_K_final = data_K %>%
  mutate(K_pred = ifelse(!is.na(K_growth),K_pred/10,K_pred))

data_final = predict_Linf(data_K_final,fam_model_Linf,gen_model_Linf,fish_model_Linf)
# 
save(data_final, file = "outputs/data_final.Rdata")



#plot K versus observed

#For testing with intial databse 
data_merged = data_prepped %>%  mutate(K_growth = NA) %>% rename(Temperature = "sstmean") %>% mutate(Temperature = Temperature - 237.5)

data_K = predict_K(data_merged,gen_model_K,fam_model_K,fish_model_K)

data_plot = data_K %>%
  pivot_longer(c(K,K_pred))%>%
  mutate(value = log(value+1))

ggplot(data_plot,aes(value,fill=name))+
  geom_density(alpha = 0.6)+
  scale_fill_manual(labels = c("Observed K","Predicted K"), values = c("#1E72A6","#FDDA26"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Growth rate",
       y=  "Density")+
  guides(fill = guide_legend(title="Growth rate"))

ggsave("figures/PredictedObservedK.pdf",width = 297, height = 210,unit="mm")
