#' Function to run all the functions needed to predict growth and Linf
#' 


predict_growth = function(){

#Prepping data, removing some errors 
data_growth = growth_database %>% 
  dplyr::select(Family,Species,MaxSizeTL,Diet,a,b,Linf,K,sstmean) %>%
  rename(MaxSize = "MaxSizeTL") %>%
  mutate(Genus = word(Species,1)) %>%
  dplyr::select(-Linf) %>%
  dplyr::select(Family,Genus,Species,Diet,K,MaxSize,sstmean,a,b) %>%
  #Some rows are only numbers, delete them 
  arrange(Genus) %>%
  slice(-c(1:3))

#Prepping data for analysis
data_prepped = data_prep(data_growth)
save(data_prepped, file ="outputs/data_prepped.RData")

#Testing model performances at predicting growth rates K based on mean adjusted R squared over 100 iterations

#At family level
fam_perf = K_fam_perf(data_prepped)
print(fam_perf)
save(fam_perf, file = "outputs/fam_perf.Rdata")

#At genus level
gen_perf = K_gen_perf(data_prepped)
print(gen_perf)
save(gen_perf, file = "outputs/gen_perf.Rdata")

#Testing Linf predictions model performance
#For this we combined the base dataset with another growth dataset
Linf_fam_perf =   Linf_fam_perf(growth_Linf)
print(Linf_fam_perf)
save(Linf_fam_perf, file = "outputs/Linf_fam_perf.Rdata")

Linf_gen_perf = Linf_gen_perf(growth_Linf)
print(Linf_gen_perf)
save(Linf_gen_perf, file = "outputs/Linf_gen_perf.Rdata")

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

#Predicting K using MTE
data_K = predict_K(data_merged,gen_model_K,fam_model_K,fish_model_K)
print(head(data_K))

#Predicting Linf using MTE
data_final = predict_Linf(data_K,fam_model_Linf,gen_model_Linf,fish_model_Linf)
print(head(data_final))

save(data_final, file = "outputs/data_final.Rdata")


}
