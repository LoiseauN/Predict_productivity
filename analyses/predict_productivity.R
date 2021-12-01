
#' Function to run all the functions needed to predict productivity
#' 


predict_productivity = function(){

#Calculating productivity
RLS_prod_all = calc_prod(data_final)
        
save(RLS_prod_all, file = "outputs/RLS_prod_all.RData")

RLS_prod = calc_prod_transect(RLS_prod_all,info)

#Removing outliers, Biomass values superior to 99.5% of values
RLS_prod = RLS_prod %>% filter(Biom < quantile(RLS_prod$Biom,0.995))

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


}



