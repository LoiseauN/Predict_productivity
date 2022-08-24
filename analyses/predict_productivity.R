
#' Function to run all the functions needed to predict productivity
#' 

predict_productivity = function(){
  
#Prepping RLS Data
print("Preparing RLS Data..")
data_final = RLS_data_prep(fish,traits,coef,env)

#Calculating productivity
print("Calculating productivity for all fish")
RLS_prod_all = calc_prod_rfishprod(data_final)

#Sensitivtiy to account for variability between transects
RLS_prod_sensitivity = RLS_prod_all %>% filter(Size < quantile(Size, 0.95)) %>% filter(Num < quantile(Num, 0.95))

save(RLS_prod_all, file = "outputs/RLS_prod_all.Rdata")

print("Aggregating at transect level and prepping management classes and covariates")
RLS_prod = calc_prod_transect(RLS_prod_all,info)

save(RLS_prod,file = "outputs/RLS_prod.Rdata")

RLS_prod_sensitivty = calc_prod_transect(RLS_prod_sensitivity, info)

#Removing outliers, Biomass and productivity values superior to 99% of values
RLS_prod = RLS_prod %>% filter(Biom < quantile(RLS_prod$Biom,0.99)) %>% filter(Productivity < quantile(RLS_prod$Productivity,0.99))
RLS_prod_sensitivty = RLS_prod_sensitivty %>% filter(Biom < quantile(RLS_prod$Biom,0.99)) %>% filter(Productivity < quantile(RLS_prod$Productivity,0.99))
save(RLS_prod, file = "outputs/RLS_prod.Rdata")

RLS_info = read.table("data/RLS_transect_info.txt") %>%
  dplyr::rename(site_code = "SiteCode")

summary(RLS_prod)
range(RLS_prod$Productivity)

#Prepping covariates
RLS_Covariates_transect = data_covariates(RLS_prod,env_data = env,socio_data = socio,mpa_data = mpa)
RLS_Covariates_transect_sensitivity = data_covariates(RLS_prod_sensitivty,env,socio,mpa)
save(RLS_Covariates_transect,file="outputs/RLS_Covariates_transect.Rdata")

print(paste("Number of transects:",length(unique(RLS_Covariates_transect$SurveyID))))
RLS_Covariates = RLS_Covariates_transect %>% distinct(site_code, .keep_all = T) %>% dplyr::select(-c( Voice, ControlofCorruption, NoViolence,SurveyID))
RLS_Covariates_sensitivity = RLS_Covariates_transect_sensitivity %>% distinct(site_code, .keep_all = T)%>% dplyr::select(-c( Voice, ControlofCorruption, NoViolence,SurveyID))

# quantile(RLS_Covariates$Biom, 0.95)
save(RLS_Covariates,file="outputs/RLS_Covariates.Rdata")

# #TEMPORARY CODE NOT YET AS FUNCTION TO CREATE CLUSTERS
# RLS_test = RLS_Covariates %>% dplyr::select(site_code, log10Biom, log10ProdB) %>% column_to_rownames("site_code")
# 
# #Suggested number of clusters
# #Optimum is at 3
# fviz_nbclust(RLS_test, kmeans, method = "silhouette")
# 
# #test =
# km.res <- kmeans(RLS_test, 3, nstart = 25)
# 
# fviz_cluster(km.res, data = RLS_test,
#              ellipse.type = "convex",
#              palette = "jco",
#              ggtheme = theme_minimal())
# 
# 
# clusters = as.data.frame(km.res$cluster)
# 
# RLS_Management = cbind(RLS_test, clusters) %>%
#   dplyr::rename(Class = "km.res$cluster") %>%
#   mutate(Class = ifelse(Class == 1, "partial",ifelse(Class == 2, "deadzone",ifelse(Class == 3,"pristine",NA)))) %>%
#   mutate(Class = as.factor(Class)) %>%
#   dplyr::select(-c(log10ProdB,log10Biom))
# 
# RLS_Management = cbind(RLS_Covariates, RLS_Management)

range(RLS_Management$Productivity)
range(RLS_Management$Biom)
range(RLS_Management$Prod)


# #Protection classes
RLS_Management = data_management(RLS_Covariates,0.95,0.25,0.75,0.25)
RLS_Management_sensitivity = data_management(RLS_Covariates_sensitivity,0.95,0.25,0.75,0.25)
RLS_Management_5 = data_management(RLS_Covariates,0.9,0.2,0.7,0.2)
RLS_Management_5plus = data_management(RLS_Covariates,0.99,0.3,0.8,0.3)
RLS_Management_15 = data_management(RLS_Covariates,0.8,0.1,0.6,0.1)
RLS_Management_15plus = data_management(RLS_Covariates,0.99,0.4,0.9,0.4)
save(RLS_Management,file="outputs/RLS_Management.Rdata")

#Modelling
print("Modelling management classes according to all covariates")
set.seed(42)
model_test = test_model(RLS_Management)
model_sensitivity = test_model(RLS_Management_sensitivity)
model_5 = test_model(RLS_Management_5)
model_5plus = test_model(RLS_Management_5plus)
model_15 = test_model(RLS_Management_15)
model_15plus = test_model(RLS_Management_15plus)
save(model_test, file ="outputs/model_test.Rdata")

}



