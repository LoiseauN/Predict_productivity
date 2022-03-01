
#' Function to run all the functions needed to predict productivity
#' 

predict_productivity = function(){

#Calculating productivity
RLS_prod_all = calc_prod(data_final)

save(RLS_prod_all, file = "outputs/RLS_prod_all.Rdata")

RLS_prod = calc_prod_transect(RLS_prod_all,info)

length(unique(RLS_prod$site_code))

#Removing outliers, Biomass values superior to 99.5% of values
RLS_prod = RLS_prod %>% filter(Biom < quantile(RLS_prod$Biom,0.995))

length(unique(RLS_prod$site_code))

save(RLS_prod, file = "outputs/RLS_prod.Rdata")

#Prepping covariates
RLS_Covariates_transect = data_covariates(RLS_prod,env,socio,mpa)
RLS_Covariates = RLS_Covariates_transect %>% distinct(site_code, .keep_all = T) %>% dplyr::select(-c(Biom,Prod,Productivity))
#
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

# #Protection classes
RLS_Management = data_management(RLS_Covariates,0.95,0.25,0.75,0.25)
save(RLS_Management,file="outputs/RLS_Management.Rdata")

#Modelling
set.seed(42)
model_test = test_model(RLS_Management)
save(model_test, file ="outputs/model_test.Rdata")

}



