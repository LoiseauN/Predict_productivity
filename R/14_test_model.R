#' This function does 100 iterations of the random forest model to check its performance in predicting management class
#' 
#' @param prod_data output from data_management function
#' 
#' 
#' @return dataframe with variable importance and model performance
#' 


test_model <- function(prod_data){

  
  #Selecting covariables of interest
  data_formodel = prod_data %>%
    dplyr::select(-c(site_code, SurveyID, Biom, Prod, Effectiveness, Productivity, log10ProdB:Country)) %>%
    na.omit()

#Running random foret a 100 times
ranger_loop = mclapply(1:100,function(i){
  
  data_split <- initial_split(data_formodel, prop = 0.8)
  
  train <- training(data_split)
  test <- testing(data_split)
  
  mod = ranger(Class~ .,data=train,mtry=3,probability=F,num.trees=1000,importance="permutation")
  
  score=predict(mod,test)
  
  CM = caret::confusionMatrix(score$predictions,test$Class)
  
  CM$byClass
  
  model_varImp = list(data.frame(variable_importance <- rownames_to_column(as.data.frame(importance(mod)))),
                      data.frame(Accuracy = CM$overall[1],
                                 Kappa = CM$overall[2],
                                 TSS = CM$byClass[1]+CM$byClass[2]-1),
                      data.frame(Balanced_Accuracy = CM$byClass[,11]))
  return(model_varImp)
  
},mc.cores = 5)

#Getting variable importance from output list
rel_inf = ranger_loop %>%
  #Transfomring list into large dataframe
  flatten_df() %>%
  #Recuperating column names
  unnest(cols=c())%>%
  #Keeping only variables and their importance
  dplyr::select(rowname,importance.mod.)%>%
  na.omit()%>%
  #Getting mean variable importance over all iterations
  dplyr::group_by(rowname)%>%
  dplyr::summarize(importance.mod.=mean(importance.mod.))%>%
  mutate(percentage = (importance.mod.*100)/sum(importance.mod.))

#Getting model performance from output list
preds = ranger_loop %>%
  #Transfomring list into large dataframe
  flatten_df() %>%
  #Recuperating column names
  unnest(cols=c())%>%
  #Keeping only variables and their importance
  dplyr::select(Accuracy:TSS)%>%
  na.omit()%>%
  map_dbl(mean)%>%
  as_tibble(rownames=NA)

output = list(data.frame(rel_inf),
              data.frame(preds))

return(output)

}
