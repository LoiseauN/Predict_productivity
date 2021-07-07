#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 


test_model <- function(prod_data){
  
  #Selecting covariables of interest
  data_formodel = prod_data %>%
    dplyr::select(Depth:Effectiveness,Class)%>%
    na.omit()

#Running random foret a 100 times
ranger_loop = mclapply(1:100,function(i){
  
  data_split <- initial_split(data_formodel, prop = 0.8)
  
  train <- training(data_split)
  test <- testing(data_split)
  
  mod = ranger(Class~.,data=train,mtry=3,probability=F,num.trees=1000,importance="permutation")
  
  score=predict(mod,test)
  
  CM = caret::confusionMatrix(score$predictions,test$Class)
  
  model_varImp = list(data.frame(variable_importance <- rownames_to_column(as.data.frame(importance(mod)))),
                      data.frame(Accuracy = CM$overall[1],
                                 Kappa = CM$overall[2],
                                 TSS = CM$byClass[1]+CM$byClass[2]-1),
                      data.frame(Balanced_Accuracy = CM$byClass[,11]))
  return(model_varImp)
  
},mc.cores = 3)

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

#GETTING BALANCED ACCURACY FROM ITERATIONS

#Getting balanced accuracy from output list
preds_class = ranger_loop %>%
  #Transfomring list into large dataframe
  flatten_df() %>%
  #Recuperating column names
  unnest(cols=c())%>%
  #Keeping only variables and their importance
  dplyr::select(Balanced_Accuracy)%>%
  na.omit()%>%
  mutate(class = rep(c("deadzone","partial","pristine","transition"),100))%>%
  group_by(class)%>%
  dplyr::summarize(Balanced_Accuracy=mean(Balanced_Accuracy))

output = list(data.frame(rel_inf),
              data.frame(preds),
              data.frame(preds_class))

beep(sound=4)

return(output)




}
