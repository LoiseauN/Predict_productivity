#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

test_model <- function(prod_data){
  
  ranger_loop = mclapply(1:100,function(i){
    
    data_split <- initial_split(prod_data, prop = 0.8)
    
    train <- training(data_split)
    test <- testing(data_split)
    
    mod = ranger(Class~.,data=train,mtry=3,probability=F,num.trees=1000,importance="permutation")
    
    score=predict(mod,test)
    
    CM = caret::confusionMatrix(score$predictions,test$Class)
    
    model_varImp = list(CM$table)
    
    return(model_varImp)
    
  })
  
  t(apply(simplify2array(ranger_loop), 1:100, mean))
  
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
    dplyr::summarize(importance.mod.=mean(importance.mod.))
  
  output = list(data.frame(rel_inf),
                data.frame(preds),
                data.frame(preds_class))
  
  return(output)
  
  
}
