#' Testing models for the prediction of Tinf 
#'
#' This function does 100 iterations of the model to predict Linf and gives model performance
#'
#' @param growth_Linf the output from the data_prep function + added data
#' 
#' @return dataframe with performance of model
#' 
#' 

Linf_gen_perf <- function(growth_data){
  
  #Multiple cross validation procedures to get mean R squared model
  test <-  mclapply(1:100,function(p){
    
    #Split data into 80% and 20% for crossvalidation 
    
    growth_data_split <- growth_data %>%
      initial_split(prop=0.8)
    
    growth_data_train <-  training(growth_data_split)
    growth_data_test <- testing(growth_data_split)
    
    #NLME model to predict Tinf at family level
    Growth_gen = groupedData(K~MaxSize|Genus, data=growth_data_train)
    
    tryCatch(Growth_gen_model <-  nlme(Linf~MaxSize.fct(a,MaxSize),
                              data=Growth_gen,fixed=a~1,random=a~1,start=list(fixed=c(0.5)))
             ,error = function(e) cat("Error, model didn't converge"))
    
    #Cleaning model parameters
    Growth_gen_model_clean <- broom.mixed::tidy(Growth_gen_model,effects="ran_coefs") %>%
      #Divising terme column into three columns  for each coeff: Intercept, logMmax and InvTkb
      pivot_wider(names_from="term",values_from="estimate")%>%
      #Renaming the columns for merge
      dplyr::rename(Genus="level",
                    a_pred="a")%>%
      merge(growth_data_test,by="Genus")%>%
      mutate(Linf_predict = a_pred*MaxSize)
    
    #Performance of family/diet model
    gen_perf <- summary(lm(Linf~Linf_predict,Growth_gen_model_clean))$adj.r.squared
    
    p <- list(data.frame(loop = p,
                         rsquared = gen_perf))
    
  })
  
  family_perf <- do.call(rbind,do.call(rbind,test))
  family_perf <- summarize(family_perf,rsquared = mean(rsquared))
  
}

