#' Testing models for the prediction of Tinf 
#'
#' This function preps your data for necessary parameters for MTE 
#'
#' @param growth_data the output from the data_prep function 
#' 
#' @return dataframe with performance of each model
#' @export
#' 
#' 

Linf_fam_diet_perf <- function(growth_data){
  
  #Multiple cross validation procedures to get mean R squared model
  test <-  mclapply(1:100,function(p){
    
    #Split data into 80% and 20% for crossvalidation 
    
    growth_data_split <- growth_data %>% 
      initial_split(prop=0.8)
    
    growth_data_train <-  training(growth_data_split)
    growth_data_test <- testing(growth_data_split)
    
    #NLME model to predict Tinf at family level
    Growth_fam_diet= groupedData(K~MaxSize|Family/Diet, data=growth_data_train)
    
    Growth_fam_diet_model <-  nlme(Linf~MaxSize.fct(a,MaxSize),
                              data=Growth_fam_diet,fixed=a~1,random=a~1,start=list(fixed=c(0.5)))
    
    #Cleaning model parameters
    Growth_fam_diet_model_clean <- broom.mixed::tidy(Growth_fam_diet_model,effects="ran_coefs") %>%
      #Divising terme column into three columns  for each coeff: Intercept, logMmax and InvTkb
      pivot_wider(names_from="term",values_from="estimate")%>%
      #Renaming family
      mutate(Family=sub("/.*","",level))%>%
      #Renaming the columns for merge
      dplyr::rename(FamilyDiet="level",
                    a_pred="a")%>%
      merge(growth_data_test,by="Family")%>%
      mutate(Linf_predict = a_pred*MaxSize)
    
    #Performance of family/diet model
    fam_diet_perf <- summary(lm(Linf~Linf_predict,Growth_fam_diet_model_clean))$adj.r.squared
    
    p <- list(data.frame(loop = p,
                         rsquared = fam_diet_perf))
    
  })
  
  family_perf <- do.call(rbind,do.call(rbind,test)) %>%
    summarize(rsquared = mean(rsquared))
  
}