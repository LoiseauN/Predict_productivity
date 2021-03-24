#' Testing models for the prediction of K growth rates at genus level
#'
#' This function preps your data for necessary parameters for MTE 
#'
#' @param growth_data the output from the data_prep function 
#' 
#' @return dataframe with performance of each model
#' @export
#' 
#' 

K_gen_diet_perf <- function(growth_data){
  
  #Multiple cross validation procedures to get mean R squared model
  test <-  mclapply(1:100,function(p){
    
    #Split data into 80% and 20% for crossvalidation 
    
    growth_data_split = growth_data %>% 
      initial_split(prop=0.8)
    
    growth_data_train = training(growth_data_split)
    growth_data_test = testing(growth_data_split)
    
    #----------------MODEL AT FAMILY LEVEL INCLUDING DIET-------------------------
    
    #Testing model at genus level including Diet
    gen_diet_model <- lmer(logK~logMmax+InvTkb + Diet+(1+logMmax|Genus),growth_data_train,
                           control = lmerControl(optimizer = "optimx", calc.derivs = T,
                                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    
    #Cleaning genus/diet model parameters 
    gen_diet_model_clean <- broom.mixed::tidy(gen_diet_model,effects="ran_coefs") %>%
      #Dividing terme column into three columns  for each coeff: Intercept, logMmax and InvTkb
      pivot_wider(names_from="term",values_from="estimate")%>%
      #Renaming the columns for merge
      dplyr::rename(Genus="level",
                    Intercept = '(Intercept)',
                    SlopeLogMmax = logMmax,
                    SlopeInvTkb = InvTkb)%>%
      #Merging with test database 
      merge(growth_data_test,by="Genus") %>%
      #Predicting K using MTE
      mutate(K_pred = exp(Intercept) * Mmax**(SlopeLogMmax)*exp(SlopeInvTkb/(8.62e-05*sstmean)))
    
    #Performance of genus/diet model
    gen_diet_perf <-  summary(lm(K~K_pred,gen_diet_model_clean))$adj.r.squared
    
    
    #---------------GETTING MODEL PERFORMANCEs-----------------------------------
    
    p <- list(data.frame(loop = p,
                         rsquared = gen_diet_perf))
    
  })
  
  genus_diet_perf <- do.call(rbind,do.call(rbind,test)) %>%
    summarize(rsquared = mean(rsquared))
  
}
