#' Testing models for the prediction of K growth rates at genus level
#'
#' This function does 100 iterations of the model at genus level to give model performance
#'
#' @param growth_data the output from the data_prep function 
#' 
#' @return dataframe with performance of genus model
#' 
#' 

K_gen_perf <- function(growth_data){
  
  #Multiple cross validation procedures to get mean R squared model
  test <-  mclapply(1:100,function(p){
    
    #Split data into 80% and 20% for crossvalidation 
    
    growth_data_split = growth_data %>% 
      initial_split(prop=0.8)
    
    growth_data_train = training(growth_data_split)
    growth_data_test = testing(growth_data_split)
    
    #----------------MODEL AT FAMILY LEVEL--------------------------
    
    #Testing model at genus level
    gen_model <- lmer(logK~logMmax+InvTkb+(1 + logMmax|Genus),growth_data_train,
                      control = lmerControl(optimizer = "optimx", calc.derivs = T,
                                            optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    
    fixef(gen_model)
    
    #Cleaning genus model parameters 
    gen_model_clean <- broom.mixed::tidy(gen_model,effects="ran_coefs") %>%
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
    
    (gen_perf <- summary(lm(K~K_pred,gen_model_clean))$adj.r.squared)
    

     # ggplot(gen_model_clean,aes(log(K+1), log(K_pred+1)))+
     # geom_point(aes(color=Genus)) +
     #   geom_smooth(method = "lm")+
     #   scale_color_viridis_d(guide="none") +
     #   theme_minimal() +
     #   labs(x = "Observed growth rates (log scale)",
     #        y  = "Predicted growth rates (log scale)")+
     #   theme(text = element_text(size=20))

    # ggsave("figures/Genus_crossvaldiation.png", width = 20, height = 15)
    # 
    length(unique(data_prepped$Genus))

    #Performance of genus model
    (gen_perf <- summary(lm(K~K_pred,gen_model_clean))$adj.r.squared)
    
    
    #---------------GETTING MODEL PERFORMANCEs-----------------------------------
    
    p <- list(data.frame(loop = p,
                         rsquared = gen_perf))
    
  })
  
  genus_perf <- do.call(rbind,do.call(rbind,test)) %>%
    summarize(rsquared = mean(rsquared))
  
}
