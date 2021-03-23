#' Testing models for the prediction of K growth rates at family level
#'
#' This function preps your data for necessary parameters for MTE 
#'
#' @param growth_data the output from the data_prep function 
#' 
#' @return dataframe with performance of each model
#' @export
#' 
#' 


growth_data = growth_data_prep
plot_fam_perf <- function(growth_data){
  
  #Multiple cross validation procedures to get mean R squared model
  test <-  mclapply(1:100,function(p){
    
    #Split data into 80% and 20% for crossvalidation 
    
    growth_data_split = growth_data %>% 
      initial_split(prop=0.8,strata="Family")
    
    growth_data_train = training(growth_data_split)
    growth_data_test = testing(growth_data_split)
    
    #----------------MODEL AT FAMILY LEVEL--------------------------
    
    #Testing model at family level
    fam_model <- lmer(logK~logMmax+InvTkb + (1+logMmax|Family),growth_data_train,
                      control = lmerControl(optimizer = "optimx", calc.derivs = T,
                                            optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    
    #Getting family model parameters 
    fam_model_clean <-  broom.mixed::tidy(fam_model,effects="ran_coefs") %>%
      #Dividing one column into three columns  for each coeff: Intercept, logMmax and InvTkb
      pivot_wider(names_from="term",values_from="estimate")%>%
      #Renaming the columns for merge
      dplyr::rename(Family="level",
                    Intercept = '(Intercept)',
                    SlopeLogMmax = logMmax,
                    SlopeInvTkb = InvTkb) %>%
      #Merging with test data
      merge(growth_data_test, by = "Family") %>%
      #Predicting K using MTE
      mutate(K_pred = exp(Intercept) * Mmax**(SlopeLogMmax)*exp(SlopeInvTkb/(8.62e-05*sstmean)))
    
    model = lm(K~K_pred,fam_model_clean)
    
    by_family  = group_by(fam_model_clean,Family)
    
    model_augment = do(by_family,augment(lm(K~K_pred,data=.))) %>%
      mutate(diff = abs(K - K_pred))

    #---------------GETTING MODEL PERFORMANCEs-----------------------------------
    
    p <- list(data.frame(model_augment))
   
  })
  
  family_perf <- do.call(rbind,do.call(rbind,test))

  ggplot(family_perf, aes(reorder(Family,-diff),diff,fill=Family))+
    geom_boxplot()+
    geom_jitter(alpha = 1/8,size=0.1)+
    guides(fill=FALSE)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y="Difference between observed growth rates and predicted growth rates",
         x="")

  ggsave("figures/diffam.pdf",height =11,width = 13)
  ggsave("figures/diffam.png",height =11,width = 13)

  ggplot(family_perf, aes(.fitted, .resid)) +
    geom_line(aes(group=Family,color=Family),alpha= 1/3)+
    geom_smooth(se=F,color="black")+
    theme_bw()+
    labs(x="Fitted values",
         y="Residuals")

  ggsave("figures/resfam.pdf",height=11,width = 13)
  ggsave("figures/resfam.png",height=11,width = 13)
  
}
