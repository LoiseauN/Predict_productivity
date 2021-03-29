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

plot_gen_perf <- function(growth_data){
  
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
    
    #Model parameters with genus
    
    model = lm(K~K_pred,gen_model_clean)
    
    by_genus  = group_by(gen_model_clean,Genus)
  
    model_augment = do(by_genus,augment(lm(K~K_pred,data=.))) %>%
      mutate(diff = abs(K - K_pred))
    
    #---------------GETTING MODEL PERFORMANCEs-----------------------------------
    
    p <- list(data.frame(model_augment))
    
    
  })
  
  
  gen_perf <- do.call(rbind,do.call(rbind,test))
  
  ggplot(gen_perf, aes(reorder(Genus,-diff),diff,fill=Genus))+
    geom_boxplot()+
    geom_jitter(alpha = 1/8,size=0.1)+
    scale_fill_viridis_d()+
    guides(fill=FALSE)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y="Difference between observed growth rates and predicted growth rates",
         x="")
  
  ggsave("figures/difgen.pdf",height =11,width = 13)
  ggsave("figures/difgen.png",height =11,width = 13)
  
  ggplot(gen_perf, aes(.fitted, .resid)) +
    geom_line(aes(group=Genus,color=Genus),alpha= 1/3)+
    guides(color=FALSE)+
    geom_smooth(se=F,color="black")+
    theme_bw()+
    labs(x="Fitted values",
         y="Residuals")
  
  ggsave("figures/resgen.pdf",height=11,width = 13)
  ggsave("figures/resgen.png",height=11,width = 13)
  
}
