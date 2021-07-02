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


plot_normality <- function(growth_data){
  
  #Checking normality for growth model at genus level
  gen_model <- lmer(logK~logMmax+InvTkb+(1 + logMmax|Genus),growth_data,
                    control = lmerControl(optimizer = "optimx", calc.derivs = T,
                                          optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  fixed = plot(check_normality(fam_model,effects="fixed"))
  random = plot(check_normality(fam_model,effects="random"))[[1]]
  
  ggarrange(fixed,random,ncol=1,heights=c(1,2))
  
  ggsave("Figures/growth_normality.png",height=15,width=13)
  ggsave("Figures/growth_normality.pdf",height=15,width=13)
  
}
  
