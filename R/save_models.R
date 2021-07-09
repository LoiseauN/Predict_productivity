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


save_fam_model_K <- function(growth_data){
  
  fam_model <- lmer(logK~logMmax+InvTkb + (1+logMmax|Family),growth_data,
                    control = lmerControl(optimizer = "optimx", calc.derivs = T,
                                          optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  #Adding the predicted K using metabolic theory equation
  fam_model_K =  broom.mixed::tidy(fam_model,effects="ran_coefs") %>%
    #Divising one column into three columns  for each coeff: Intercept, logMmax and InvTkb
    pivot_wider(names_from="term",values_from="estimate")%>%
    #Renaming the columns for merge
    dplyr::rename(Family="level",
                  Intercept = '(Intercept)',
                  SlopeLogMmax = logMmax,
                  SlopeInvTkb = InvTkb)
  
  return(fam_model_K)
  
}

save_gen_model_K <- function(growth_data){
  
  gen_model <- lmer(logK~logMmax+InvTkb + (1+logMmax|Genus),growth_data,
                    control = lmerControl(optimizer = "optimx", calc.derivs = T,
                                          optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  #Adding the predicted K using metabolic theory equation
  gen_model_K =  broom.mixed::tidy(gen_model,effects="ran_coefs") %>%
    #Divising one column into three columns  for each coeff: Intercept, logMmax and InvTkb
    pivot_wider(names_from="term",values_from="estimate")%>%
    #Renaming the columns for merge
    dplyr::rename(Genus="level",
                  Intercept = '(Intercept)',
                  SlopeLogMmax = logMmax,
                  SlopeInvTkb = InvTkb)
  
  return(gen_model_K)
  
}


save_fish_model_K <- function(growth_data){
  
  fish_model = lmer(logK~logMmax+InvTkb+(1+logMmax|Family),
                    growth_data, control = lmerControl(optimizer = "optimx", calc.derivs = T,
                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  
  fish_model_K = fixef(fish_model) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    pivot_wider(names_from = "rowname",values_from = ".")%>%
    dplyr::rename(Intercept = "(Intercept)",
                  SlopeLogMmax = "logMmax",
                  SlopeInvTkb = "InvTkb")
  
  return(fish_model_K)
  
}

save_fam_model_Linf <- function(growth_data){
  
  Growth_fam <-  groupedData(K~MaxSize|Family, data= growth_data)
  
  fam_model <- nlme(Linf~MaxSize.fct(a,MaxSize),
                                              data=Growth_fam,fixed=a~1,random=a~1,start=list(fixed=c(0.5)))
  
  #Tidying Family  model and Family/Diet model
  fam_model_Linf = broom.mixed::tidy(fam_model,effects="ran_coefs") %>%
  #Divising terme column into three columns  for each coeff: Intercept, logMmax and InvTkb
  pivot_wider(names_from="term",values_from="estimate")%>%
  #Renaming the columns for merge
  dplyr::rename(Family="level",
                  pente_pred="a")
  
  return(fam_model_Linf)
  
}

save_gen_model_Linf <- function(growth_data){
  
  Growth_gen = groupedData(K~MaxSize|Genus, data=growth_data)
  
  gen_model <- nlme(Linf~MaxSize.fct(a,MaxSize),
                         data=Growth_gen,fixed=a~1,random=a~1,start=list(fixed=c(0.5)))
  
  #Tidying Family  model and Family/Diet model
  gen_model_Linf = broom.mixed::tidy(gen_model,effects="ran_coefs") %>%
    #Divising terme column into three columns  for each coeff: Intercept, logMmax and InvTkb
    pivot_wider(names_from="term",values_from="estimate")%>%
    #Renaming the columns for merge
    dplyr::rename(Genus="level",
                  pente_pred="a")
  
  return(gen_model_Linf)
  
}

save_fish_model_Linf <- function(growth_data){

Growth=groupedData(K~MaxSize|Family, data=growth_data)

fish_model = nlme(Linf~MaxSize.fct(a,MaxSize),
                     data=Growth,fixed=a~1,random=a~1,start=list(fixed=c(0.5)))

fish_model_Linf = fixef(fish_model)

return(fish_model_Linf)


}
