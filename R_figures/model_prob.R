#' Relationship between each covariate and the management classes 
#' 
#' @param prod_data output from the data_management function
#' @param modeloutput output from the test_model function
#' 
#' @return plots representing the interaction between each covariate and the management class
#' 

model_prob = function(prod_data,modeloutput){
  
  prod_data = RLS_Management
  modeloutput = test_model
  
  #Selecting covariables of interest
  data_formodel = prod_data %>%
    dplyr::select(-c(site_code, SurveyID, No.take.multizoned, log10ProdB:Country)) %>%
    mutate(gravtot2 = log(gravtot2+1))%>%
    na.omit()
  
  #Full model and delete transition data
  mod = ranger(Class~.,data=data_formodel,mtry=3,probability = T,num.trees=1000)
  
  var_imp = model_test_output[[1]] %>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    dplyr::group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))%>%
    mutate(percentage = (importance.mod.*100)/sum(importance.mod.))%>%
    arrange(percentage) %>%
    column_to_rownames("rowname")
  
  var_imp$var_names = c("NoViolence","Voices","ControlofCorruption","NGO","Effectiveness","MarineEcosystemDependency",
                        "HDI","	mean_pH_1year_5year","Depth","mean_sst_5year","mean_DHW_5year","gravtot2","mean_npp_5year")
  
  var_probs = mclapply(rownames(var_imp),function(i){
    
    pd = NULL
    plot_list = list()
    for (p in 1:3) {
      tmp <- pdp::partial(mod, pred.var = c(i),
                          which.class = p, grid.resolution = 101 ,n.trees=1000)
      pd <- rbind(pd, cbind(tmp, Class = levels(prod_data$Class)[p]))
      
    }
    
    pristine = pd %>%
      filter(Class=="pristine")%>%
      ggplot(aes(eval(parse(text = paste(i))),yhat)) +
      geom_point(colour="#C1DB60",size=3,alpha=0.7)+
      geom_smooth(se=F)+
      theme_bw()+
      labs(x= var_imp[i,3],
           y="")+
      guides(fill=F)
    
    partial = pd %>%
      filter(Class=="partial")%>%
      ggplot(aes(eval(parse(text = paste(i))),yhat))+
      geom_point(colour="#046c9a",size=3,alpha=0.7)+
      geom_smooth(se=F)+
      theme_bw()+
      labs(x= var_imp[i,3],
           y="")+
      guides(fill=F)
    
    
    deadzone = pd %>%
      filter(Class=="deadzone")%>%
      ggplot(aes(eval(parse(text = paste(i))),yhat))+
      geom_point(colour="#d69d4e",size=3,alpha=0.7)+
      geom_smooth(se=F)+
      theme_bw()+
      labs(x= var_imp[i,3],
           y="")+
      guides(fill=F)
    
    (plots = ggarrange(pristine, partial, deadzone, ncol = 1,nrow=3))
    
    plot_list[[i]] = plots
    
    return(plot_list)
    
  }, mc.cores = 1)
  
  var_probs_flat = var_probs %>% flatten()
  
  setwd(here())
  
  save(var_probs_flat,file="outputs/var_probs_flat.RData")
  
  load("outputs/var_probs_flat.RData")
  
  #For each group of covariates, three plots 
  plot1 = var_probs_flat[1:3]
  plot2 = var_probs_flat[4:6]
  plot3 = var_probs_flat[7:9]
  plot4 = var_probs_flat[10:13]
  
  bigplot = plot_grid(plotlist=plot1,ncol=3)
  
  ggsave("figures/Figure4_1.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_1.png",height=210,width=297,units="mm")
  
  bigplot2 = plot_grid(plotlist=plot2,ncol=3)
  
  ggsave("figures/Figure4_2.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_2.png",height=210,width=297,units="mm")
  
  bigplot3 = plot_grid(plotlist=plot3,ncol=3)
  
  ggsave("figures/Figure4_3.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_3.png",height=210,width=297,units="mm")
  
  bigplot4 = plot_grid(plotlist=plot4,ncol=3)
  
  ggsave("figures/Figure4_4.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_4.png",height=210,width=297,units="mm")

}  