



model_prob = function(prod_data,modeloutput){
  
  #Selecting covariables of interest
  data_formodel = prod_data %>%
    dplyr::select(Depth:Effectiveness,Class)%>%
    na.omit()
  
  #Full model and delete transition data
  mod = ranger(Class~.,data=data_formodel,mtry=3,probability = T,num.trees=1000)
  
  var_imp = as.data.frame(modeloutput[1]) %>%
    arrange(-percentage) %>%
    filter(rowname != "Effectiveness")%>%
    column_to_rownames("rowname")
  
  var_probs = mclapply(rownames(var_imp),function(i){
    
    pd = NULL
    plot_list = list()
    for (p in 1:4) {
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
      labs(x=paste(i),
           y="")+
      guides(fill=F)
    
    partial = pd %>%
      filter(Class=="partial")%>%
      ggplot(aes(eval(parse(text = paste(i))),yhat))+
      geom_point(colour="#046c9a",size=3,alpha=0.7)+
      geom_smooth(se=F)+
      theme_bw()+
      labs(x=paste(i),
           y="")+
      guides(fill=F)
    
    
    deadzone = pd %>%
      filter(Class=="deadzone")%>%
      ggplot(aes(eval(parse(text = paste(i))),yhat))+
      geom_point(colour="#d69d4e",size=3,alpha=0.7)+
      geom_smooth(se=F)+
      theme_bw()+
      labs(x=paste(i),
           y="")+
      guides(fill=F)
    
    transition = pd %>%
      filter(Class=="transition")%>%
      ggplot(aes(eval(parse(text = paste(i))),yhat))+
      geom_point(colour="#ABDDDE",size=3,alpha=0.7)+
      geom_smooth(se=F)+
      theme_bw()+
      labs(x=paste(i),
           y="")+
      guides(fill=F)
    
    (plots = ggarrange(pristine, partial, deadzone,transition, ncol = 1,nrow=4))
    
    plot_list[[i]] = plots
    
    return(plot_list)
    
    beep_on_error(sound = 4)
    
  }, mc.cores = 8)
  
  
  var_probs_flat = var_probs %>% flatten()
  
  var_probs_flat[6] = NULL
  var_probs_flat[14] = NULL
  var_probs_flat[13] = NULL 
  
  setwd(here())
  
  save(var_probs_flat,file="outputs/var_probs_flat.RData")
  
  load("outputs/var_probs_flat.RData")
  
  plot1 = var_probs_flat[1:3]
  plot2 = var_probs_flat[4:6]
  plot3 = var_probs_flat[7:9]
  plot4 = var_probs_flat[10:12]
  
  bigplot = plot_grid(plotlist=plot1,ncol=3)
  
  ggsave("figures/Figure4_1.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_1.png",height=210,width=297,units="mm")
  
  bigplot2 = plot_grid(plotlist=plot2,ncol=3)
  
  ggsave("figures/Figure4_2.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_2.png",height=210,width=297,units="mm")
  
  bigplot = plot_grid(plotlist=plot3,ncol=3)
  
  ggsave("figures/Figure4_3.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_3.png",height=210,width=297,units="mm")
  
  bigplot2 = plot_grid(plotlist=plot4,ncol=3)
  
  ggsave("figures/Figure4_4.pdf",height=210,width=297,units="mm")
  ggsave("figures/Figure4_4.png",height=210,width=297,units="mm")
  
  beep(sound=3)
  
}  