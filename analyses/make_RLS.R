#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","arm","performance","ggforce","beepr","gridExtra")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data---------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".Rdata")
data_list = lapply(files, load, .GlobalEnv)

socio <- readRDS(file="RLS_socio_withoutNA.rds")
env   <- readRDS(file="RLS_env_spatio_temporal.rds")
mpa   <- readRDS(file="RLS_mpa.rds")
coralcover   <- read.csv("RLS_coralcover.csv")
colnames(mpa)[1] <- "SurveyID"

data_prod_brut <-merge(data_prod_brut,socio,by="SurveyID",all.x=T)
data_prod_brut <-merge(data_prod_brut,env,by="SurveyID",all.x=T)
data_prod_brut <-merge(data_prod_brut,mpa,by="SurveyID",all.x=T)
data_prod_brut <-merge(data_prod_brut,coralcover,by="SurveyID",all.x=T)
data_prod_brut$Effectiveness<- factor(data_prod_brut$Effectiveness,levels=c("Low","Medium","High"),labels = c("Low","Medium","High"),ordered=T)

setwd(here())

#---------------Loading functions-----------------------

#Prepping RLS data for management
data_prod_select = data_prod_brut %>%
  dplyr::select(SurveyID,Depth,log10ProdB,log10Biom,gravtot2,HDI,NoViolence,Voice,ControlofCorruption,NGO,MarineEcosystemDependency,mean_chl_5year,mean_DHW_5year,
                mean_npp_5year,mean_pH_1year_5year,mean_sst_5year,Effectiveness,SiteLatitude,SiteLongitude)

data_management <- function(data_prod,s_sup_biom,s_inf_biom,s_sup_prod,s_inf_prod){
  
  biom75 = quantile(data_prod$log10Biom,s_sup_biom,na.rm=T)
  biom25 = quantile(data_prod$log10Biom,s_inf_biom,na.rm=T)
  prod75 = quantile(data_prod$log10ProdB,s_sup_prod,na.rm=T)
  prod25 = quantile(data_prod$log10ProdB,s_inf_prod,na.rm=T)
  
  #Diving data into 3 classes for each biomass/productivity relationship
  management = data_prod %>% 
    mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                          ifelse(log10Biom < biom75 & log10ProdB > prod75,"partial",
                                 ifelse(log10Biom > biom75,"pristine","transition"))),
           Class = as.factor(Class))
  
  return(management)
  
}

data_prod_management = data_management(data_prod_select,0.99,0.25,0.75,0.25)

save(data_prod_management,file="outputs/data_prod_management.RData")

#---------------------Prepping covariates-----------------------------------

data_covariates <- function(data_prod){
  
  covariates = data_prod %>%
    dplyr::mutate(Depth = arm::rescale(log(abs(Depth)+1)),
                  gravtot2 = arm::rescale(log(gravtot2+1)),
                  HDI = arm::rescale(log(HDI+1)),
                  NGO= arm::rescale(log(NGO+1)),
                  MarineEcosystemDependency = arm::rescale(log(MarineEcosystemDependency+1)),
                  mean_chl_5year=  arm::rescale(log(mean_chl_5year+1)),
                  mean_DHW_5year = arm::rescale(log(mean_DHW_5year+1)),
                  mean_npp_5year = arm::rescale(log(mean_npp_5year+1)),
                  mean_pH_1year_5year= arm::rescale(log(mean_pH_1year_5year+1)),
                  mean_sst_5year = arm::rescale(log(mean_sst_5year+1)),
                  Effectiveness = as.factor(Effectiveness)) %>%
                  # Gears.allowed= as.factor(Gears.allowed),
                  # coral_cover = arm::rescale(log(coral_cover+1))) %>%
    column_to_rownames('SurveyID')
  
  return(covariates)

}

#Prepping covariates
RLS_covariates = data_covariates(data_prod_management) %>%
  dplyr::select(-c(SiteLatitude,SiteLongitude)) %>%
  dplyr::select(-c(log10ProdB,log10Biom))



#------------------------------------------

summary(RLS_covariates)

RLS_covariates$Effectiveness = addNA(RLS_covariates$Effectiveness) 

test_df = RLS_covariates %>% na.omit()

test = test_model(test_df)

#Exploring data
group.colors <- c(deadzone = "#d69d4e", partial = "#ABDDDE", pristine = "#eccbae", transition = "#046c9a")

#Function to test model on RLS data 
test_model <- function(prod_data){
  
  ranger_loop = mclapply(1:100,function(i){
    
    data_split <- initial_split(prod_data, prop = 0.8)
    
    train <- training(data_split)
    test <- testing(data_split)
    
    mod = ranger(Class~.,data=train,mtry=3,probability=F,num.trees=1000,importance="permutation")
    
    score=predict(mod,test)
    
    CM = caret::confusionMatrix(score$predictions,test$Class)
    
    model_varImp = list(data.frame(variable_importance <- rownames_to_column(as.data.frame(importance(mod)))),
                        data.frame(Accuracy = CM$overall[1],
                                   Kappa = CM$overall[2],
                                   TSS = CM$byClass[1]+CM$byClass[2]-1),
                        data.frame(Balanced_Accuracy = CM$byClass[,11]))
    return(model_varImp)
    
  })
  
  #Getting variable importance from output list
  rel_inf = ranger_loop %>%
    #Transfomring list into large dataframe
    flatten_df() %>%
    #Recuperating column names
    unnest(cols=c())%>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    dplyr::group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))%>%
    mutate(percentage = (importance.mod.*100)/sum(importance.mod.))
  
  #Getting model performance from output list
  preds = ranger_loop %>%
    #Transfomring list into large dataframe
    flatten_df() %>%
    #Recuperating column names
    unnest(cols=c())%>%
    #Keeping only variables and their importance
    dplyr::select(Accuracy:TSS)%>%
    na.omit()%>%
    map_dbl(mean)%>%
    as_tibble(rownames=NA)
  
  #GETTING BALANCED ACCURACY FROM ITERATIONS
  
  #Getting balanced accuracy from output list
  preds_class = ranger_loop %>%
    #Transfomring list into large dataframe
    flatten_df() %>%
    #Recuperating column names
    unnest(cols=c())%>%
    #Keeping only variables and their importance
    dplyr::select(Balanced_Accuracy)%>%
    na.omit()%>%
    mutate(class = rep(c("deadzone","partial","pristine","transition"),100))%>%
    group_by(class)%>%
    dplyr::summarize(Balanced_Accuracy=mean(Balanced_Accuracy))
  
  output = list(data.frame(rel_inf),
                data.frame(preds),
                data.frame(preds_class))
  
  return(output)
  
}

#Doing some figures
plot_classes(data_prod_management)
plot_var_imp(test)

plot_classes <- function(data_prod){
  
  biom75 = quantile(data_prod$logBiom,0.99)
  biom25 = quantile(data_prod$logBiom,0.25)
  prod75 = quantile(data_prod$logProdB,0.75)
  prod25 = quantile(data_prod$logProdB,0.25)
  
  group.colors <- c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE")
  
  #Plotting the classes
  (ggplot(data_prod,aes(log10Biom,log10ProdB,colour=Class))+
      geom_point(size=4,alpha=0.4)+
      geom_hline(yintercept=prod25,linetype="dashed")+
      geom_vline(xintercept=biom25,linetype="dashed")+
      geom_hline(yintercept=prod75,linetype="dashed")+
      geom_vline(xintercept=biom75,linetype="dashed")+
      scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
      labs(x="Biomass (g/m²)",
           y = "Productivity (%)")+
      theme_classic())
  
  ggsave("figures/Figure1.pdf",dpi=300,width = 297, height = 210,units = "mm")
  
}

plot_var_imp <- function(model_test_output){
  
  #Getting variable importance from output list
  rel_inf = model_test_output[[1]] %>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    dplyr::group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))%>%
    mutate(percentage = (importance.mod.*100)/sum(importance.mod.))%>%
    arrange(percentage)
  
  rel_inf$rowname = c("Effectiveness","NGO","Depth","MarineEcosystemDependecy","HDI","Voice","ControlofCorruption","NoViolence","mean_DHW_5year","	
mean_pH_1year_5year","mean_npp_5year","mean_chl_5year","gravtot2","mean_sst_5year")
  
  plot = ggdotchart(rel_inf,x = "rowname",y="percentage",
                    color = "rowname",
                    palette = "simpsons",
                    add = "segments",
                    add.params = list(color = "rowname",size = 1.5),
                    rotate = TRUE,   
                    group = "rowname",
                    dot.size = 11, 
                    label = round(rel_inf$percentage,4),
                    font.label = list(color = "white", size = 6, 
                                      vjust = 0.5),
                    xlab = "",
                    ylab = "Variable importance (%)",
                    ggtheme = theme_pubclean())
  
  ggpar(plot, legend = "none")
  
  ggsave("Figures/Figure3.pdf",height=210,width=297,units="mm")
  
}

model_prob(test_df)

prod_data = test_df
modeloutput = test

model_prob = function(prod_data,modeloutput){
  
  #Full model and delete transition data
  mod = ranger(Class~.,data=prod_data,mtry=3,probability = T,num.trees=1000)
  
    var_imp = as.data.frame(modeloutput[1]) %>%
      arrange(-percentage) %>%
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
      geom_point(colour="#d69d4e",size=3,alpha=0.7)+
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
      geom_point(colour="#eccbae",size=3,alpha=0.7)+
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

    
    save(var_probs_flat,file="outputs/var_probs_flat.RData")
    
    load("outputs/var_probs_flat.RData")
    
    plot1 = var_probs_flat[1:3]
    plot2 = var_probs_flat[4:6]
    plot3 = var_probs_flat[7:9]
    plot4 = var_probs_flat[10:12]
    
    bigplot = plot_grid(plotlist=plot1,ncol=3)
    
    ggsave("figures/Figure4_1.pdf",height=210,width=297,units="mm")
    
    bigplot2 = plot_grid(plotlist=plot2,ncol=3)
    
    ggsave("figures/Figure4_2.pdf",height=210,width=297,units="mm")
    
    bigplot = plot_grid(plotlist=plot3,ncol=3)
    
    ggsave("figures/Figure4_3.pdf",height=210,width=297,units="mm")
    
    bigplot2 = plot_grid(plotlist=plot4,ncol=3)
    
    ggsave("figures/Figure4_4.pdf",height=210,width=297,units="mm")
    
 }  