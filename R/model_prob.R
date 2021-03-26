#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

prod_data = NC_covariates
model_prob = function(prod_data){
  
  #Full model and delete transition data
  mod = ranger(Class~.,data=prod_data,mtry=3,probability = T,num.trees=1000)

  
  #getting model probabilities
  pd_travel <- NULL
  for (i in 1:4) {
    tmp <- pdp::partial(mod, pred.var = c("Travel_h"),
                   which.class = i, grid.resolution = 101 ,n.trees=1000)
    pd_travel <- rbind(pd_travel, cbind(tmp, Class = levels(prod_data$Class)[i]))
  }
  
  pd_depth <- NULL
  for (i in 1:4) {
    tmp <- pdp::partial(mod, pred.var = c("depth"),
                   which.class = i, grid.resolution = 101 ,n.trees=1000)
    pd_depth <- rbind(pd_depth, cbind(tmp, Class = levels(prod_data$Class)[i]))
  }
  
  pd_coralcover <- NULL
  for (i in 1:4) {
    tmp <- pdp::partial(mod, pred.var = c("coralcover"),
                   which.class = i, grid.resolution = 101 ,n.trees=1000)
    pd_coralcover <- rbind(pd_coralcover, cbind(tmp, Class = levels(prod_data$Class)[i]))
  }
  
  #plotting model 
  
  pristine_travel = pd_travel %>%
    filter(Class=="pristine")%>%
    ggplot(aes(Travel_h,yhat))+
    geom_point(colour="#d69d4e",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Travel time",
         y="")+
    guides(fill=F)
  
  partial_travel = pd_travel %>%
    filter(Class=="partial")%>%
    ggplot(aes(Travel_h,yhat))+
    geom_point(colour="#046c9a",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Travel time",
         y="")+
    guides(fill=F)
  
  
  deadzone_travel = pd_travel %>%
    filter(Class=="deadzone")%>%
    ggplot(aes(Travel_h,yhat))+
    geom_point(colour="#eccbae",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Travel time",
         y="")+
    guides(fill=F)
  
  
  pristine_depth = pd_depth %>%
    filter(Class=="pristine")%>%
    ggplot(aes(depth,yhat))+
    geom_point(colour="#d69d4e",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Depth",
         y ="")+
    guides(fill=F)
  
  
  partial_depth = pd_depth %>%
    filter(Class=="partial")%>%
    ggplot(aes(depth,yhat))+
    geom_point(colour="#046c9a",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Depth",
         y="")+
    guides(fill=F)
  
  
  deadzone_depth = pd_depth %>%
    filter(Class=="deadzone")%>%
    ggplot(aes(depth,yhat))+
    geom_point(colour="#eccbae",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Depth",
         t="")+
    guides(fill=F)
  
  
  pristine_coralcover = pd_coralcover %>%
    filter(Class=="pristine")%>%
    ggplot(aes(coralcover,yhat))+
    geom_point(colour="#d69d4e",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Coral cover",
         y="")+
    guides(fill=F)

  
  partial_coralcover = pd_coralcover %>%
    filter(Class=="partial")%>%
    ggplot(aes(coralcover,yhat))+
    geom_point(colour="#046c9a",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Coral cover",
         y="")+
    guides(fill=F)
  
  
  deadzone_coralcover = pd_coralcover %>%
    filter(Class=="deadzone")%>%
    ggplot(aes(coralcover,yhat))+
    geom_point(colour="#eccbae",size=3,alpha=0.7)+
    geom_smooth(se=F)+
    theme_bw()+
    labs(x="Coral cover",
         y="")+
    guides(fill=F)
  
  plots = ggarrange(pristine_travel,pristine_depth,pristine_coralcover,
            partial_travel,partial_depth,partial_coralcover,
            deadzone_travel,deadzone_depth,deadzone_coralcover,nrow = 3, ncol = 3)
  
  plots = annotate_figure(plots,left = "Classification probability")
  
  img_pristine = readPNG("graphics/pristine.png")
  img_partial = readPNG("graphics/partial.png")
  img_dead = readPNG("graphics/dead.png")
  
  im_pr = ggplot()+
    background_image(img_pristine)+
    theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))
  
  im_pa = ggplot()+
    background_image(img_partial)+
    theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))
  
  im_d = ggplot()+
    background_image(img_dead)+
    theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))
  
 (img = ggarrange(im_pr,
            im_pa,
            im_d,ncol=1,widths = 6,heights = 5))
  
  ggarrange(img,plots,widths=c(1,2))
  
  ggsave("Figures/proba_vars.pdf",height=11,width=17)
  ggsave("Figures/proba_vars.png",height=11,width=17)
}