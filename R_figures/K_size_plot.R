#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

K_by_size = function(data_prod){
  
  data_plot = data_prod %>%
    group_by(Species)%>%
    mutate(K = mean(K_pred),
           Family = as.factor(Family),
           Species = as.factor(Species))%>%
    distinct(K,.keep_all=T)
  
  sapply(data_plot,typeof)
  
  pal <- wes_palette("Darjeeling1", 27, type = "continuous")
  
  (p = ggscatter(data_plot,x="SizeMax",y="K",
            color="Family",
            palette = pal,
            size=3,alpha=0.6)+
    border())+geom_smooth(se=F,color="black")
  
  
  ggsave("Figures/K_by_size.pdf",height=10,width=15)
  ggsave("Figures/K_by_size.png",height=10,width=15)
  
}