#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

plot_metrics_comparison = function(data_transect){


  prod_biom_lm = cor(data_transect$log10Prod,data_transect$log10Biom,method="pearson")
  pvalue1 = cor.test(data_transect$log10Prod,data_transect$log10Biom,method="pearson")$p.value
  
  ggplot(data_transect,aes(x=log10Biom,y=log10Prod))+
    geom_point(aes(colour=log(gravtot2+1)))+
    scale_colour_viridis_c(name="Gravity - log scale")+
    geom_smooth(method="lm",color='black',se=F)+
    theme_bw()+
    labs(x="Biomass (g/m^2) - log scale",
         y="Biomass production (g/m^2/year) - log scale")
  
  ggsave("Figures/prod_biom.png",height=210, width= 297,units="mm")
  
  prodB_biom_lm =  cor(data_transect$log10ProdB,data_transect$log10Biom,method="pearson")
  pvalue2 = cor.test(data_transect$log10ProdB,data_transect$log10Biom,method="pearson")$p.value
  
  ggplot(data_transect,aes(log10Biom,log10ProdB))+
    geom_point(aes(colour=log(gravtot2+1)))+
    scale_colour_viridis_c(name="Gravity - log scale")+
    geom_smooth(method="lm",color='black',se=F)+
    theme_bw()+
    labs(x="Biomass (g/m^2) - log scale",
         y="Productivity (%/year) - log scale")
  
  ggsave("Figures/prodB_biom.png",height=210, width= 297,units="mm")
  
  prodB_prod_lm = cor(data_transect$log10ProdB,data_transect$log10Prod,method="pearson")
  pvalue3 = cor.test(data_transect$log10ProdB,data_transect$log10Prod,method="pearson")$p.value
  
  ggplot(data_transect,aes(log10ProdB,log10Prod))+
    geom_point(aes(colour=log(gravtot2+1)))+
    scale_colour_viridis_c(name="Gravity - log scale")+
    geom_smooth(method="lm",color='black',se=F)+
    theme_bw()+
    labs(x="Productivity (%/year) - log scale",
         y="Biomass production (g/m^2/year) - log scale")
  
  ggsave("Figures/proB_prod.png",height=210, width= 297,units="mm")
  
  lm_metrics = data.frame(prod_biom_lm,prodB_biom_lm,prodB_prod_lm, pvalue1, pvalue2, pvalue3)
  
  
  return(lm_metrics)
}
