#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data with management classes to plot
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

boxplot_class <- function(data_prod){

labels = c("Transitional reefs","Productive reefs","Degraded reefs","Sanctuaries")

coralcover = ggplot(data_prod,aes(Class,coralcover,fill=Class))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values=c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE"))+
  scale_x_discrete(labels= labels)+
  theme(legend.position="none")+
  labs(x="",
       y="Coral cover")

depth = ggplot(data_prod,aes(Class,depth,fill=Class))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values=c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE"))+
  scale_x_discrete(labels= labels)+
  theme(legend.position="none")+
  labs(x="",
       y="Depth")

traveltime = ggplot(data_prod,aes(Class,Travel_h,fill=Class))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values=c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE"))+
  scale_x_discrete(labels= labels)+
  theme(legend.position="none")+
  labs(x="",
       y="Travel time")

final_plot = ggarrange(traveltime,coralcover,depth,ncol=1)

ggsave("Figures/SuppFigureX.pdf",height=297,width=210,units="mm")


}