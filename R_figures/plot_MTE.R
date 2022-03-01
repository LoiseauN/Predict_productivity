#' Plots of adjusted growth rates according to temperature and mass
#' 
#' @param growth_data output of data_prep function
#' @param fam_model_K K prediction model at family level - output from the save_model_K function in predict_growth script
#' 
#' 
#' @return Plots of adjusted growth rates according to temperature and mass
#' 

plot_MTE = function(growth_data,fam_model_K, gen_model_K){
  
  growth_data = data_prepped
  
  #PLOT MTE
  MTE_fam = fam_model_K %>%
    merge(growth_data,by="Family") %>%
    #Calculating temperature corrected and mass corrected logK
    mutate(TempCorrectLogK = logK - SlopeInvTkb * InvTkb,
           TempStandardLogK = logK - SlopeInvTkb * InvTkb + SlopeInvTkb * mean(InvTkb),
           MassCorrectLogK = logK - SlopeLogMmax * logMmax,
           MassStandardLogK = logK - SlopeLogMmax * logMmax * mean(logMmax)) %>%
    #max and min logMmax and InvTkb for all fish
    mutate(MaxlogMmaxFixe = max(logMmax),
           MinlogMmaxFixe = min(logMmax),
           MaxInvTkbFixe = max(InvTkb),
           MinInvTkbFixe = min(InvTkb),
           MeanInvTkb = mean(InvTkb),
           MeanlogMmax = mean(logMmax))%>%
    #Fixed effects for all fish according to the fish level (from Morais_fish Rdata)
    mutate(InterceptFixe = 20.199189,
           SlopelogMmaxFixe = -0.194814,
           SlopeInvTkbFixe = 	-0.507734) %>%
    #grouping by family and calculating min and max logMmax and InvTkb for each family
    group_by(Family) %>%
    mutate(MaxlogMmax = max(logMmax),
           MinlogMmax = min(logMmax),
           MaxInvTkb = max(InvTkb),
           MinInvTkb = min(InvTkb))%>%
    ungroup()
  
  
  nth_element <- function(vector, starting_position, n) { 
    vector[seq(starting_position, length(vector), n)] 
  }
  coli <-sample(nth_element(luv_colours$col, 1, 3)[-c(1:3)])
  
  (p.temperature.corrected = ggplot(MTE_fam, aes(x=logMmax, y=TempCorrectLogK, colour=Family)) +
      geom_point(size=1) +
      scale_colour_manual(values = coli)+
      labs(title="logMmax predictor effect", x="logMmax",y="Temperature-corrected logK") +
      geom_segment(aes(x=MinlogMmax, xend=MaxlogMmax, y=Intercept + SlopeLogMmax*MinlogMmax, 
                       yend=Intercept + SlopeLogMmax*MaxlogMmax,size="Family (random effect)")) +
      geom_segment(aes(x=MinlogMmaxFixe, xend=MaxlogMmaxFixe, 
                       y=InterceptFixe + SlopelogMmaxFixe*MinlogMmaxFixe, 
                       yend=InterceptFixe + SlopelogMmaxFixe*MaxlogMmaxFixe,
                       size="Fish in general (fixed effect)"),colour="black") +
      scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
      
      theme_bw(base_size=10) +
      theme(
        plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold")
      ) +
      labs(title="Temperature predictor effect",
           x='1/kT',
           y="Mass-corrected log K"))
  
  ggsave("figures/Temperature_corrected.pdf",height = 210, width = 297, units = "mm")
  ggsave("figures/Temperature_corrected.png",height = 210, width = 297, units = "mm")
  
  (p.mass.corrected = ggplot(MTE_fam, aes(x=InvTkb, y=MassCorrectLogK, colour=Family)) +
      geom_point(size=1) +
      scale_colour_manual(values = coli)+
      labs(title="InvTkb predictor effect", x="InvTkb",y="Mass-corrected logK") +
      geom_segment(aes(x=MinInvTkb, xend=MaxInvTkb, y=Intercept + SlopeInvTkb*MinInvTkb, 
                       yend=Intercept + SlopeInvTkb*MaxInvTkb,size="Family (random effect)")) +
      geom_segment(aes(x=MinInvTkbFixe, xend=MaxInvTkbFixe, y=InterceptFixe + SlopeInvTkbFixe*MinInvTkbFixe, 
                       yend=InterceptFixe + SlopeInvTkbFixe*MaxInvTkbFixe,
                       size="Fish in general (fixed effect)"),colour="black") +
      scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
      theme_bw(base_size=10) +
      theme(
        plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold")
      )+
      labs(title="Species mass predictor effect",
           x="Log species mass",
           y="Temperature-corrected log K"))
  
  ggsave("figures/Mass_corrected.png",height = 210, width = 297, units = "mm")
  ggsave("figures/Mass_corrected.pdf",height = 210, width = 297, units = "mm")
  

  
  #PLOT MTE
  MTE_gen = gen_model_K %>%
    merge(growth_data,by="Genus") %>%
    #Calculating temperature corrected and mass corrected logK
    mutate(TempCorrectLogK = logK - SlopeInvTkb * InvTkb,
           TempStandardLogK = logK - SlopeInvTkb * InvTkb + SlopeInvTkb * mean(InvTkb),
           MassCorrectLogK = logK - SlopeLogMmax * logMmax,
           MassStandardLogK = logK - SlopeLogMmax * logMmax * mean(logMmax)) %>%
    #max and min logMmax and InvTkb for all fish
    mutate(MaxlogMmaxFixe = max(logMmax),
           MinlogMmaxFixe = min(logMmax),
           MaxInvTkbFixe = max(InvTkb),
           MinInvTkbFixe = min(InvTkb),
           MeanInvTkb = mean(InvTkb),
           MeanlogMmax = mean(logMmax))%>%
    #Fixed effects for all fish according to the fish level (from Morais_fish Rdata)
    mutate(InterceptFixe = 15.1464618,
           SlopelogMmaxFixe = -0.194814,
           SlopeInvTkbFixe = 	-0.3784034) %>%
    #grouping by family and calculating min and max logMmax and InvTkb for each family
    group_by(Genus) %>%
    mutate(MaxlogMmax = max(logMmax),
           MinlogMmax = min(logMmax),
           MaxInvTkb = max(InvTkb),
           MinInvTkb = min(InvTkb))%>%
    ungroup()
  
  
  nth_element <- function(vector, starting_position, n) { 
    vector[seq(starting_position, length(vector), n)] 
  }
  coli <-sample(nth_element(luv_colours$col, 1, 3)[-c(1:3)])
  
  (p.temperature.corrected = ggplot(MTE_gen, aes(x=logMmax, y=TempCorrectLogK, colour=Genus)) +
      geom_point(size=1) +
      scale_colour_manual(values = coli, guide = "none")+
      labs(title="logMmax predictor effect", x="logMmax",y="Temperature-corrected logK") +
      geom_segment(aes(x=MinlogMmax, xend=MaxlogMmax, y=Intercept + SlopeLogMmax*MinlogMmax, 
                       yend=Intercept + SlopeLogMmax*MaxlogMmax,size="Genus (random effect)")) +
      geom_segment(aes(x=MinlogMmaxFixe, xend=MaxlogMmaxFixe, 
                       y=InterceptFixe + SlopelogMmaxFixe*MinlogMmaxFixe, 
                       yend=InterceptFixe + SlopelogMmaxFixe*MaxlogMmaxFixe,
                       size="Fish in general (fixed effect)"),colour="black") +
      scale_size_manual(name="Predictions", values=c("Genus (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
      
      theme_bw(base_size=10) +
      theme(
        plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold")
      ) +
      labs(title="Temperature predictor effect",
           x='1/kT',
           y="Mass-corrected log K"))
  
  ggsave("figures/Temperature_corrected_genus.pdf",height = 210, width = 297, units = "mm")
  ggsave("figures/Temperature_corrected_genus.png",height = 210, width = 297, units = "mm")
  
  (p.mass.corrected = ggplot(MTE_gen, aes(x=InvTkb, y=MassCorrectLogK, colour=Genus)) +
      geom_point(size=1) +
      scale_colour_manual(values = coli, guide = "none")+
      labs(title="InvTkb predictor effect", x="InvTkb",y="Mass-corrected logK") +
      geom_segment(aes(x=MinInvTkb, xend=MaxInvTkb, y=Intercept + SlopeInvTkb*MinInvTkb, 
                       yend=Intercept + SlopeInvTkb*MaxInvTkb,size="Genus (random effect)")) +
      geom_segment(aes(x=MinInvTkbFixe, xend=MaxInvTkbFixe, y=InterceptFixe + SlopeInvTkbFixe*MinInvTkbFixe, 
                       yend=InterceptFixe + SlopeInvTkbFixe*MaxInvTkbFixe,
                       size="Fish in general (fixed effect)"),colour="black") +
      scale_size_manual(name="Predictions", values=c("Genus (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
      theme_bw(base_size=10) +
      theme(
        plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold")
      )+
      labs(title="Species mass predictor effect",
           x="Log species mass",
           y="Temperature-corrected log K"))
  
  ggsave("figures/Mass_corrected_genus.pdf",height = 210, width = 297, units = "mm")
  ggsave("figures/Mass_corrected_genus.png",height = 210, width = 297, units = "mm")

  
  
}
