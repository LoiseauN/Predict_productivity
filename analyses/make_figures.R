#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot","rnaturalearth","plotly","ggridges","ggbeeswarm","ggforce","rfishbase",
          "parameters","NbClust","cluster","klaR","beepr","gstat",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","arm","fishualize","wpp2019","gridExtra","ggpubr","scales","magrittr","hrbrthemes")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#----------------Loading results------------------------

path = (here::here("data"))
setwd(path)
files <- list.files(here::here("data"),pattern = ".RData|.Rdata")
data_list = lapply(files, load, .GlobalEnv)


path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"),pattern = ".RData|.Rdata")
data_list = lapply(files, load, .GlobalEnv)

#-------------Loading code for plots---------------

path = (here::here("R_figures"))
setwd(path)
files.source = list.files(here::here("R_figures"))
sapply(files.source, source)

#-------------Plots--------------------------------

setwd(here())

RLS_prod_figures = RLS_prod_all %>%
  filter(SurveyID %in% RLS_Management$SurveyID)

RLS_prod_transect_figures = RLS_prod %>%
  filter(SurveyID %in% RLS_Management$SurveyID)

RLS_Management_degraded = RLS_Management %>%
  filter(Class == "deadzone")
RLS_Management_partial = RLS_Management %>%
  filter(Class == "partial")
RLS_Management_pristine = RLS_Management %>%
  filter(Class == "pristine")
RLS_Management_transition = RLS_Management %>%
  filter(Class == "transition")



#Number of reef fish taxonolmy
length(unique(RLS_prod_figures$Species))
length(unique(RLS_prod_figures$Family))
length(unique(RLS_prod_figures$Genus))

#Number of transects, sites, countries
length(unique(RLS_Management$Country))

#How much does each management class have
summary(RLS_Management)

121/3739
186/3739
902/3739
2506/3739

#Plot S2S3 Figure
K_by_size(RLS_prod_figures)

(K_by_family = ggplot(RLS_prod_figures,aes(reorder(Family,-log(K_pred+1)),log(K_pred+1),fill=Family,colour=Family))+
  geom_jitter(size = 0.1, alpha = 0.2)+
  geom_boxplot(alpha = 0.7)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+ 
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs( y = "Estimated growth rate K (log scale)",
        x = ""))

ggsave("Figures/K_by_family.pdf",height=210,width=297, units = "mm")
ggsave("Figures/K_by_family.png",height=210,width=297, units = "mm")

length(unique(RLS_prod_figures$Family))

#Plot number of predictions by class

ggplot(RLS_prod_figures)+
  geom_histogram(bins = 30,aes(K_pred,fill=pred_type))+
  scale_fill_viridis_d()+
  theme_classic()+
  labs(x = "Predicted growth rates K",
       y= "Number of predictions",
       fill = "Prediction level")

RLS_prod_figures$pred_type = as.factor(RLS_prod_figures$pred_type)
summary(RLS_prod_figures)

ggsave("Figures/predictionslevels.pdf",height=210,width=297,units="mm")
ggsave("Figures/predictionslevels.png",height=210,width=297,units="mm")

#Plot Figure S4
plot_metrics_comparison(RLS_Management)

#Supp Table 1
SuppTable1(RLS_prod_figures)

#Figure 1
plot_classes(RLS_Management)

 #Figure 2
map_management(RLS_Management)

#Figure 3
plot_var_imp(model_test)

#Figure4 
model_prob(RLS_Management,model_test)

group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "#ABDDDE")

group.colors2 <- c(partial = "#046c9a", transition = "#ABDDDE")


  
#PLOT MTE
MTE_fam = fam_model_K %>%
    merge(data_prepped,by="Family") %>%
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
    mutate(InterceptFixe = 20.11202,
           SlopelogMmaxFixe = -0.2305791,
           SlopeInvTkbFixe = 	-0.5000861) %>%
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

ggsave("figures/Mass_corrected.pdf",height = 210, width = 297, units = "mm")



ggsave("figures/Temperature_corrected.pdf",height = 120, width = 297, units = "mm")


(p.temperature.adjusted = ggplot(MTE_fam, aes(x=logMmax, y=TempStandardLogK)) +
  geom_point(size=1,aes(colour=Family)) +
  scale_colour_manual(values = coli)+
  labs(title="logMmax predictor effect", x="logMmax",y="Temperature-adjusted logK") +
  geom_segment(aes(x=MinlogMmax, xend=MaxlogMmax, 
                   y=Intercept + SlopeLogMmax*MinlogMmax + SlopeInvTkbFixe*MeanInvTkb, 
                   yend=Intercept + SlopeLogMmax*MaxlogMmax + SlopeInvTkbFixe*MeanInvTkb,
                   size="Family (random effect)",colour=Family)) +
  geom_segment(aes(x=MinlogMmaxFixe, xend=MaxlogMmaxFixe,
                   y=InterceptFixe + SlopelogMmaxFixe*MinlogMmaxFixe + SlopeInvTkbFixe*MeanInvTkb, 
                   yend=InterceptFixe + SlopelogMmaxFixe*MaxlogMmaxFixe+ SlopeInvTkbFixe*MeanInvTkb,
                   size="Fish in general (fixed effect)"),colour="black") +
  scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
  theme_bw(base_size=10) +
  theme(
    plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  ))

# make the mass-corrected (growth is corrected at a mean mass) plot

(p.mass.adjusted = ggplot(MTE_fam, aes(x=InvTkb, y=MassStandardLogK, colour=Family)) +
  geom_point(size=1) +
  scale_colour_manual(values = coli) +
  labs(title="InvTkb predictor effect", x="InvTkb",y="Mass-adjusted logK") +
  geom_segment(aes(x=MinInvTkb, xend=MaxInvTkb, y=Intercept + SlopeInvTkb*MinInvTkb + SlopelogMmaxFixe*MeanlogMmax, 
                   yend=Intercept + SlopeInvTkb*MaxInvTkb + SlopelogMmaxFixe*MeanlogMmax,size="Family (random effect)")) +
  geom_segment(aes(x=MinInvTkbFixe, xend=MaxInvTkbFixe, 
                   y=InterceptFixe + SlopeInvTkbFixe*MinInvTkbFixe + SlopelogMmaxFixe*MeanlogMmax, 
                   yend=InterceptFixe + SlopeInvTkbFixe*MaxInvTkbFixe+ SlopelogMmaxFixe*MeanlogMmax,
                   size="Fish in general (fixed effect)"),colour="black") +
  scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
  theme_bw(base_size=10) +
  theme(
    plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  ))

# 
# 
# #Prepping SAU Data
# SAU_Data = resSAU_raw_2021 %>%
#   group_by(eez_Name, sector_type_name,year) %>%
#   summarise(sum = sum(catch_sum)) %>%
#   summarise(summean = mean(sum)) %>%
#   ungroup() %>%
#   rename(Country = "eez_Name")%>%
#   mutate(Country = as.character(Country)) 
# 
# #Correcting specific countries
# SAU_Data$Country[str_detect(SAU_Data$Country,"Costa_Rica")==T] = "Costa Rica" 
# SAU_Data$Country[str_detect(SAU_Data$Country,"Panama")==T] = "Panama" 
# SAU_Data$Country[str_detect(SAU_Data$Country,"Mexico")==T] = "Mexico" 
# SAU_Data$Country[str_detect(SAU_Data$Country,"Spain")==T] = "Spain" 
# SAU_Data$Country[str_detect(SAU_Data$Country,"Indonesia")==T] = "Indonesia" 
# SAU_Data$Country[str_detect(SAU_Data$Country,"Nicaragua")==T] = "Nicaragua" 
# SAU_Data$Country[str_detect(SAU_Data$Country,"Timor")==T] = "East Timor"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Turk")==T] = "Turks and Caicos Islands"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Marshall")==T] = "Republic of the Marshall Islands"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Papua")==T] = "Papua New Guinea"
# SAU_Data$Country[str_detect(SAU_Data$Country,"French_Polynesia")==T] = "French Polynesia"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Pitcairn")==T] = "Pitcairn Islands"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Chile")==T] = "Chile"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Solomon")==T] = "Solomon Islands"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Japan")==T] = "Japan"
# SAU_Data$Country[str_detect(SAU_Data$Country,"American_Samoa")==T] = "American Samoa"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Cook_Islands")==T] = "Cook Islands"
# SAU_Data$Country[str_detect(SAU_Data$Country,"Egypt")==T] = "Egypt"
# 
# SAU_Final = SAU_Data %>%
#   group_by(Country,sector_type_name) %>%
#   summarise(Catch = mean(summean))
# 
# save(SAU_Final,file="outputs/SAU_final.Rdata")
# 
# data(pop)
# 
# country_pop = pop %>%
#   rename(Country = "name") %>%
#   dplyr::rename(pop="2020") %>%
#   dplyr::select(c(Country,pop))
# 
# #Merging SAU with our data
# RLS_Management_SAU = RLS_Management %>%
#   left_join(SAU_Final, by = "Country") %>%
#   filter(Class == "transition" | Class == "partial") %>%
#   filter(Effectiveness == "No_Mpa") %>%
#   pivot_wider(names_from = sector_type_name,values_from = Catch) %>%
#   left_join(country_pop,by="Country") %>%
#   mutate(Catch_ratio = (Artisanal + Subsistence)/Industrial,
#          Catch_pop = (Subsistence)/pop,
#          logCatch = log(Catch_pop) + min(Catch_pop, na.rm = T)) %>%
#   mutate(stdgrav= log(gravtot2) + min(gravtot2,na.rm = T)) %>%
#   mutate(Category = as.factor(ifelse(stdgrav < -5 & logCatch < -1,'IUCN1',ifelse(stdgrav < 3 & logCatch < 1, "IUCN2",ifelse(stdgrav < 6 & logCatch<  2.5, "IUCN3","IUCN")))))
# 
