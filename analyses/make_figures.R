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

#We need these dataframes for some figures
#Productivity at commmunity level but only for kept transects
RLS_prod_figures = RLS_prod_all %>%
  filter(SurveyID %in% RLS_Management$SurveyID)

RLS_prod_transect_figures = RLS_prod %>%
  filter(SurveyID %in% RLS_Management$SurveyID)

#Plot figure representing K by size for each family and for some examples 
K_plots(RLS_prod_figures)

#Plot number of predeictions for K at family, genus and species level
#Check pred_type column for precise number
predictions_by_level(RLS_prod_figures)
  
#Plot comparison of metrics biomass/biomass production/productivity and correlation levels
plot_metrics_comparison(RLS_Management)

#Table with family, genus used in the study
used_taxonomy(RLS_prod_figures)

#World maps of our transects with management information
map_management(RLS_Management)

#Various representation of our biomass/productivity bi-plot
plot_classes(RLS_Management)

#Figure 2 : variable importance in our model
plot_var_imp(model_test)

#Figure 3 : representations between variables and management classes
model_prob(RLS_Management,model_test)

#MTE plots
plot_MTE(data_prepped, fam_model_K)


# 
# (p.temperature.adjusted = ggplot(MTE_fam, aes(x=logMmax, y=TempStandardLogK)) +
#   geom_point(size=1,aes(colour=Family)) +
#   scale_colour_manual(values = coli)+
#   labs(title="logMmax predictor effect", x="logMmax",y="Temperature-adjusted logK") +
#   geom_segment(aes(x=MinlogMmax, xend=MaxlogMmax, 
#                    y=Intercept + SlopeLogMmax*MinlogMmax + SlopeInvTkbFixe*MeanInvTkb, 
#                    yend=Intercept + SlopeLogMmax*MaxlogMmax + SlopeInvTkbFixe*MeanInvTkb,
#                    size="Family (random effect)",colour=Family)) +
#   geom_segment(aes(x=MinlogMmaxFixe, xend=MaxlogMmaxFixe,
#                    y=InterceptFixe + SlopelogMmaxFixe*MinlogMmaxFixe + SlopeInvTkbFixe*MeanInvTkb, 
#                    yend=InterceptFixe + SlopelogMmaxFixe*MaxlogMmaxFixe+ SlopeInvTkbFixe*MeanInvTkb,
#                    size="Fish in general (fixed effect)"),colour="black") +
#   scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
#   theme_bw(base_size=10) +
#   theme(
#     plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
#     axis.title.x = element_text(color="black", size=14, face="bold"),
#     axis.title.y = element_text(color="black", size=14, face="bold")
#   ))
# 
# # make the mass-corrected (growth is corrected at a mean mass) plot
# 
# (p.mass.adjusted = ggplot(MTE_fam, aes(x=InvTkb, y=MassStandardLogK, colour=Family)) +
#   geom_point(size=1) +
#   scale_colour_manual(values = coli) +
#   labs(title="InvTkb predictor effect", x="InvTkb",y="Mass-adjusted logK") +
#   geom_segment(aes(x=MinInvTkb, xend=MaxInvTkb, y=Intercept + SlopeInvTkb*MinInvTkb + SlopelogMmaxFixe*MeanlogMmax, 
#                    yend=Intercept + SlopeInvTkb*MaxInvTkb + SlopelogMmaxFixe*MeanlogMmax,size="Family (random effect)")) +
#   geom_segment(aes(x=MinInvTkbFixe, xend=MaxInvTkbFixe, 
#                    y=InterceptFixe + SlopeInvTkbFixe*MinInvTkbFixe + SlopelogMmaxFixe*MeanlogMmax, 
#                    yend=InterceptFixe + SlopeInvTkbFixe*MaxInvTkbFixe+ SlopelogMmaxFixe*MeanlogMmax,
#                    size="Fish in general (fixed effect)"),colour="black") +
#   scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
#   theme_bw(base_size=10) +
#   theme(
#     plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
#     axis.title.x = element_text(color="black", size=14, face="bold"),
#     axis.title.y = element_text(color="black", size=14, face="bold")
#   ))
