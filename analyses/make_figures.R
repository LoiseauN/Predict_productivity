#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","nlme","cowplot","rnaturalearth","plotly","ggridges","ggbeeswarm","ggforce","rfishbase",
          "parameters","NbClust","cluster","klaR","beepr",
          "harrypotter","wesanderson","dichromat","ranger","ggpubr","data.table","pdp","png","sf","broom.mixed","arm","fishualize")
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

#--------------Loading SHP-------------------------

setwd(here())
nc_map = st_read("shp/NewCaledonia_v7.shp")

#-------------Plots--------------------------------

setwd(here())

test = fish %>% filter(Num == 4500)
boxplot(data_forproduction$Biomass)

RLS_temp = data_prod_brut %>%
  dplyr::select(SurveyID,Biom,Prod,Productivity,log10Prod)

RLSwithprod = RLS_Management %>%
  left_join(RLS_temp, by = "SurveyID")


max(data_forproduction$K_pred,na.rm=T)
min(data_forproduction$K_pred,na.rm=T)

min(RLSwithprod$Biom)
max(RLSwithprod$Biom)

min(RLSwithprod$Prod)
max(RLSwithprod$Prod)

min(RLSwithprod$Productivity)
max(RLSwithprod$Productivity)

truc = RLSwithprod %>% filter(Class == "transition")

mean(truc$Productivity)
sd(truc$Productivity)
(4757/6840)*100

length(unique(data_forproduction$Species))

#Plot S2S3 Figure
K_by_size(data_forproduction)

#Plot Figure S4
plot_metrics_comparison(RLSwithprod)

#Supp Table 1
SuppTable1(data_forproduction)

#Figure 1
plot_classes(RLS_Management)

#Figure 2
map_management(RLS_Management)

#Figure 3
plot_var_imp(model_test)

#Figure4 
model_prob(RLS_Covariates,model_test)

group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "#ABDDDE")

(Boxplot = RLS_Management %>%
  filter(Country != "Australia") %>%
  ggplot(aes(Class,c,fill=Class,color=Class))+
  geom_jitter(size = 1,alpha = 0.5)+
  geom_violin()+
  scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  scale_fill_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  theme_minimal())

Noaustralia = RLS_Management %>% filter(Country != "Australia")

upper = quantile(Noaustralia$HDI,0.75)

hist(Noaustralia$MarineEcosystemDependency)

(Density = RLS_Management %>%
  # filter(Country != "Australia") %>%
  ggplot(aes(x = MarineEcosystemDependency, y = Class, fill = Class))+
  geom_density_ridges()+
  # geom_vline(data=RLS_Management,aes(xintercept = 0.8))+
  # geom_vline(data=RLS_Management,aes(xintercept = 0.85))+
  scale_fill_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  theme_ridges())

RLS_Management %>%
  filter(Class == "transition" | Class == "partial") %>%
  mutate(HDI_Sep = ifelse(HDI>0.85,"Sanctuary","Partial protection"))%>%
  ggplot(aes(log10Biom,log10ProdB))+
  geom_mark_hull(aes(fill=Class,label=Class),con.cap=0)+
  scale_fill_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  geom_point(size=3,alpha=0.4,aes(color=HDI_Sep))+
  scale_color_hp_d(option="NewtScamander")+
  labs(x= "Biomass",
       y= "Productivity",
       color = "HDI")+
  theme_minimal()

RLS_Management %>%
  filter(Class == "transition" | Class =="partial") %>%
  mutate(HDI_Sep = ifelse(HDI>0.85,"Sanctuary","Partial protection"))%>%
  ggplot(aes(log10Biom,log10ProdB))+
  geom_mark_hull(aes(fill=Class,label=Class),con.cap=0)+
  scale_fill_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  geom_point(size=4,alpha=0.7,aes(color=log(gravtot2)))+
  scale_color_gradient2(low = "green",mid="white", high ="red")

hist(log(RLS_Management$gravtot2))

meanDep= mean(RLS_Management$MarineEcosystemDependency,na.rm=T)
meanGrav = mean(log(RLS_Management$gravtot2+1),na.rm=T)

RLS_Management %>%
  filter(Class == "transition" | Class == "partial") %>%
  filter(Effectiveness == "No_Mpa") %>%
  mutate(loggravtot = log(gravtot2+1)) %>%
  mutate(HDI_Sep = ifelse(MarineEcosystemDependency>meanDep & loggravtot < meanGrav, "No take", ifelse(MarineEcosystemDependency < meanDep & loggravtot < meanGrav, "No entry", ifelse(MarineEcosystemDependency > meanDep& loggravtot > meanGrav, "OECM", "No take"))))%>%
  ggplot(aes(MarineEcosystemDependency,loggravtot,color=HDI_Sep))+
  geom_point()+
  scale_color_hp_d(option="LunaLovegood")+
  geom_hline(yintercept=meanGrav,linetype="dashed")+
  geom_vline(xintercept=meanDep,linetype="dashed")+
  theme_minimal()+
  labs(x= "Marine Ecosystem Dependency",
       y= "Gravity",
       color = "Proposed Management")

ggsave("figures/managementcutoff.png", width = 297, height = 210,units="mm")
ggsave("figures/managementcutoff.pdf", width = 297, height = 210,units="mm")

RLS_Management %>%
  filter(Class == "transition" | Class == "partial") %>%
  filter(Effectiveness == "No_Mpa") %>%
  mutate(loggravtot = log(gravtot2+1)) %>% 
  mutate(HDI_Sep = ifelse(MarineEcosystemDependency>meanDep & loggravtot < meanGrav, "No take", ifelse(MarineEcosystemDependency < meanDep & loggravtot < meanGrav, "No entry", ifelse(MarineEcosystemDependency > meanDep& loggravtot > meanGrav, "OECM", "No take"))))%>%
  ggplot(aes(log10Biom,log10ProdB))+
  scale_fill_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  geom_point(size=3,alpha=0.4,aes(color=HDI_Sep))+
  scale_color_hp_d(option="LunaLovegood")+
  labs(x= "Biomass",
       y= "Productivity",
       color = "Proposed Management")+
  theme_minimal()

ggsave("figures/managementmetrics.png",width = 297, height = 210,units="mm")
ggsave("figures/managementmetrics.pdf",width = 297, height = 210,units="mm")


RLS_proposed = RLS_Management %>%
  filter(Effectiveness == "No_Mpa") %>%
  mutate(loggravtot = log(gravtot2+1)) %>%
  filter(Class == "transition" | Class == "partial") %>%
  mutate(Prop = ifelse(MarineEcosystemDependency>meanDep & loggravtot < meanGrav, "Notake", ifelse(MarineEcosystemDependency < meanDep & loggravtot < meanGrav, "Noentry", ifelse(MarineEcosystemDependency > meanDep& loggravtot > meanGrav, "OECM", "Notake"))),
         Prop = as.factor(Prop))

RLS_Country = RLS_Management %>% dplyr::select(MarineEcosystemDependency,Country) %>% distinct(Country, .keep_all = T) %>% rename(region = "Country")

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% left_join(RLS_Country,by="region") %>% fortify


(sites <- st_as_sf(RLS_proposed, coords = c("SiteLongitude", "SiteLatitude"), 
                   crs = 4326, agr = "constant"))

group.colors.prop <- c(Noentry = "#144D49", Notake = "#73C1C4", OECM = "#830342")

pal <- wes_palette("Zissou1", 4, type = "continuous")
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region,fill = MarineEcosystemDependency),
           colour = "black", size=0.2) +
  scale_fill_gradientn(colours = pal, na.value = "lightgrey") +
  geom_point(data = RLS_proposed, size = 3, alpha = 0.6, aes(x=SiteLongitude, y= SiteLatitude, colour=Prop))+
  scale_color_manual(values=group.colors.prop, labels = c("No entry", "No take", "OECM/IUCN"))+
  theme_void()+
  theme(plot.background = element_rect(color="white"))+
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
  labs(fill = "Marine ecosystem dependency",
      color = "Proposed Management")

ggsave("figures/Managementmap.pdf", width = 24.00, height = 15.75, units= "in",dpi= 600)

