calc_prod_rfishprod <- function(data_final){

  # Formula from Morais and Bellwood (2018) #
  fmod <- formula(~ sstmean + MaxSizeTL)
  
  traits_trophic = traits %>%
    dplyr::mutate(Species = sub(" ", "_", CURRENT_SPECIES_NAME)) %>%
    dplyr::select(Species,Trophic.group ) 
  
  data_final_prod = data_final %>% 
    #Selecting columns of interest
    dplyr::select(SurveyID,Num,Family,Genus,Species, Sizeclass,Temperature,lwa,lwb,MaxLength,Area) %>%
    #Last gobie family left, deleting it
    dplyr::filter(Family != "Gobiesocidae") %>%
    #Renaming for rfishprod compatibliy
    dplyr::rename(MaxSizeTL = "MaxLength",
                  sstmean = "Temperature",
                  Size = "Sizeclass",
                  a = "lwa",
                  b = "lwb") %>%
    mutate(Size = ifelse(Size >= MaxSizeTL,MaxSizeTL,Size))


  # Check dataset repdata #
  # (repdata <- rfishprod:::repdata)
  # 
  # # Getting levels ready #
  # repdata <- rfishprod::tidytrait (repdata, db)
  # data_final_prod = repdata
  
  datagr <- rfishprod::predKmax(data_final_prod,
                      dataset = db, 
                      fmod = fmod,
                      niter = 10,
                      return = 'pred')
  
  datagr <- datagr$pred
  
  # Predicting M/Z: the instantaneous mortality rate (Recommendation: see help file for) #
  datagr$Md <- with (datagr, rfishprod::predM(Lmeas = Size,
                            Lmax = MaxSizeTL,
                            Kmax = Kmax,
                            t = 1,
                            method = 'Gislason'))
  
  # Positioning your fish in their growth trajectory #
  # aka. what's the size they're supposed to have on the next day? #
  datagr$Size_nextday= with(datagr, rfishprod::applyVBGF (Lmeas = Size,
                                                 Lmax = MaxSizeTL,
                                                 Kmax = Kmax,
                                                 t = 1))
  
  # Estimating gross somatic growth (g) #
  datagr$somatic_growth = with(datagr, rfishprod::somaGain (a = a,
                                                 b = b,
                                                 Lmeas = Size,
                                                 Lmax = MaxSizeTL,
                                                 Kmax = Kmax,
                                                 t = 1))
  
  range(datagr$somatic_growth)
  
  # Applying stochastic mortality #
  datagr$mortality = rfishprod::applyMstoch(datagr$Md,t=1)
  
  
  #Alternatively, estimating per capita mass loss due to mortality #
  datagr$soma_loss = with(datagr, rfishprod::somaLoss (M = Md,
                                            Lmeas = Size,
                                            a = a,
                                            b = b,
                                            t = 1))
  

  datagr_prod = datagr %>%
    #Calculating biomass turnover
    mutate(W = a*(Size^b),
           Biom = (W*Num),
           Prod = ifelse(mortality == T, (somatic_growth * Num),0))
  
  return(datagr_prod)
  
}
  