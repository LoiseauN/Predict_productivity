#' Merging with K from growth database
#'
#' Here we merge K of growth data into our dataset for species thate are present in BOTH datasaets 
#'
#' @param growth_data output from the data_prep function 
#' @param prod_data data to predict productivity on
#' @param gen_model the output from the save_gen_model_K function
#' 
#' @return dataframe with performance of each model
#' @export
#' 
#' 

merge_growth <- function(growth_data,prod_data,gen_model){
  
  gen_model = gen_model %>%
    dplyr::select(Genus,SlopeInvTkb)
  
  #Creating empty columns 
  prod_data$K_growth <- NA
  prod_data$sstmean_growth <- NA
  prod_data$SlopeInvTkb_growth <- NA
  
  for(i in 1:nrow(prod_data)){
    
    print(paste("i =", i," percentage avancement =", round((i/nrow(prod_data))*100, digits = 2) ,"%"))
    
    subgrowth <- growth_data[as.character(growth_data$Species) %in%  prod_data$Species[i],] %>%
      merge(gen_model,by="Genus")
    
    if(nrow(subgrowth)==0) {
      
      prod_data$K_growth[i]    <- NA
      prod_data$sstmean_growth[i] <- NA
      prod_data$SlopeInvTkb_growth[i] <- NA
      
    }
    
    else if(nrow(subgrowth)==1) {    
      
      prod_data$K_growth[i] <- subgrowth$K
      prod_data$sstmean_growth[i] <- subgrowth$sstmean
      prod_data$SlopeInvTkb_growth[i] <- subgrowth$SlopeInvTkb
      
    }
    
    else if(is.na(prod_data$sst[i])) {
      
      prod_data$K_growth[i] <- NA
      prod_data$sstmean_growth[i] <- NA
      prod_data$SlopeInvTkb_growth[i] <- NA
      
    }
    
    else{
      #here if we think that we should limit the difference in temperature between renato and our data
      # if(abs(subgrowth$sstmean - data_fish_Temp$Temperature[i]>5)) {data_fish_Temp$K_growth[i] <- NA }else{
      
      prod_data$K_growth[i] <- subgrowth$K[which.min(abs(subgrowth$sstmean - prod_data$sst[i]))]
      prod_data$sstmean_growth[i] <- subgrowth$sstmean[which.min(abs(subgrowth$sstmean - prod_data$sst[i]))]
      prod_data$SlopeInvTkb_growth[i]<- subgrowth$SlopeInvTkb[which.min(abs(subgrowth$sstmean - prod_data$sst[i]))]
      
      #}
    }
  }
  
  return(prod_data)
  
}
