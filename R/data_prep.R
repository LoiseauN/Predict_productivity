#' Prep data for MTE calculations : predictions of growth rate and Linf
#'
#' This function preps your data for necessary parameters for MTE 
#'
#' @param growth_data
#' 
#' @return dataset with required parameters to calculate MTE growth metrics
#' @export
#' 
#' 
#' 

data_prep <- function(data){

  data_prepped = data %>%
    #convert temperature to kelvin
    dplyr::mutate(sstmean = sstmean+273.5)%>%
    #Calcultating metrics of MTE
    dplyr::mutate(Mmax = a * MaxSize ^ b,
                  logMmax = log(Mmax),
                  logK = log(K),
                  logLmax = log(MaxSize),
                  InvTkb = 1/((sstmean)*8.62e-05)) %>%
    #Convert categorical to factors
    dplyr::mutate(Species = as.factor(Species),
                  Genus = as.factor(Genus),
                  Family = as.factor(Family),
                  Diet = as.factor(Diet))
}
