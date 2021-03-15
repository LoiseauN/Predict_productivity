#' Defining function Linf = a*MaxSize in order to estimate Tinf
#'
#' @param growth_data Maxsize and a
#' 
#' @return dataframe with performance of each model
#' @export
#' 
#' 

#Defining Linf = a * Maxsize function                     
MaxSize.fct=function(a,x) {
  y=a*x
  y
}
