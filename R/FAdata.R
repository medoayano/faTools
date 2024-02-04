#' @title Function1: FAdata
#' @description \code{FAdata} Make data frame for multivariate analysis; Make ID and Species vectors
#'
#' @param dt data frame with ID (column 1), Species (column 2), and FA composition (column 3~n)
#' @param convert Please put "FALSE" here if you don't convert FA composition data to "sqrt"
#' @export

FAdata <- function(dt, convert=TRUE){

  id <- dt[,1]
  species <- dt[,2]
  species_num <- as.numeric(as.factor(species))

  conv.data <- dt[,3:ncol(dt)]

  if(convert == TRUE){
    conv.data <- sqrt(conv.data)
  } else {
    ###
  }
  return(list(
    id <- id,
    species <- species,
    species_num <- species_num,
    conv.data <- conv.data
  ))
}
