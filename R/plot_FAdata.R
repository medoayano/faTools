#' @title Function2: plot_FAdata
#' @description \code{plot_FAdata} Make nMDS plot
#'
#' @importFrom vegan vegdist
#' @importFrom vegan metaMDS
#' @importFrom vegan envfit
#' @importFrom vegan ordiplot
#'
#' @param conv.data data frame made by "FAdata" function
#' @param method Please select dissimilarity index, such as "bray", "euclidean", "chao", "morisita" ...
#' @param print_stress Please put "TRUE" here if you see results of stress value
#' @param print_fit Please put "TRUE" here if you see table of vector analysis
#' @param plot_vector Please put "TRUE" here if you print vectors on nMDS plot
#' @param plot_legend Please put "TRUE" here if you print legend on nMDS plot
#' @export

plot_FAdata <- function(conv.data, method,
                        print_stress = FALSE, print_fit = FALSE,
                        plot_vector = FALSE, plot_legend = FALSE){

  dist <- vegdist(conv.data,method)
  mds <- metaMDS(dist,k=2)

  if(print_stress == TRUE){
    print(mds$stress)
  } else {
    ###
  }
  if(mds$stress > 0.2){
    stop(paste("***Error: NOT good fitting. Please confirm your data set"))
  } else {
    ###
  }

  envNMDS <- envfit(mds,conv.data)

  if(print_fit == TRUE){
    print(envNMDS)
  } else {
    ###
  }

  cols <- c("#2b927b","#6977ee","#d240d9","#e64c44","#7c8929","#2c9544",
            "#358ab4","#9966ee","#e34392","#a97932","#4c932b")
  shape <- c(0,15,1,16,2,17,5,18,3,4,6)

  ordiplot(mds,type="n")
  points(mds, pch=shape[species_num], col=cols[species_num], display="sites")

  if(plot_vector == TRUE){
    plot(envNMDS, col="black", p.max = 0.05)
  } else {
    ###
  }

  if(plot_legend == TRUE){
    legend("topright",legend=c(unique(species)),
           col=cols[(unique(species_num))],
           pch=shape[unique(species_num)])
  } else {
    ###
  }
}
