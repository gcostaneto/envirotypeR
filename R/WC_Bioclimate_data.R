#' @title  Wordclim Bioclimatic Variables
#'
#'
#'@description A collection of 19 bioclimatic variables from WorldClim. Check <https://www.worldclim.org/data/bioclim.html>
#'
#'@docType data
#'
#'@usage data(WC_Bioclimate)
#'

#'@examples
#'\dontrun{
#' data(WC_Bioclimate)
#' require(raster)
#' plot(WC_Bioclimate[[1]]) # plot the fister layer
#' names(WC_Bioclimate)
#'}
#' @format A RasterStack file

#' @references
#' Fick, S.E. and R.J. Hijmans. _WorldClim 2: new 1km spatial resolution climate surfaces for global land areas_. International Journal of Climatology 37 (12): 4302-4315.

#'


"WC_Bioclimate"
