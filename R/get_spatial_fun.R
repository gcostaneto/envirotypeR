#'==================================================================================================
#' Title.    : Collecting point estimates from raster files
#' Author.   : G Costa-Neto
#' Created at: 2023-11-15
#' Updated at: 2023-11-15
#' Previous versions: EnvRtype::extract_GIS()
#' Current Version: 0.0.1
#'
#' get_spatial()
#'==================================================================================================


#' @title  Easily Collection of Digital Spatial Data

#' @description Extracts point feature esitmations from large digital image files
#'
#' @author Germano Martins Costa Neto
#'
#' @param digital.raster RasterStack or SpatialRaster file
#' @param which.raster.number numeric, if digital.raster is RasterStack this number denotes which raster to be used
#' @param env.id vector (character or level). Identifimessageion of the site/environment (e.g. Piracicaba01).
#' @param lat vector (numeric). Latitude values of the site/environment (e.g. -13.05) in WGS84.
#' @param lng vector (numeric). Longitude values site/environment (e.g. -56.05) in WGS84.
#' @param env.dataframe data.frame
#' @param merge logical
#' @param name.feature vector (character)
#
#' @importFrom terra rast
#' @importFrom terra extract
#' @importFrom sf st_as_sf
#'
#'
get_spatial <- function(digital.raster =NULL,
                        which.raster.number = NULL,
                        lat =NULL, lng =NULL, env.dataframe=NULL,
                        env.id=NULL,name.feature = NULL, merge=TRUE){




  message("---------------------------------------------------------------")
  message('get_spatial - Collecting point estimates from raster files')
  message('Based on sf::st_as_sf and  terra::extract')
#  message('https://docs.ropensci.org/nasapower')
  message("---------------------------------------------------------------")

  if (!requireNamespace("terra", quietly = TRUE)) {
    utils::install.packages("terra")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    utils::install.packages("raster")
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    utils::install.packages("sf")
  }


  envs_to_pull <- unique(env.dataframe[,env.id])
  startTime <- Sys.time()
  message(paste0('Start at...........................', format(startTime, "%a %b %d %X %Y"),'\n'))
  message(paste0('Number of Environmental Units ........',length( envs_to_pull)))

  if(length(digital.raster) == 1 )  digital.raster <- digital.raster
  if(!is.null( which.raster.number ))
  {
    digital.raster <- digital.raster[[ which.raster.number ]]
  }

  if(!is.null(name.feature))  names(digital.raster) = name.feature
  if(is.null(name.feature)) name.feature = names(digital.raster)
  if(is.null(lat)) message('Error: provide the latitude column name for lat in env.dataframe')
  if(is.null(lng)) message('Error: provide the longitude column name for lng in env.dataframe')
  if(is.null(env.id)) message('Error: provide the environmental id column name for env.id in env.dataframe')

  if(is.numeric(lat) & is.numeric(lng))
  {
    coords    <- data.frame(x = lng, y = lat)
  }
  else{
    coords    <- data.frame(x = env.dataframe[,lng], y = env.dataframe[,lat])
  }

 # sp::proj4string(  coords) = sp::CRS("+proj=longlat +datum=WGS84")
  sp_vector <- sf::st_as_sf(coords)


  if(isFALSE(class(digital.raster)[1] == "SpatRaster")) digital.raster = terra::rast(digital.raster)
  extracted_values <- terra::extract(digital.raster , sp_vector)


  if(!is.null(env.id))
  {
    if(is.null(env.dataframe))
    {
      extracted_values$ID = env.id
    }

    if(!is.null(env.dataframe))
    {
      extracted_values$ID = env.dataframe[,env.id]
      names(extracted_values)[1] = env.id

      if(isTRUE(merge))
      {
        extracted_values = merge(env.dataframe,extracted_values,by=env.id)
      }
    }
  }

  endTime <- Sys.time()
  message(paste0('\nEnvironmental Units downloaded.......',  length(unique( extracted_values [,1])),'/',length( envs_to_pull)))
  message(paste0('Digital Raster Features..............',  length(name.feature)))
  message(paste0('Environmental data...................',  length(name.feature)* length(unique( extracted_values [,1])),'\n'))

  message(paste0('Done!..............................', format(endTime, "%a %b %d %X %Y")))
  message(paste0('Total Time.........................',  round(difftime(endTime,startTime,units = 'secs'),2),'s'))


  return(extracted_values)
}
