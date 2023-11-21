#'==================================================================================================
#' Title.    : Collecting soil data from SoilGrids
#' Author.   : G Costa-Neto, based on B. Monier codes
#' Created at: 2022-09-15
#' Updated at 2023-11-21 (envirotypeR version 0.1.1)
#' Previous versions: -
#' Current Version: 0.1.1 (envirotypeR)
#'
#' get_soil()
#'==================================================================================================
#'
#'@title  Easily Collection of Soil Features (250m res)
#'
#'
#' @description Imports soil features from SoilGrids data set
#'
#' @author Brandon Monier, modified by Germano Costa Neto
#'
#' @param env.id vector (character or level). Identification of the site/environment (e.g. Piracicaba01).
#' @param lat vector (numeric). Latitude values of the site/environment (e.g. -13.05) in WGS84.
#' @param lon vector (numeric). Longitude values site/environment (e.g. -56.05) in WGS84.
#' @param variables.names vector (character).
#' @param properties A SoilGrid property. Defaults to \code{clay}. Possible
#' @param verbose Report messages to console? Defaults to \code{FALSE}.
#'
#' @return A `data.frame` object
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#'

#' @references
#' Poggio et al (2021). _SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty_. European Geosciences Union, <https://doi.org/10.5194/soil-7-217-2021>.
#'
#' @importFrom utils write.csv
#' @importFrom nasapower get_power
#' @importFrom plyr ldply
#' @importFrom utils install.packages
#' @importFrom raster getData
#'
#----------------------------------------------------------------------------------------
# getting soil data from SoiGrids
#----------------------------------------------------------------------------------------


get_soil = function(env.id,
                    lat,
                    lon,
                    home.path = NULL,  # home directory. If null, getwd()
                    output.path = NULL,
                    variables.names=NULL,
                    n.core = NULL, #  numeric value denoting the numbe of cores.
                    output_name = NULL, # charcter denoting the name of the model under test
                    parallel = TRUE,
                    logfile=NULL) # if is TRUE, and n.core is null, then use detectCores() - 1)
{


  if (!requireNamespace("parallel", quietly = TRUE)) {
    utils::install.packages("doParallel")
  }

  if (!requireNamespace("foreach", quietly = TRUE)) {
    utils::install.packages("foreach")
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    utils::install.packages("jsonlite")
  }

  library(jsonlite, include.only = c("fromJSON"))
  library(httr, include.only = c("content", "GET"))

  if(is.null(logfile)) logfile <-'get_soil_log.txt'
  # === Utility =======================================================

  ## General parameters (that can be integrated into funtion) ----
  configParams <- list(
    urlTemplate = "https://rest.isric.org/soilgrids/v2.0/properties/query?lon=%f&lat=%f%s&depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm&value=Q0.05&value=Q0.5&value=Q0.95&value=mean&value=uncertainty",
    acceptedProps = c(
      "bdod",
      "cec",
      "clay",
      "cfvo",
      "nitrogen",
      "ocd",
      "ocs",
      "phh2o",
      "sand",
      "silt",
      "soc"
    )
  )

  formatSoilGrid <- function(
    json = NULL
  ) {
    if (is.null(json)) stop("Missing JSON data.")
    if (!is.list(json)) stop("This does not appear to be JSON data.")
    if (is.null(json$properties$layers)) stop("This does not appear to be SoilGrid data.")

    tmpProc <- lapply(seq_len(length(json$properties$layers$depths)), function(i) {
      tmpDf      <- json$properties$layers$depths[[i]]

      tmpDf <- cbind(tmpDf$label, tmpDf$values)

      tmpDf$name <- json$properties$layers$name[i]
      tmpDf$unit <- json$properties$layers$unit_measure$mapped_units[i]
      tmpDf$lon  <- json$geometry$coordinates[1]
      tmpDf$lat  <- json$geometry$coordinates[2]

      colnames(tmpDf) <- c(
        "depth", "q_5", "q_50", "q_95",
        "mean", "uncertainty", "property",
        "unit", "lon", "lat"
      )
      return(tmpDf)
    })

    return(do.call("rbind", tmpProc))
  }


  getSoilData <- function(
    lon = 0,
    lat = 0,
    properties = "clay",
    verbose = F
  ) {
    if (is.null(url)) stop("Missing URL signature.")

    if (any(!properties %in% configParams$acceptedProps)) stop("Incorrect properties.")

    url <- configParams$urlTemplate
    propString <- paste("&property=", properties, collapse = "", sep = "")

    finalURL <- sprintf(url, lon, lat, propString)

    if (verbose) message("Getting query...")

    getReq  <- httr::GET(finalURL)
    getReq2 <- httr::content(getReq, "text", encoding = "ISO-8859-1")

    jsonReq <- jsonlite::fromJSON(getReq2)

    if (verbose) message("Finished (", round(jsonReq$query_time_s, 3), "s)")

    return(formatSoilGrid(jsonReq))
  }




  message("------------------------------------------------")
  message('get_soil() - Pulling Soil Features from SoilGrids')
  message('Connecting to the API client')
  message('https://soilgrids.org')
  message("------------------------------------------------  \n")



  if(is.null(home.path)) home.path = getwd()
  if(is.null(   output.path))    output.path = getwd()

  if (is.null(variables.names)) {
    variables.names = c("bdod",
                        "cec",
                        "clay",
                        "nitrogen",
                        "phh2o",
                        "sand",
                        "soc")

  }



  if(isTRUE(parallel))
  {
    if(is.null( n.core))
    {
      if(isTRUE(parallel))
      {
        n.core <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(n.core)
        doParallel::registerDoParallel(cl)
      }
    }
    if(!is.null(n.core))
    {
      cl <-  parallel::makeCluster(n.core)
      doParallel::registerDoParallel(cl)
    }
    #  message(paste0('   threads = ',n.core))
  }
  if(isFALSE(parallel))
  {
    # message(paste0('   parallel = FALSE',n.core))
  }

  envs_to_pull <- unique(env.id)
  startTime <- Sys.time()
  message(paste0('Start at...........................', format(startTime, "%a %b %d %X %Y"),'\n'))
  message(paste0('Number of Environmental Units ........',length( envs_to_pull)))
  message(paste0('Parallelization.....................[ ',ifelse(isTRUE(parallel),'x',''),' ]'))
  if(isTRUE(parallel))      message(paste0('Number of threads...................[ ',n.core,' ]'))



  temp = foreach::foreach(env.j = 1:length( envs_to_pull ), .combine = "rbind",.errorhandling="pass",
                          .packages = c('tidyverse','reshape2','httr','plyr','jsonlite')) %dopar%
    {
      output_soil =   getSoilData(lon = lon[env.j],
                                  lat = lat[env.j],
                                  properties = variables.names)[,c('depth','property','mean','uncertainty')]

      if(nrow(output_soil) > 0)
      {
        output_soil <- data.frame(   output_soil)

        output_soil$env <- envs_to_pull[env.j]
        output_soil <-data.frame(  output_soil)
        output_soil$depth<-gsub(  output_soil$depth,pattern='-',replacement='_')
        output_soil$feature <- paste0( output_soil$property,'|', output_soil$depth)

        # output_unce <- reshape2::dcast(output,env~feature,value.var = 'uncertainty')
      }

      return( output_soil)



    }

  if(isTRUE(parallel)) {parallel::stopCluster(cl) }
  output_soil_mean <- reshape2::dcast(   temp,env~feature,value.var = 'mean')

  endTime <- Sys.time()
  message(paste0('Environmental Units downloaded ......',  length(unique(  output_soil_mean$env)),'/',length( envs_to_pull)))
  message(paste0('Soil Features downloaded.............',  ncol(  output_soil_mean )-1),'\n')
  message(paste0('Environmental data...................',  length(unique(  output_soil_mean$env))*(ncol(  output_soil_mean )-1)),'\n')

  message(paste0('Done!..............................', format(endTime, "%a %b %d %X %Y")))
  message(paste0('Total Time.........................',  round(difftime(endTime,startTime,units = 'secs'),3)))
  #  message("------------------------------------------------  \n")


  return(  output_soil_mean )

}







