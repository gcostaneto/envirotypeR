% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_soil_fun.R
\name{get_soil}
\alias{get_soil}
\title{Easily Collection of Soil Features (250m res)}
\usage{
get_soil(
  env.id,
  lat,
  lon,
  home.path = NULL,
  variables.names = "clay",
  sleep = 15,
  logfile = NULL
)
}
\arguments{
\item{env.id}{vector (character or level). Identification of the site/environment (e.g. Piracicaba01).}

\item{lat}{vector (numeric). Latitude values of the site/environment (e.g. -13.05) in WGS84.}

\item{lon}{vector (numeric). Longitude values site/environment (e.g. -56.05) in WGS84.}

\item{variables.names}{vector (character).}

\item{properties}{A SoilGrid property. Defaults to \code{clay}. Possible}

\item{verbose}{Report messages to console? Defaults to \code{FALSE}.}
}
\value{
A `data.frame` object
}
\description{
Imports soil features from SoilGrids data set
}
\details{
==================================================================================================
Title.    : Collecting soil data from SoilGrids
Author.   : G Costa-Neto, based on B. Monier codes
Created at: 2022-09-15
Updated at 2024-02-24 (envirotypeR version 0.1.3)
Previous versions: -
Current Version: 0.1.3 (envirotypeR)

get_soil()
==================================================================================================
}
\references{
Poggio et al (2021). _SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty_. European Geosciences Union, <https://doi.org/10.5194/soil-7-217-2021>.
}
\author{
Brandon Monier, modified by Germano Costa Neto
}