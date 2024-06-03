#' Example of W matrix using G2F data
#'
#'@description  An example of W matrix of environmental features using some weather variables for G2F. Genomes to fields (G2F) is a multi-institutional, public collaborative to develop information and tools that support the
#'translation of maize (Zea mays L.) genomic information into relevant phenotypes for the benefit of growers, consumers,
#'and society. Building on existing maize genome sequence resources, the project focuses on developing approaches to improve phenomic predictability and facilitate
#' the development and deployment of tools and resources that help address fundamental problems of sustainable agricultural productivity.
#' Specific projects within G2F involve collaboration from research fields such as genetics, genomics, plant physiology, agronomy, climatology and crop modeling,
#' computational sciences, statistics, and engineering. See https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-020-4922-8.
#'
#' Here we used the geographic coordinates from 93 environments (diverse locations across 4 years)
#'
#'
#'@format A matrix of environmental features (mean centered and scaled) for each environment (trial-year)

#'@docType data
#'
#'@usage data(W_matrix_G2F)
#'
#'@examples
#'\dontrun{
#' require(envirotypeR)
#' data(W_matrix_G2F)
#' dim(W_matrix_G2F)
#'}
#' @format data.frame file

"W_matrix_G2F"
