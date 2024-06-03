#==================================================================================================
# Title:    Feature Imputation and Computation of Environmental Eigenvectors
# Author:   G Costa-Neto
# Created at: 2023-11-27
# Updated at: 2023-12-05 (envirotypeR version 0.1.3)
# Current Version: 0.1.1 (envirotypeR)
#
# process_synthetic(), based on missMDA and FactoMineR
#==================================================================================================
#
#' @title Creates Orthogonal Vectors from Environmental Features.
#'
#' @description Feature Imputation and Computation of Environmental Eigenvectors from raw envirotyping data.
#'
#' @author Germano Costa Neto
#'
#'
#' @return A list of objects containing the following elements:
#' \itemize{
#'   \item{environmental.features}{ A data frame containing the original environmental variables and the synthetic environmental traits.}
#'   \item{synthetic.environmental.traits}{ A data frame containing the synthetic environmental traits (eigenvectors).}
#'   \item{variance.explained.by.eigen}{ A data frame with the eigenvalues, explained variance, and cumulative explained variance for each synthetic trait.}
#'   \item{variable.components}{ A data frame with the variable components (coordinates) for each synthetic trait.}
#'   \item{variable.contribution}{ A data frame with the variable contributions for each synthetic trait.}
#'   \item{variable.correlation}{ A data frame with the variable correlations for each synthetic trait.}
#'   \item{raw_PC_object}{(Optional) The FactoMineR PCA object, returned only if `return_raw` is TRUE.}
#'}
#'
#' @examples
#' \dontrun{
#' ## using a matrix of features pre-created (W_matrix)
#' W_processed <- process_synthetic(env.dataframe = envirotypeR::W_matrix_G2F)
#' lenght(W_processed)
#' }
#'
#'
#' @param env.dataframe (data.frame) with row names representing environments and column names as environmental variables. It accepts missing values.
#' @param missing.rate (numeric) the minimum missing rate accepted, default is 0.05.
#' @param n.synthetic (numeric) number of orthogonal vectors (Moran's eigenvectors) representing synthetic environmental traits to be created.
#' @param impute.missing (boolean) if TRUE, an imputation approach is conducted to impute missing data.
#' @param digits (numeric) number of digits in the output matrices.
#' @param sample.imp (numeric) Number of environments to be sampled and used for creating a training set for imputation when dealing with large datasets.
#' @param return_raw (boolean) if TRUE, returns a FactoMineR object to be used for clustering.
#'
#' @importFrom missMDA estim_ncpPCA
#' @importFrom FactoMineR PCA
#' @importFrom missMDA imputePCA
#' @importFrom reshape2 melt
#'

process_synthetic <- function(env.dataframe, missing.rate = 0.05, n.synthetic = 15, digits = 5, impute.missing = TRUE, sample.imp = 5, return_raw = FALSE) {

  message("--------------------------------------------------------------------------")
  message('process_synthetic() - Generating Synthetic Traits from envirotypes and PCA')
  message('Based on missMDA::imputePCA and FactoMineR::PCA')
  message("-------------------------------------------------------------------------")

  if (!requireNamespace("missMDA", quietly = TRUE)) {
    utils::install.packages("missMDA")
  }
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    utils::install.packages("FactoMineR")
  }

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    utils::install.packages("reshape2")
  }


  missing_rate <- function(x, n.synthetic = n.synthetic) {
    total_len <- length(x)
    total_na <- sum(is.na(x))
    return(total_na / total_len)
  }

  startTime <- Sys.time()
  n.feature <- ncol(env.dataframe)
  message(paste0('Start at..............................', format(startTime, "%a %b %d %X %Y"), '\n'))
  message(paste0('Number of Environmental Units ........', nrow(env.dataframe)))
  message(paste0('Number of Environmental Features......', n.feature))
  message(paste0('Assumed Missing Rate..................', 100 * missing.rate, '%'))

  .missing_rate <- reshape2::melt(apply(env.dataframe, 2, FUN = missing_rate))
  .quality_control_var <- rownames(.missing_rate)[which(.missing_rate$value <= missing.rate)]

  .missing_entries = sum(is.na(env.dataframe))
  message(paste0('Missing Entries.......................',  .missing_entries, ' from ', nrow(env.dataframe) * ncol(env.dataframe)))

  message(paste0('\nFiltered Features.....................',length(.quality_control_var)))

  env.dataframe <- env.dataframe[, .quality_control_var]

  if (.missing_entries == 0) {
    message(paste0('Imputing variables using missMDA.......[  ]'))
  }

  if (.missing_entries > 0) {
    message(paste0('Imputing variables using missMDA.......[ ', ifelse(isTRUE(impute.missing), 'Yes', 'No'), ' ]'))

    if (isTRUE(impute.missing)) {
      .isNA <- as.numeric(which(is.na(apply(env.dataframe, 2, var))))

      message(paste0('Start Imputing......................'))
      if (isTRUE(length(.isNA) <= 3)) {
        .possibleID <- as.numeric(which(!is.na(apply(env.dataframe, 2, var))))
        .isNA <- c(.isNA, sample(.possibleID, size = sample.imp, replace = FALSE))
      }

      .ncomp <- missMDA::estim_ncpPCA(env.dataframe[, .isNA], ncp.min = 2, ncp.max = 5, method.cv = "Kfold", nbsim = 100, pNA = 0.05)

      env.dataframe[, .isNA] <- missMDA::imputePCA(env.dataframe[, .isNA], ncp = .ncomp[[1]])$completeObs
    }
  }


  message(paste0('\nGenerating Synthetic traits and metrics :'))

  .raw_PCs <- FactoMineR::PCA(env.dataframe, scale.unit = TRUE, graph = FALSE, ncp = n.synthetic)

  message(paste0('\nEnvironmental Eigenvectors..........',n.synthetic))
  message(paste0('New Environmental Features...........',n.synthetic + ncol(env.dataframe), ' (original features + synthetic traits)'))


  .contribution <- data.matrix(round(.raw_PCs$var$contrib, digits))
  .coordinates <- data.matrix(round(.raw_PCs$var$coord, digits))
  .correlation <- data.matrix(round(.raw_PCs$var$cor, digits))
  .synthetic <- data.matrix(round(.raw_PCs$svd$U, digits))

  .summary_comp <- data.frame(.raw_PCs$eig[1:n.synthetic, ])
  colnames(.summary_comp) = c('eigenvalue', 'explained_variance', 'cumulative_explained_variance')
  rownames(.synthetic) = rownames(env.dataframe)

  rownames(.summary_comp) = colnames(.contribution) = colnames(.coordinates) = colnames(.correlation) = colnames(.synthetic) = paste0('envPC_', 1:n.synthetic)

  endTime <- Sys.time()

  message(paste0('Done!..............................', format(endTime, "%a %b %d %X %Y")))
  message(paste0('Total Time.........................',  round(difftime(endTime, startTime, units = 'secs'), 2), 's'))

  if (isTRUE(return_raw)) {
    return(list(
      environmental.features = cbind(env.dataframe, .synthetic),
      synthetic.environmental.traits = .synthetic,
      variance.explained.by.eigen = .summary_comp,
      variable.components = .coordinates,
      variable.contribution = .contribution,
      variable.correlation = .correlation,
      raw_PC_object = .raw_PCs
    ))
  }
  if (isFALSE(return_raw)) {
    return(list(
      environmental.features = cbind(env.dataframe, .synthetic),
      synthetic.environmental.traits = .synthetic,
      variance.explained.by.eigen = .summary_comp,
      variable.components = .coordinates,
      variable.contribution = .contribution,
      variable.correlation = .correlation
    ))
  }
}
