
#' Templin baseline data.
#'
#' An example of genotype data for preparing msgsi input data
#' in *rubias* format (but prep_msgsi_data() can also take GCL format)
#' for making road-scale baseline
#'
#' @format A tibble with 29363 rows and 60 variables:
#' @source ADF&G GCL database LOKI
"base_templin"

#' Yukon baseline data.
#'
#' An example of genotype data for preparing msgsi input data
#' in *rubias* format (but prep_msgsi_data() can also take GCL format)
#' for making regional, finer-scale baseline
#'
#' @format A tibble with 5435 rows and 358 variables:
#' @source ADF&G GCL database LOKI
"base_yukon"

#' Mixture data.
#'
#' An example of genotype data for preparing msgsi input data
#' in *rubias* format (but prep_msgsi_data() can also take GCL format)
#' for making mixed population data
#' This is a made up mixture sample, not real
#'
#' @format A tibble with 150 rows and 358 variables:
#' @source ADF&G GCL database LOKI
"mix"

#' Templin population infomation.
#'
#' A tibble that contains collection name, reporting unit, group number
#' for each of the 211 populations in the Templin baseline
#'
#' @format A tibble with 211 rows and 3 variables: \code{collection}, \code{repunit},
#'   \code{grpvec}
"templin_pops211"

#' Yukon population infomation.
#'
#' A tibble that contains collection name, reporting unit, group number
#' for each of the 50 populations in the Yukon River baseline
#'
#' @format A tibble with 50 rows and 3 variables: \code{collection}, \code{repunit},
#'   \code{grpvec}
"yukon_pops50"










