
#' Stratified estimator for Ms.GSI
#'
#' @param mdl_out Optional. Model output object name.
#' @param path Where to find output from each mixture as a folder.
#' @param mixvec Character vector of mixture sillies that are used to locate the folders where output .csv files lives, if `mdl_out` is not provided.
#' @param catchvec Numeric vector of harvest for each mixture/stratum, must be in the same order as `mixvec`.
#' @param cv Numeric vector of harvest estimate coefficients of variation for each stratum, must be the same order as `mixvec`.
#'
#' @return A tibble of proportions and harvest numbers by reporting group for combined mixtures/strata.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' stratified_estimator_msgsi(path = "test", mixvec = c("m1", "m2", "m3"), catchvec = c(4500, 5000, 3000), cv = c(0.5, 0.1 ,0.3))
#'
stratified_estimator_msgsi <- function(mdl_out = NULL, path = NULL, mixvec, catchvec, cv = NULL) {

  if (is.null(mdl_out) & is.null(path) | !dir.exists(path)) {
    stop("`Either provide a Ms.GSI output or a valid `path` to folders containing output .csv files for each mixture.")
  }

  if (length(mixvec) != length(catchvec)) {
    stop("The lengths of mixture names and catch numbers are not the same.")
  }

  if (is.null(cv)) cv <- rep(0, length(catchvec))

  lapply(1:length(mixvec), function(i) {
    readr::read_csv(file = file.path(path, mixvec[i], "trace_comb.csv"),
                    col_types = readr::cols(.default = "d")) %>%
      dplyr::mutate(mix = mixvec[i],
                    # harvest = catchvec[i]),
                    harvest = rlnorm(max(itr)*max(chain),
                                     meanlog = log(catchvec[i])-log(cv[i]^2 + 1)/2,
                                     sdlog = sqrt(log(cv[i]^2 + 1))) )%>%
      # dplyr::mutate_at(1:(ncol(.) - 3), ~ . * catchvec[i])
      dplyr::mutate_at(1:(ncol(.) - 3), ~ . * harvest)
  }) %>% dplyr::bind_rows() %>%
    tidyr::pivot_longer(-c(itr, chain, mix, harvest), names_to = "group") %>%
    dplyr::summarise(mean_harv = mean(value),
                     sd_harv = stats::sd(value),
                     median_harv = stats::median(value),
                     ci05_harv = stats::quantile(value, 0.05),
                     ci95_harv = stats::quantile(value, 0.95),
                     mean = mean(value / harvest),
                     sd = stats::sd(value / harvest),
                     median = stats::median(value / harvest),
                     ci05 = stats::quantile(value / harvest, 0.05),
                     ci95 = stats::quantile(value / harvest, 0.95),
                     `P=0` = mean(value < 0.5),
                     .by = c(group))

}

