
#' Stratified estimator for Ms.GSI
#'
#' @param mdl_out Optional. Ms.GSI output object name for combining group proportions and harvest of a single mixture.
#' @param path Where to find output from each mixture as a folder.
#' @param mixvec Character vector of mixture sillies that are used to locate the folders where output .csv files lives, if `mdl_out` is not provided.
#' @param catchvec Numeric vector of harvest for each mixture/stratum, must be in the same order as `mixvec`.
#' @param cv Numeric vector of harvest estimate coefficients of variation for each stratum, must be the same order as `mixvec`.
#' @param new_pop_info Population information for the new grouping. A tibble with columns
#'   `repunit` and `new_repunit`. `repunit` is the names of the original reporting groups.
#'
#' @return A tibble of proportions and harvest numbers by reporting group for combined mixtures/strata.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' new_groups <- mdl_out$summ_comb %>% dplyr::select(group) %>%
#'  dplyr::mutate(new_repunit = c(rep("broad", 9), rep("regional", 3))) %>%
#'  dplyr::rename(repunit = group)
#'
#' stratified_estimator_msgsi(path = "test", mixvec = c("m1", "m2", "m3"), catchvec = c(4500, 5000, 3000), cv = c(0.5, 0.1 ,0.3), new_pop_info = new_groups)
#' }
#'
stratified_estimator_msgsi <- function(mdl_out = NULL, path = NULL, mixvec, catchvec, cv = NULL, new_pop_info = NULL) {

  if (is.null(mdl_out) & is.null(path)) {
    stop("`Either provide a Ms.GSI output or a valid `path` to folders containing output .csv files for each mixture.")
  }

  if (length(mixvec) != length(catchvec)) {
    stop("The lengths of mixture names and catch numbers are not the same.")
  }

  if (!is.null(mdl_out) & length(mixvec) > 1) {
    mixvec <- mixvec[1]
    catchvec <- catchvec[1]
    if (!is.null(cv)) cv <- cv[1]
    message("There's only one mixture provided (as mdl_out), first mixture and harvest is used for calculations.")
  }

  if (is.null(cv)) cv <- rep(0, length(catchvec))

  if (is.null(mdl_out)) {
    grp_info <- readr::read_csv(file = file.path(path, mixvec[1], "comb_groups.csv"),
                                show_col_types = FALSE)
  } else grp_info <- mdl_out$comb_groups

  if (!is.null(new_pop_info)) {
    grp_info <- grp_info %>%
      dplyr::left_join(new_pop_info, by = "repunit") %>%
      dplyr::mutate(repunit = new_repunit) %>%
      dplyr::select(-new_repunit)
  }

  lapply(1:length(mixvec), function(i) {
    if (is.null(mdl_out)) {
      trace <- readr::read_csv(file = file.path(path, mixvec[i], "trace_comb.csv"),
                               col_types = readr::cols(.default = "d"))
    } else trace <- mdl_out$trace_comb

    trace %>%
      dplyr::mutate(mix = mixvec[i],
                    harvest = rlnorm(max(itr)*max(chain),
                                     meanlog = log(catchvec[i])-log(cv[i]^2 + 1)/2,
                                     sdlog = sqrt(log(cv[i]^2 + 1))) )%>%
      dplyr::mutate_at(1:(ncol(.) - 4), ~ . * harvest)
  }) %>% dplyr::bind_rows() %>%
    tidyr::pivot_longer(-c(itr, chain, mix, harvest), names_to = "collection") %>%
    dplyr::left_join(grp_info, by = "collection") %>%
    dplyr::summarise(p = sum(value), .by = c(repunit, itr, chain, mix, harvest)) %>%
    dplyr::summarise(mean_harv = mean(p),
                     sd_harv = stats::sd(p),
                     median_harv = stats::median(p),
                     ci05_harv = stats::quantile(p, 0.05),
                     ci95_harv = stats::quantile(p, 0.95),
                     mean = mean(p / harvest),
                     sd = stats::sd(p / harvest),
                     median = stats::median(p / harvest),
                     ci05 = stats::quantile(p / harvest, 0.05),
                     ci95 = stats::quantile(p / harvest, 0.95),
                     `P=0` = mean(p < 0.5),
                     .by = c(repunit))

}

