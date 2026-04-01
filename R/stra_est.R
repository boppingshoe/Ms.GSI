
#' Stratified estimator for Ms.GSI
#'
#' @param mdl_out Optional. Ms.GSI output object name for combining group proportions and harvest of a single mixture.
#' @param path Where to find output from each mixture as a folder.
#' @param mixvec Character vector of mixture sillies that are used to locate the folders where output .csv files lives, if `mdl_out` is not provided.
#' @param new_pop_info Population information for the new grouping. A tibble with columns `repunit` and `new_repunit`. `repunit` is the names of the original reporting groups. Can include a column for `collection` if reorganizing using collections.
#' @param new_pop_by Option to reorganize the reporting groups by "repunit" or "collection". Default is "repunit".
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
#' stratified_estimator_msgsi(path = "test", mixvec = c("m1", "m2", "m3"), new_pop_info = new_groups)
#' }
#'
stratified_estimator_msgsi <- function(mdl_out = NULL, path = NULL, mixvec, new_pop_info = NULL, new_pop_by = "repunit", naive = FALSE, catchvec = NULL, cv = NULL) {

  if (is.null(mdl_out) & is.null(path)) {
    stop("`Either provide a Ms.GSI output or a valid `path` to folders containing output .csv files for each mixture.")
  }

  # group information ----
  if (is.null(mdl_out)) {
    grp_info <- readr::read_csv(file = file.path(path, mixvec[1], "comb_groups.csv"),
                                show_col_types = FALSE)
  } else {
    grp_info <- mdl_out$comb_groups
  }

  if (!is.null(new_pop_info)) {
    if (!"collection" %in% names(new_pop_info) & new_pop_by == "collection") {
      stop("To reorganize by collection, please provide new_pop_info with collection information.")
    } else if ("collection" %in% names(new_pop_info) & new_pop_by == "repunit") {
      stop("To reorganize by repunit, please only provide new_pop_info with reporting unit information without collection.")
    }
    grp_info <- grp_info %>%
      dplyr::left_join(new_pop_info, by = new_pop_by) %>%
      dplyr::mutate(repunit = new_repunit) %>%
      dplyr::select(-new_repunit)
  }

  # calculations ----
  if (isFALSE(naive)) {
    lapply(1:length(mixvec), function(i) {

      if (is.null(mdl_out)) {

        sstc_trace_t1 <- readr::read_csv(file = file.path(path, mixvec[i], "sstc_trace_t1.csv"),
                                         col_types = readr::cols(.default = "d"))
        sstc_trace_t2 <- readr::read_csv(file = file.path(path, mixvec[i], "sstc_trace_t2.csv"),
                                         col_types = readr::cols(.default = "d"))
        nburn <- readr::read_csv(file = file.path(path, mixvec[i], "msgsi_specs.csv"),
                                 col_types = readr::cols(.default = "d")) %>%
          dplyr::filter(name == "nburn") %>%
          dplyr::pull(value)
      } else {
        sstc_trace_t1 <- mdl_out$sstc_trace_t1
        sstc_trace_t2 <- mdl_out$sstc_trace_t2
        nburn <- as.numeric(mdl_out$specs["nburn"])
      }

      coll_names_t1 <- names(sstc_trace_t1)[!names(sstc_trace_t1) %in% names(sstc_trace_t2)]

      sstc_trace_t1[, c(coll_names_t1, "itr", "ch")] %>%
        dplyr::left_join(sstc_trace_t2, by = dplyr::join_by(itr, ch)) %>%
        tidyr::pivot_longer(-c(itr, ch), names_to = "collection") %>%
        dplyr::filter(collection %in% grp_info$collection,
                      itr > nburn) %>%
        dplyr::left_join(grp_info, by = "collection") %>%
        dplyr::summarise(sstc = sum(value), .by = c(itr, ch, repunit)) %>%
        dplyr::mutate(mix = mixvec[i])

    }) %>% dplyr::bind_rows() %>%
      dplyr::mutate(p = sstc / sum(sstc), .by = c(itr, ch)) %>%
      dplyr::summarise(mean_sstc = mean(sstc),
                       sd_sstc = stats::sd(sstc),
                       median_sstc = stats::median(sstc),
                       ci05_sstc = stats::quantile(sstc, 0.05),
                       ci95_sstc = stats::quantile(sstc, 0.95),
                       mean = mean(p),
                       sd = stats::sd(p),
                       median = stats::median(p),
                       ci05 = stats::quantile(p, 0.05),
                       ci95 = stats::quantile(p, 0.95),
                       `P=0` = mean(sstc < 0.5),
                       .by = c(repunit))

  } else { # the old way

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

    lapply(1:length(mixvec), function(i) {
      if (is.null(mdl_out)) {
        nburn <- readr::read_csv(file = file.path(path, mixvec[i], "msgsi_specs.csv"),
                                 col_types = readr::cols(.default = "d")) %>%
          dplyr::filter(name == "nburn") %>%
          dplyr::pull(value)
        trace <- readr::read_csv(file = file.path(path, mixvec[i], "trace_comb.csv"),
                                 col_types = readr::cols(.default = "d")) %>%
          dplyr::filter(itr > nburn)
      } else {
        nburn <- as.numeric(mdl_out$specs["nburn"])
        trace <- mdl_out$trace_comb %>%
          dplyr::filter(itr > nburn)
      }

      trace %>%
        dplyr::mutate(mix = mixvec[i],
                      harvest = stats::rlnorm(nrow(trace),
                                              meanlog = log(catchvec[i])-log(cv[i]^2 + 1)/2,
                                              sdlog = sqrt(log(cv[i]^2 + 1))) )%>%
        dplyr::mutate_at(dplyr::vars(-c(itr, ch, mix, harvest)), ~ . * harvest)
    }) %>% dplyr::bind_rows() %>%
      tidyr::pivot_longer(-c(itr, ch, mix, harvest), names_to = "collection") %>%
      dplyr::left_join(grp_info, by = "collection") %>%
      dplyr::summarise(harv_p = sum(value), .by = c(repunit, itr, ch)) %>%
      dplyr::mutate(p = harv_p / sum(harv_p), .by = c(itr, ch)) %>%
      dplyr::summarise(mean_harv = mean(harv_p),
                       sd_harv = stats::sd(harv_p),
                       median_harv = stats::median(harv_p),
                       ci05_harv = stats::quantile(harv_p, 0.05),
                       ci95_harv = stats::quantile(harv_p, 0.95),
                       mean = mean(p),
                       sd = stats::sd(p),
                       median = stats::median(p),
                       ci05 = stats::quantile(p, 0.05),
                       ci95 = stats::quantile(p, 0.95),
                       `P=0` = mean(harv_p < 0.5),
                       .by = c(repunit))

  }

}


utils::globalVariables(c("new_repunit", "mix", "harvest", "sstc"))


