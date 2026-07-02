
#' Stratified estimator for Ms.GSI
#'
#' Combine the stock group estimates of multiple mixtures (i.e., strata) weighted by harvest numbers or fishing efforts. Summary can be done by extracting the stock-specific total catch/harvest output from the model runs or by multiplying harvest (provided as input) by stock proportions. Reporting groups can stay in the same format or be reorganized by combining old reporting groups or reorganizing collections. See vignette for details.
#'
#' @param mdl_out Optional. Ms.GSI output object name for combining group proportions and harvest of a single mixture.
#' @param path Where to find output from each mixture as a folder.
#' @param mixvec Character vector of mixture sillies that are used to locate the folders where output .csv files lives, if `mdl_out` is not provided.
#' @param new_pop_info Population information for the new grouping. A tibble with columns `repunit` and `new_repunit`. `repunit` is the names of the original reporting groups. Can include a column for `collection` if reorganizing using collections.
#' @param new_pop_by Option to reorganize the reporting groups by "repunit" or "collection". Default is "repunit".
#' @param naive TRUE if you want the summary done by the old way (stock-specific harvets = harvest * stock proportion), or you are using fishing effort instead of catch number.
#' @param catchvec If `naive = TRUE`, manually input harvest or fishing effort means with the same order as `mixvec`.
#' @param cv If `naive = TRUE`, manually input harvest or fishing effort cv's with the same order as `mixvec`.
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
  if (!is.null(path)) {
    if (!file.exists(file.path(path, mixvec[1], "sstc_trace_t1.csv"))) { # for pre-v0.1.0
      message("No SSTC file detected. Summaries will default to using naive approach (the old way).")
      naive <- TRUE
      sstc_all_mix_combo <- NULL
    }
  }

  if (!exists("sstc_all_mix_combo")) {
    sstc_all_mix_combo <- lapply(1:length(mixvec), function(i) {

      if (is.null(mdl_out)) {
        sstc_trace_t1 <- readr::read_csv(file = file.path(path, mixvec[i], "sstc_trace_t1.csv"),
                                         col_types = readr::cols(.default = "?"))
        sstc_trace_t2 <- readr::read_csv(file = file.path(path, mixvec[i], "sstc_trace_t2.csv"),
                                         col_types = readr::cols(.default = "?"))
        nburn <- readr::read_csv(file = file.path(path, mixvec[i], "msgsi_specs.csv"),
                                 col_types = readr::cols(.default = "c")) %>%
          dplyr::filter(name == "nburn") %>%
          dplyr::pull(value) %>% as.numeric()

      } else {

        sstc_trace_t1 <- mdl_out$sstc_trace_t1
        sstc_trace_t2 <- mdl_out$sstc_trace_t2
        nburn <- as.numeric(mdl_out$specs["nburn"])
      }

      sstc_trace_t1 %>%
        dplyr::filter(!collection %in% sstc_trace_t2$collection) %>%
        dplyr::bind_rows(sstc_trace_t2) %>%
        dplyr::filter(collection %in% grp_info$collection,
                      itr > nburn) %>%
        dplyr::rename(repunit_old = repunit) %>%
        dplyr::left_join(grp_info, by = "collection") %>%
        dplyr::mutate(mix = mixvec[i])

    }) %>% dplyr::bind_rows() %>%
      dplyr::summarise(sstc = sum(sstc),
                       ac = sum(ac), .by = c(itr, ch, repunit)) %>%
      dplyr::mutate(p = sstc / sum(sstc), .by = c(itr, ch))
  }

  if (isFALSE(naive)) {
    n_ch <- length(unique(sstc_all_mix_combo$ch))

    mc_sstc <- coda::as.mcmc.list(
      lapply(1:n_ch, function(chain) {
        dplyr::filter(sstc_all_mix_combo, ch == chain) %>%
          tidyr::pivot_wider(id_cols = -c(p, ac), names_from = repunit, values_from = sstc) %>%
          dplyr::select(-c(itr, ch)) %>%
          coda::mcmc()
      })
    ) # mcmc list

    sstc_all_mix_combo %>%
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
                       `Z=0` = mean(ac == 0),
                       .by = c(repunit)) %>%
      dplyr::left_join(
        data.frame(
          GR = { if (n_ch > 1) {
            coda::gelman.diag(mc_sstc,
                              transform = FALSE,
                              autoburnin = FALSE,
                              multivariate = FALSE)$psrf[,"Point est."]
          } else NA },
          n_eff = coda::effectiveSize(mc_sstc)
        ) %>%
          tibble::rownames_to_column(var = "repunit"), by = dplyr::join_by(repunit)
      )

  } else { # the old way (naive == TRUE)

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

    all_mix_combo <- lapply(1:length(mixvec), function(i) {
      if (is.null(mdl_out)) {
        mdl_specs <-
          readr::read_csv(file = file.path(path, mixvec[i], "msgsi_specs.csv"),
                          col_types = readr::cols(.default = "c"))
        nburn <- mdl_specs %>%
          dplyr::filter(name == "nburn") %>%
          dplyr::pull(value) %>% as.numeric()
        keep_burn <- mdl_specs %>%
          dplyr::filter(name == "keep_burn") %>%
          dplyr::pull(value) %>% as.logical()
        thin <- mdl_specs %>%
          dplyr::filter(name == "thin") %>%
          dplyr::pull(value) %>% as.numeric()

        trace <- readr::read_csv(file = file.path(path, mixvec[i], "trace_comb.csv"),
                                 col_types = readr::cols(.default = "d")) %>%
          { if (is.null(sstc_all_mix_combo)) {
            dplyr::mutate(., itr = itr * thin + isFALSE(keep_burn) * nburn)
          } else . } %>%
          dplyr::filter(itr > nburn)

      } else {

        nburn <- as.numeric(mdl_out$specs["nburn"])
        trace <- mdl_out$trace_comb %>%
          dplyr::filter(itr > nburn)
      }

      trace %>%
        dplyr::rename(dplyr::any_of(c(ch = "chain"))) %>% # for pre-v0.1.0
        dplyr::mutate(mix = mixvec[i],
                      harvest = stats::rlnorm(nrow(trace),
                                              meanlog = log(catchvec[i])-log(cv[i]^2 + 1)/2,
                                              sdlog = sqrt(log(cv[i]^2 + 1))) )%>%
        dplyr::mutate_at(dplyr::vars(-c(itr, ch, mix, harvest)), ~ . * harvest)
    }) %>% dplyr::bind_rows() %>%
      tidyr::pivot_longer(-c(itr, ch, mix, harvest), names_to = "collection") %>%
      dplyr::left_join(grp_info, by = "collection") %>%
      dplyr::summarise(harv_p = sum(value), .by = c(repunit, itr, ch)) %>%
      dplyr::mutate(p = harv_p / sum(harv_p), .by = c(itr, ch))

    n_ch <- length(unique(all_mix_combo$ch))

    mc_p <- coda::as.mcmc.list(
      lapply(1:n_ch, function(chain) {
        dplyr::filter(all_mix_combo, ch == chain) %>%
          tidyr::pivot_wider(id_cols = -harv_p, names_from = repunit, values_from = p) %>%
          dplyr::select(-c(itr, ch)) %>%
          coda::mcmc()
      })
    )

    all_mix_combo %>%
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
                       .by = c(repunit)) %>%
      { if (!is.null(sstc_all_mix_combo)) {
        dplyr::left_join(.,
          sstc_all_mix_combo %>%
            dplyr::summarise(`Z=0` = mean(ac == 0),
                             .by = c(repunit)),
          by = dplyr::join_by(repunit))
      } else . } %>%
      dplyr::left_join(
        data.frame(
          GR = { if (n_ch > 1) {
            coda::gelman.diag(mc_p,
                              transform = FALSE,
                              autoburnin = FALSE,
                              multivariate = FALSE)$psrf[,"Point est."]
          } else NA },
          n_eff = coda::effectiveSize(mc_p)
        ) %>%
          tibble::rownames_to_column(var = "repunit"),
        by = dplyr::join_by(repunit))

  }

}


utils::globalVariables(c("new_repunit", "mix", "harvest", "sstc", "harv_p", "ac", "pprc"))


