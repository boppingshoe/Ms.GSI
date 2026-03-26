
#' Sample total harvest numbers from a lognormal distribution
#'
#' @param x A vector consists of harvest mean and CV.
#' @param n Number of sample draws.
#' @param seed Optional random seed.
#'
#' @return A vector of harvest numbers drawn from a lognormal distribution with specified mean and CV.
#'
#' @examples
#' tot_harv <- harv_func(c(500, 0.05))
#'
#' @export
harv_func <- function(x, n = 5000, seed = NULL) {
  lnvar <- log(x[2]^2 + 1)
  lnmean <- log(x[1]) - lnvar / 2
  if (!is.null(seed)) set.seed(seed)
  stats::rlnorm(n, lnmean, sqrt(lnvar))
}


#' Summarize trace output for stock-specific harvest
#'
#' @param mdl_out Output of GSI model run
#' @param mdl_dat Input data for GSI model
#'
#' @return A tibble of harvest estimates for each reporting group as columns and MCMC iterations as rows.
#'
#' @examples
#' # prep input data
#' msgsi_dat <- prep_msgsi_data(mixture_data = mix, baseline1_data = base_templin, baseline2_data = base_yukon, pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5, harvest_mean = 500, harvest_cv = 0.05)
#'
#' # run multistage model
#' msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#'
#' # summarize individual assignments
#' harv_summ <- msgsi_harv_summ(msgsi_out, msgsi_dat)
#'
#' @export
msgsi_harv_summ <- function(mdl_out, mdl_dat) {
  nburn <- as.numeric(mdl_out$specs["nburn"])

  t1_harv <-
    mdl_out$sstc_trace_t1 %>%
    tidyr::pivot_longer(-c(itr, ch), names_to = "collection", values_to = "sstc_coll") %>%
    dplyr::left_join(mdl_dat$groups_t1, by = "collection") %>%
    dplyr::filter(itr > nburn, !grpvec %in% mdl_dat$sub_group) %>%
    dplyr::summarise(sstc = sum(sstc_coll), .by = c(itr, ch, repunit))

  t2_harv <-
    mdl_out$sstc_trace_t2 %>%
    dplyr::filter(itr > nburn) %>%
    tidyr::pivot_longer(-c(itr, ch), names_to = "collection", values_to = "sstc_coll") %>%
    dplyr::left_join(mdl_dat$groups_t2, by = "collection") %>%
    dplyr::summarise(sstc = sum(sstc_coll), .by = c(itr, ch, repunit))

  dplyr::bind_rows(t1_harv, t2_harv) %>%
    dplyr::summarise(mean_harv = mean(sstc),
                     median_harv = stats::median(sstc),
                     sd_harv = stats::sd(sstc),
                     ci05_harv = stats::quantile(sstc, 0.05),
                     ci95_harv = stats::quantile(sstc, 0.95),
                     .by = repunit)

}


utils::globalVariables(c("sstc_coll", "sstc"))



