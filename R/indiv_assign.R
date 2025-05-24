
#' Individual assignment summary
#'
#' @param mdl_out Model output object name.
#' @param mdl_dat Input data object name.
#' @param show_t2_grps Set it to be `FALSE` if you want the proportions of broad-scale groups to be combined. Default = `TRUE`.
#'
#' @return Individual assignment summary
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # set up input data and run multistage model
#' msgsi_dat <-
#'   prep_msgsi_data(mixture_data = mix,
#'   baseline1_data = base_templin, baseline2_data = base_yukon,
#'   pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#'
#' # run model
#' msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#'
#' # individual assignment summary
#' ind_iden <- indiv_assign(msgsi_out, msgsi_dat)
#'
indiv_assign <- function(mdl_out, mdl_dat, show_t2_grps = TRUE) {
  p <- apply(mdl_out$idens_t1, 2,
             function (idens) {
               factor(idens, levels = seq(length(mdl_dat$groups$grpvec))) %>%
                 table(.) %>%
                 prop.table(.)
             }) %>% rowsum(., mdl_dat$groups$grpvec) %>% t()

  pi <- apply(mdl_out$idens_t2, 2,
              function (idens) {
                factor(idens, levels = seq(length(mdl_dat$p2_groups$grpvec))) %>%
                  table(.) %>%
                  prop.table(.)
              }) %>% rowsum(., mdl_dat$p2_groups$grpvec) %>% t()

  # pho <- rowSums(p[, mdl_dat$sub_group])
  pho <- apply(mdl_out$idens_t1, 2,
               function (idens) mean(idens %in% which(mdl_dat$groups$grpvec %in% mdl_dat$sub_group)))

  if (show_t2_grps == TRUE) {
    tidyr::tibble(ID = mdl_dat$x$indiv) %>%
      dplyr::bind_cols({
        cbind(p[, -mdl_dat$sub_group], pho * pi) %>%
          as.data.frame() %>%
          stats::setNames(c(mdl_dat$group_names_t1[-mdl_dat$sub_group], mdl_dat$group_names_t2))
      })
  } else {
    tidyr::tibble(ID = mdl_dat$x$indiv) %>%
      dplyr::bind_cols({
        cbind(1 - pho, pho * pi) %>%
          as.data.frame() %>%
          stats::setNames(c("Not regional", mdl_dat$group_names_t2))
      })
  }
}

