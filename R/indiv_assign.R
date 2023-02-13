
#' Individual assignment summary
#'
#' @param mdl_out Model output object name.
#' @param mdl_dat Input data object name.
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
#' msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#'
#' # trace plot
#' ind_iden <- indiv_assign(msgsi_out, msgsi_dat)
#'
indiv_assign <- function(mdl_out, mdl_dat) {
  regional <- which(mdl_dat$groups %in% mdl_dat$sub_group)

  pho <- apply(mdl_out$idens_t1, 2,
               function (idens) mean(idens %in% regional))

  pi <- apply(mdl_out$idens_t2, 2,
              function (idens) {
                factor(idens, levels = seq(length(mdl_dat$p2_groups))) %>%
                  table(.) %>%
                  tapply(., mdl_dat$p2_groups, sum) %>%
                  prop.table(.)
              })

  tidyr::tibble(ID = mdl_dat$x$indiv) %>%
    dplyr::bind_cols({
      cbind(1 - pho, pho * t(pi)) %>%
        as.data.frame() %>%
        stats::setNames(c("Not regional", mdl_dat$group_names_t2))
    })
}
