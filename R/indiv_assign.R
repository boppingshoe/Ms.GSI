
#' Individual assignment summary
#'
#' @param mdl_out Model output object name.
#' @param mdl_dat Input data object name.
#' @param show_t1_grps Set it to be `FALSE` if you want the proportions of broad-scale groups to be combined. Default = `TRUE`.
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
indiv_assign <- function(mdl_out, mdl_dat, show_t1_grps = TRUE) {
  nburn <- as.numeric(mdl_out$specs["nburn"])
  keep_burn <- mdl_out$specs["keep_burn"] == "TRUE"
  nreps <- as.numeric(mdl_out$specs["nreps"])
  thin <- as.numeric(mdl_out$specs["thin"])

  keep_list <- ((nburn*keep_burn + 1):(nreps - nburn * isFALSE(keep_burn)))[!((nburn*keep_burn + 1):(nreps - nburn * isFALSE(keep_burn))) %% thin] / thin

  p <- apply(mdl_out$idens_t1[keep_list,], 2,
             function (idens) {
               factor(idens, levels = seq(length(mdl_dat$groups_t1$grpvec))) %>%
                 table(.) %>%
                 prop.table(.)
             }) %>% rowsum(., mdl_dat$groups_t1$grpvec) %>% t()

  pi <- apply(mdl_out$idens_t2[keep_list,], 2,
              function (idens) {
                factor(idens, levels = seq(length(mdl_dat$groups_t2$grpvec))) %>%
                  table(.) %>%
                  prop.table(.)
              }) %>% rowsum(., mdl_dat$groups_t2$grpvec) %>% t()

  # pho <- rowSums(p[, mdl_dat$sub_group])
  pho <- apply(mdl_out$idens_t1[keep_list,], 2,
               function (idens) mean(idens %in% which(mdl_dat$groups_t1$grpvec %in% mdl_dat$sub_group)))

  if (show_t1_grps == TRUE) {
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

