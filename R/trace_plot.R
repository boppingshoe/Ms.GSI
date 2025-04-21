
#' Plot MCMC trace
#'
#' @param obj Trace from the model output.
#' @param nburn Number of burn-in you set up when you ran the model.
#'   Default is 0 if you didn't save the burn-ins (keep_burn = FALSE).
#' @param thin Number of thinning you set up when you ran the model.
#'   Default is 1 (no thinning).
#' @param pop_info Population information. A tibble with columns
#'   collection (collection names), repunit (reporting unit names),
#'    and grpvec (group numbers).
#'
#' @return Trace plot in ggplot

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
#' tr_plot(obj = msgsi_out$trace_comb, pop_info = msgsi_out$comb_groups)

tr_plot <- function (obj, nburn = 0, thin = 1, pop_info = NULL) {

  if (is.null(pop_info)) {
    name_order <- dplyr::select(obj, -c(itr, chain)) %>% colnames()
    trace <- tidyr::pivot_longer({{ obj }}, cols = -c(chain, itr),
                                 names_to = "repunit", values_to = "p")
  } else {
    name_order <- unique(pop_info$repunit)
    trace <- tidyr::pivot_longer({{ obj }}, cols = -c(chain, itr), names_to = "collection") %>%
      dplyr::left_join(pop_info, by = "collection") %>%
      dplyr::summarise(p = sum(value), .by = c(chain, itr, repunit))
  }

  trace %>%
    dplyr::mutate(repunit = factor(repunit, levels = name_order)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = itr, y = p, color = factor(chain))) +
    {if (nburn > 0) {
      ggplot2::annotate("rect", fill = "red", alpha = 0.15,
                        xmin = 0, xmax = nburn/thin, ymin = -Inf, ymax = Inf)
    }} +
    ggplot2::facet_grid(repunit ~ ., scales = "free") +
    ggplot2::labs(color = "MC chain")

} # nburn = 0 if keep_burn = FALSE

utils::globalVariables(c("chain", "itr", "repunit", "p"))




