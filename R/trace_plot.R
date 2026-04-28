
#' Plot MCMC trace
#'
#' Make trace plot for visualizing the mixing of MCMC chains. Group information (as in `groups`, `p2_groups`, or `comb_groups`) should be provided for the trace history that you want to plot or the trace plots will be shown as collections. See vignette for details.
#'
#' @param mdl_out Model output object name.
#' @param trace_obj Trace from the model output.
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
#' tr_plot(mdl_out = msgsi_out, trace_obj = "trace_comb", pop_info = msgsi_out$comb_groups)

tr_plot <- function (mdl_out, trace_obj, pop_info = NULL) {

  nburn <- as.numeric(mdl_out$specs["nburn"])
  keep_burn <- mdl_out$specs["keep_burn"] == "TRUE"
  thin <- as.numeric(mdl_out$specs["thin"])

  obj <- mdl_out[[trace_obj]] %>%
    dplyr::mutate(itr = (itr - nburn*isFALSE(keep_burn)) / thin)

  if (is.null(pop_info)) {
    name_order <- dplyr::select(obj, -c(itr, ch)) %>% colnames()
    trace <- tidyr::pivot_longer(obj, cols = -c(ch, itr),
                                 names_to = "repunit", values_to = "p")
  } else {
    name_order <- unique(pop_info$repunit)
    trace <- tidyr::pivot_longer(obj, cols = -c(ch, itr), names_to = "collection") %>%
      dplyr::left_join(pop_info, by = "collection") %>%
      dplyr::summarise(p = sum(value), .by = c(ch, itr, repunit))
  }

  trace %>%
    dplyr::mutate(repunit = factor(repunit, levels = name_order)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = itr, y = p, color = factor(ch))) +
    {if (keep_burn) {
      ggplot2::annotate("rect", fill = "red", alpha = 0.15,
                        xmin = 0, xmax = nburn/thin, ymin = -Inf, ymax = Inf)
    }} +
    ggplot2::facet_grid(repunit ~ ., scales = "free") +
    ggplot2::labs(color = "MC chain")

} # nburn = 0 if keep_burn = FALSE

utils::globalVariables(c("ch", "itr", "repunit", "p"))




