
#' Preparing multistage GSI input data
#'
#' @param mixture_data Individual fish with loci for both tier 1 and tier 2.
#'   Mixture data in GCL or *rubias* format.
#' @param baseline1_data Tier 1 baseline data in GCL or *rubias* format.
#' @param baseline2_data Tier 2 baseline data in GCL or *rubias* format.
#' @param pop1_info Population information for tier 1. A tibble with columns
#'   collection (collection names), repunit (reporting unit names),
#'   grpvec (group numbers), origin (wild/hatchery).
#' @param pop2_info Population information for tier 2. A tibble with columns
#'   collection (collection names), repunit (reporting unit names),
#'   grpvec (group numbers).
#' @param sub_group Group numbers for groups of interest. Group id numbers in tier
#'   1 that identify groups in tier 2.
#' @param file_path Where you want to save a copy of input data. Leave it empty if
#'   you don't want to save a copy.
#' @param loci1 Optional. Provide loci (for tier 1) as a fail-safe check.
#' @param loci2 Optional. Provide loci (for tier 2) as a fail-safe check.
#'
#' @return A list objects as the input data for msgsi_mdl()
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' msgsi_dat <-
#'   prep_msgsi_data(mixture_data = mix,
#'   baseline1_data = base_templin, baseline2_data = base_yukon,
#'   pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
prep_msgsi_data <-
  function(mixture_data, baseline1_data, baseline2_data,
           pop1_info, pop2_info, sub_group,
           file_path = NULL,
           loci1 = NULL, loci2 = NULL) {

    start_time <- Sys.time()

    # identify loci for each stage
    # make sure no colnames other than marker names have ".1" at the end
    loci_tier1 <-
      dplyr::tibble(locus = names(baseline1_data)) %>%
      dplyr::filter(grepl("\\.1$", locus)) %>%
      dplyr::mutate(locus = substr(locus, 1, nchar(locus) - 2)) %>%
      dplyr::pull(locus)

    loci_tier2 <-
      dplyr::tibble(locus = names(baseline2_data)) %>%
      dplyr::filter(grepl("\\.1$", locus)) %>%
      dplyr::mutate(locus = substr(locus, 1, nchar(locus) - 2)) %>%
      dplyr::pull(locus)

    # input error check

    loci_all <-
      dplyr::tibble(locus = names(mixture_data)) %>%
      dplyr::filter(grepl("\\.1$", locus)) %>%
      dplyr::mutate(locus = substr(locus, 1, nchar(locus) - 2)) %>%
      dplyr::pull(locus)

    error_message <- check_loci_pops(loci1, loci_tier1, loci2, loci_tier2,
                                     pop1_info$repunit, pop2_info$repunit,
                                     loci_all)

    if (grepl("Group names", error_message[1])) {
      warning(error_message)
      message("Compiling input data, may take a minute or two...")
    } else if ("all good" %in% error_message) {
      message("Compiling input data, may take a minute or two...")
    } else {
      stop(error_message)
    }

    # change column name if the data are gcl objects
    # to match rubias input data name convention
    if ("SILLY_CODE" %in% names(baseline1_data)) baseline1_data <- dplyr::rename(baseline1_data, collection = SILLY_CODE)

    if ("SILLY_CODE" %in% names(baseline2_data)) baseline2_data <- dplyr::rename(baseline2_data, collection = SILLY_CODE)

    if ("SillySource" %in% names(mixture_data)) mixture_data <- dplyr::rename(mixture_data, indiv = SillySource)

    # tally allele for each baseline and mixture sample
    base1 <- allefreq(baseline1_data, baseline1_data, loci_tier1, collect_by = collection) %>%
      dplyr::right_join(pop1_info, by = c("collection" = "collection"), keep = FALSE) %>%
      dplyr::relocate(!dplyr::ends_with(as.character(0:9)), .after = collection) %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with(as.character(0:9)), ~tidyr::replace_na(., 0)))

    base2 <- allefreq(baseline2_data, baseline2_data, loci_tier2, collect_by = collection) %>%
      dplyr::right_join(pop2_info, by = c("collection" = "collection"), keep = FALSE) %>%
      dplyr::relocate(!dplyr::ends_with(as.character(0:9)), .after = collection)

    mix1 <- allefreq(mixture_data, baseline1_data, loci_tier1)

    mix2 <- allefreq(mixture_data, baseline2_data, loci_tier2)

    # numbers of allele types
    nalleles_tier1 <- lapply(loci_tier1, function(loc) {
      dplyr::tibble(locus = loc,
                    call = baseline1_data %>%
                      dplyr::select(dplyr::all_of(loc), paste0(loc, ".1")) %>%
                      unlist() %>% unique() %>% .[!is.na(.)],
                    altyp = seq.int(dplyr::n_distinct(call)) %>% factor())
    }) %>% dplyr::bind_rows() %>%
      dplyr::group_by(locus) %>%
      dplyr::summarise(n_allele = max(as.numeric(altyp)), .groups = "drop")

    n_alleles_t1 <- nalleles_tier1 %>%
      dplyr::pull(n_allele) %>%
      stats::setNames(nalleles_tier1$locus)

    nalleles_tier2 <- lapply(loci_tier2, function(loc) {
      dplyr::tibble(locus = loc,
                    call = baseline2_data %>%
                      dplyr::select(dplyr::all_of(loc), paste0(loc, ".1")) %>%
                      unlist() %>% unique() %>% .[!is.na(.)],
                    altyp = seq.int(dplyr::n_distinct(call)) %>% factor())
    }) %>% dplyr::bind_rows() %>%
      dplyr::group_by(locus) %>%
      dplyr::summarise(n_allele = max(as.numeric(altyp)), .groups = "drop")

    n_alleles_t2 <- nalleles_tier2 %>%
      dplyr::pull(n_allele) %>%
      stats::setNames(nalleles_tier2$locus)

    # group names for each stage
    grp1_nms <- base1 %>%
      dplyr::arrange(grpvec) %>%
      dplyr::pull(repunit) %>%
      unique()

    grp2_nms <- base2 %>%
      dplyr::arrange(grpvec) %>%
      dplyr::pull(repunit) %>%
      unique()

    # wild or hatchery
    if ("origin" %in% names(base1)) {
      wildpops <- base1 %>%
        dplyr::filter(origin == "wild") %>%
        dplyr::pull(collection)
      hatcheries <- base1 %>%
        dplyr::filter(origin == "hatchery") %>%
        dplyr::pull(collection)
    } else {
      wildpops <- base1 %>% dplyr::pull(collection)
      hatcheries <- NULL
    }

    # iden if specified in mixture data
    if (any(grepl("known_", names(mixture_data)))) {
      iden <- mixture_data %>%
        dplyr::select(tidyr::contains("known_")) %>%
        dplyr::pull()
      if (!all(stats::na.omit(iden) %in% c(wildpops, hatcheries))) {
        stop(c("Unidentified populations found in 'known_collection': ",
               paste0(unique(stats::na.omit(iden)[which(!stats::na.omit(iden) %in% c(wildpops, hatcheries))]), ", ")))
      }
      iden <- factor(iden, levels = c(wildpops, hatcheries)) %>%
        as.numeric()
    } else {
      iden <- NULL
    }

    # output
    msgsi_dat = list(
      x = mix1,
      x2 = mix2,
      y = base1,
      y2 = base2,
      iden = iden,
      nalleles = n_alleles_t1,
      nalleles2 = n_alleles_t2,
      groups = dplyr::select(base1, collection, repunit, grpvec), # base1$grpvec,
      p2_groups = dplyr::select(base2, collection, repunit, grpvec), # base2$grpvec,
      comb_groups = dplyr::select(base1, collection, repunit, grpvec) %>%
        dplyr::filter(!grpvec %in% sub_group) %>%
        dplyr::bind_rows({
          dplyr::select(base2, collection, repunit, grpvec)
        }),
      sub_group = sub_group,
      group_names_t1 = grp1_nms,
      group_names_t2 = grp2_nms,
      wildpops = wildpops,
      hatcheries = hatcheries
    )

    if (!is.null(file_path)) save(msgsi_dat, file = file_path)

    print(Sys.time() - start_time)

    return(msgsi_dat)

  }


#' Allele frequency
#'
#' Calculate allele frequency for each locus
#'   for individual fish or a collection/population.
#'
#' @param gble_in Genotype table.
#' @param gle_ref Reference genetypr table.
#' @param loci loci names.
#' @param collect_by At what level to group by.
#'
#' @noRd
allefreq <- function(gble_in, gble_ref, loci, collect_by = indiv) {

  alleles = lapply(loci, function(loc) {
    dplyr::tibble(locus = loc,
                  call = gble_ref %>%
                    dplyr::select(dplyr::all_of(loc), paste0(loc, ".1")) %>%
                    unlist() %>%
                    unique() %>%
                    .[!is.na(.)],
                  altyp = seq.int(dplyr::n_distinct(call)) %>% factor)
    }) %>% dplyr::bind_rows()

  n_alleles = alleles %>%
    dplyr::group_by(locus) %>%
    dplyr::summarise(n_allele = max(as.numeric(altyp)), .groups = "drop")

  scores_cols = sapply(loci, function(locus) {
    c(locus, paste0(locus, ".1"))
    }) %>%
    as.vector()

  gble_in %>%
    dplyr::select(c({{ collect_by }}, dplyr::all_of(scores_cols))) %>%
    tidyr::pivot_longer(
      cols = -{{ collect_by }},
      names_to = "locus",
      values_to = "allele"
    ) %>%
    dplyr::mutate(
      locus = stringr::str_replace(string = locus, pattern = "\\.1$", replacement = "")
    ) %>%
    dplyr::left_join(alleles,
                     by = c("locus" = "locus", "allele" = "call"),
                     keep = FALSE) %>%
    dplyr::group_by({{ collect_by }}, locus) %>%
    dplyr::count(altyp, .drop = FALSE) %>%
    dplyr::filter(!is.na(altyp)) %>%
    dplyr::left_join(n_alleles,
                     by = c("locus" = "locus"),
                     keep = FALSE) %>%
    dplyr::filter(as.numeric(altyp) <= n_allele) %>%
    dplyr::select(-n_allele) %>%
    tidyr::unite("altyp", c(locus, altyp)) %>%
    tidyr::pivot_wider(names_from = altyp, values_from = n) %>%
    dplyr::ungroup()

}


#' Error check
#'
#' Check loci and population information in input data.
#'
#' @param loci1_pr User provided loci 1 info.
#' @param loci_t1 Loci info from stage 1 baseline.
#' @param loci2_pr User provided loci 2 info.
#' @param loci_t2 Loci info from stage 2 baseline.
#' @param repunit1 Reporting unit for stage 1.
#' @param repunit2 Reporting unit for stage 2.
#' @param loc_all All loci in mixture data.
#'
#' @noRd
check_loci_pops <- function(loci1_pr, loci_t1, loci2_pr, loci_t2,
                            repunit1, repunit2, loc_all) {

  # check loci if provided
  if (!is.null(loci1_pr)) {
    if (!setequal(loci_t1, loci1_pr)) {
      return(c("Unidentified loci in baseline 1 or provided list: ",
               paste0(c(setdiff(loci_t1, loci1_pr), setdiff(loci1_pr, loci_t1)), ", ")))
    }
  }

  if (!is.null(loci2_pr)) {
    if (!setequal(loci_t2, loci2_pr)) {
      return(c("Unidentified loci in baseline 2 or provided list: ",
               paste0(c(setdiff(loci_t2, loci2_pr), setdiff(loci2_pr, loci_t2)), ", ")))
    }
  }

  if (!is.null(loci1_pr) & !is.null(loci2_pr)) {
    if (!setequal(c(loci_t1, loci_t2), loc_all)) {
      return(c("Unidentified loci in mixture sample or provided lists: ",
               paste0(c(setdiff(loc_all, c(loci_t1, loci_t2)), setdiff(c(loci_t1, loci_t2), loc_all)), ", ")))
    }
  }

  # make sure group names are consistent between both tiers
  if (any(!unique(repunit2) %in% repunit1)) {
    return(c("Group names are not consistent between the two baselines.\n",
             "Names in baseline 2 but not in baseline 1: ",
             unique(repunit2)[!unique(repunit2) %in% repunit1]))
  }

  return("all good")

}


utils::globalVariables(c(".", "SILLY_CODE", "SillySource", "altyp", "collection",
                         "grpvec", "indiv", "locus", "n", "loci_all",
                         "n_allele", "origin", "repunit"))









