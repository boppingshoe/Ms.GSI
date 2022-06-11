

prep_msgsi_data <-
  function(mixture_data, # indiv fish with loci for both tier 1 and tier 2
           baseline1_data, # tier 1
           baseline2_data, # tier 2
           pop1_info, # tibble with columns collection, repunit, grpvec, origin (wild/hatchery)
           pop2_info,
           sub_group, # group id numbers in tier 1 that identify groups in tier 2
           file_path = NULL, # declare if want to save a copy
           loci1 = NULL, # provide loci (for tier 1) as a fail-safe check
           loci2 = NULL) {

    # if(!require("pacman")) install.packages("pacman")
    # pacman::p_load(tidyverse)

    # get allele frequency for each locus
    # for individual fish or a collection/population
    allefreq <- function(gble_in, gble_ref, loci, collect_by = indiv) {

      alleles = lapply(loci, function(loc) {
        dplyr::tibble(locus = loc,
                      call = gble_ref %>%
                        select(all_of(loc), paste0(loc, ".1")) %>%
                        pull %>% unique %>% .[!is.na(.)],
                      altyp = seq.int(n_distinct(call)) %>% factor)
      }) %>% dplyr::bind_rows

      n_alleles = alleles %>%
        dplyr::group_by(locus) %>%
        dplyr::summarise(n_allele = as.numeric(altyp) %>% max, .groups = "drop")

      scores_cols = sapply(loci, function(locus) {c(locus, paste0(locus, ".1"))}) %>%
        as.vector()

      frq_tib <- gble_in %>%
        dplyr::select(c({{collect_by}}, all_of(scores_cols))) %>%
        tidyr::pivot_longer(
          cols = -{{collect_by}},
          names_to = "locus",
          values_to = "allele"
        ) %>%
        dplyr::mutate(
          locus = stringr::str_replace(string = locus, pattern = "\\.1$", replacement = "")
        ) %>%
        dplyr::left_join(alleles,
                         by = c("locus" = "locus", "allele" = "call"),
                         keep = FALSE) %>%
        dplyr::group_by({{collect_by}}, locus) %>%
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

      return(frq_tib)

    }

    start_time = Sys.time()
    message("Working on it, may take a minute or two...")

    # identify loci for each stage
    loci_tier1 =
      dplyr::tibble(locus = names(baseline1_data)) %>%
      dplyr::filter(grepl("\\.1$", locus)) %>%
      dplyr::mutate(locus = substr(locus, 1, nchar(locus)-2)) %>%
      dplyr::pull(locus)

    loci_tier2 =
      dplyr::tibble(locus = names(baseline2_data)) %>%
      dplyr::filter(grepl("\\.1$", locus)) %>%
      dplyr::mutate(locus = substr(locus, 1, nchar(locus)-2)) %>%
      dplyr::pull(locus)

    # check loci if provided
    if(!is.null(loci1)) {
      if(!all(loci_tier1 %in% loci1)) stop(c("Unidentified loci in baseline 1:", paste0(setdiff(loci_tier1, loci1), ", ")))
    }

    if(!is.null(loci2)) {
      if(!all(loci_tier2 %in% loci2)) stop(c("Unidentified loci in baseline 2:", paste0(dplyr::setdiff(loci_tier2, loci2), ", ")))
    }

    if(!is.null(loci1) & !is.null(loci2)) {
      loci_all =
        dplyr::tibble(locus = names(mixture_data)) %>%
        dplyr::filter(grepl("\\.1$", locus)) %>%
        dplyr::mutate(locus = substr(locus, 1, nchar(locus)-2)) %>%
        dplyr::pull(locus)
      if(!all(c(loci_tier1, loci_tier2) %in% loci_all)) stop(c("Unidentified loci in mixture sample:", paste0(dplyr::setdiff(loci_all, c(loci_tier1, loci_tier2)), ", ")))
    }

    # make sure group names are consistent between both tiers
    if(any(!unique(pop2_info$repunit) %in% pop1_info$repunit)) stop(c("Group names are not consistent between the two baselines. Names in baseline 2 but not in baseline 1: ", unique(pop2_info$repunit)[!unique(pop2_info$repunit) %in% pop1_info$repunit]))

    # change column name if the data are gcl objects
    # to match rubias input data name convention
    if("SILLY_CODE" %in% names(baseline1_data)) baseline1_data = rename(baseline1_data, collection = SILLY_CODE)

    if("SILLY_CODE" %in% names(baseline2_data)) baseline2_data = dplyr::rename(baseline2_data, collection = SILLY_CODE)

    if("SillySource" %in% names(mixture_data)) mixture_data = dplyr::rename(mixture_data, indiv = SillySource)

    # tally allele for each baseline and mixture sample
    base1 = allefreq(baseline1_data, baseline1_data, loci_tier1, collect_by = collection) %>%
      dplyr::right_join(pop1_info, by = c("collection" = "collection"), keep = FALSE) %>%
      dplyr::relocate(!ends_with(as.character(0:9)), .after = collection) %>%
      dplyr::mutate(across(ends_with(as.character(0:9)), ~replace_na(., 0)))

    base2 = allefreq(baseline2_data, baseline2_data, loci_tier2, collect_by = collection) %>%
      dplyr::right_join(pop2_info, by = c("collection" = "collection"), keep = FALSE) %>%
      dplyr::relocate(!ends_with(as.character(0:9)), .after = collection)

    mix1 = allefreq(mixture_data, baseline1_data, loci_tier1)

    mix2 = allefreq(mixture_data, baseline2_data, loci_tier2)

    # numbers of allele types
    nalleles_tier1 = lapply(loci_tier1, function(loc) {
      dplyr::tibble(locus = loc,
                    call = baseline1_data %>%
                      dplyr::select(dplyr::all_of(loc), paste0(loc, ".1")) %>%
                      dplyr::pull %>% unique %>% .[!is.na(.)],
                    altyp = seq.int(n_distinct(call)) %>% factor)
    }) %>% dplyr::bind_rows %>%
      dplyr::group_by(locus) %>%
      dplyr::summarise(n_allele = as.numeric(altyp) %>% max, .groups = "drop")

    n_alleles_t1 = nalleles_tier1 %>% dplyr::pull(n_allele) %>% setNames(nalleles_tier1$locus)

    nalleles_tier2 = lapply(loci_tier2, function(loc) {
      dplyr::tibble(locus = loc,
                    call = baseline2_data %>%
                      dplyr::select(dplyr::all_of(loc), paste0(loc, ".1")) %>%
                      dplyr::pull %>% unique %>% .[!is.na(.)],
                    altyp = seq.int(n_distinct(call)) %>% factor)
    }) %>% dplyr::bind_rows %>%
      dplyr::group_by(locus) %>%
      dplyr::summarise(n_allele = as.numeric(altyp) %>% max, .groups = "drop")

    n_alleles_t2 = nalleles_tier2 %>% dplyr::pull(n_allele) %>% setNames(nalleles_tier2$locus)

    # group names for each stage
    grp1_nms = base1 %>% dplyr::arrange(grpvec) %>% dplyr::pull(repunit) %>% unique

    grp2_nms = base2 %>% dplyr::arrange(grpvec) %>% dplyr::pull(repunit) %>% unique

    # iden if specified in mixture data
    if(any(grepl("known_", names(mixture_data)))) {
      iden = mixture_data %>% dplyr::select(contains("known_")) %>% dplyr::pull()
    } else {iden = NULL}

    # wild or hatchery
    if("origin" %in% names(base1)) {
      wildpops = base1 %>% dplyr::filter(origin == "wild") %>% dplyr::pull(collection)
      hatcheries = base1 %>% dplyr::filter(origin == "hatchery") %>% dplyr::pull(collection)
    } else {
      wildpops = base1 %>% dplyr::pull(collection)
      hatcheries = NULL
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
      groups = base1 %>% pull(grpvec),
      p2_groups = base2 %>% pull(grpvec),
      sub_group = sub_group,
      group_names_t1 = grp1_nms,
      group_names_t2 = grp2_nms,
      wildpops = wildpops,
      hatcheries = hatcheries
    )

    if(!is.null(file_path)) save(msgsi_dat, file = file_path)

    print(Sys.time() - start_time)

    return(msgsi_dat)

  }

