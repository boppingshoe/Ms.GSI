
# devtools::load_all()

test_that("data error check", {

  loci_t1 <- dplyr::tibble(locus = names(base_templin)) %>%
    dplyr::filter(grepl("\\.1$", locus)) %>%
    dplyr::mutate(locus = substr(locus, 1, nchar(locus) - 2)) %>%
    dplyr::pull(locus)

  loci_t2 <- dplyr::tibble(locus = names(base_yukon)) %>%
    dplyr::filter(grepl("\\.1$", locus)) %>%
    dplyr::mutate(locus = substr(locus, 1, nchar(locus) - 2)) %>%
    dplyr::pull(locus)

  expect_error(prep_msgsi_data(mix, base_templin, base_yukon,
                               templin_pops211, yukon_pops50, 3:5,
                               loci1 = loci_t1[-1]),
               "Unidentified loci in baseline 1")

  expect_error(prep_msgsi_data(mix, base_templin, base_yukon,
                               templin_pops211, yukon_pops50, 3:5,
                               loci2 = c(loci_t2, "plus_one")),
               "Unidentified loci in baseline 2")

  expect_error(prep_msgsi_data(mix, base_templin,
                               dplyr::mutate(base_yukon, plus_one = 0, plus_one.1 = 0),
                               templin_pops211, yukon_pops50, 3:5,
                               loci1 = loci_t1, loci2 = c(loci_t2, "plus_one")),
               "Unidentified loci in mixture")

})
