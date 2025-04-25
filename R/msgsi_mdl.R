
#' Run multistage GSI model
#'
#' @param dat_in Name of the input data.
#' @param nreps Total number of iterations (includes burn-ins).
#' @param nburn Number of warm-up runs.
#' @param thin Frequency to thin the output.
#' @param nchains Number of independent MCMC processes.
#' @param nadapt Number of adaptation run (default is 0). Only available when
#'   running model in fully Bayesian mode.
#' @param keep_burn To save the burn-ins or not (default is FALSE).
#' @param cond_gsi To run the model in conditional GSI mode (default is TRUE).
#' @param file_path File path to save the output. Leave it empty is you don't
#'   want to save the output.
#' @param seed Random seed for reproducibility. Default is NULL (no random seed).
#' @param iden_output Option to have trace history for individual assignments included in the final output. Default is TRUE.
#' @param p1_prior_weight An optional tibble to specify weight for each broad-scale reporting group. Columns are `repunit`, `grpvec`, and `weight`.
#' @param p2_prior_weight An optional tibble to specify weight for each regional reporting group. Columns are `repunit`, `grpvec`, and `weight`.
#'
#' @return A list contains reporting group proportion summary and trace for
#'   tier 1 (summ_t1, trace_t1), tier 2 (summ_t2, trace_t2) and two tiers
#'   combined (summ_comb, trace_comb), and record of individual assignment
#'   during first tier for each individual (idens).
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom doRNG %dorng%
#' @importFrom foreach %dopar%
#'
#' @examples
#' # setup input data
#' msgsi_dat <-
#'   prep_msgsi_data(mixture_data = mix,
#'   baseline1_data = base_templin, baseline2_data = base_yukon,
#'   pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#'
#' # run multistage model
#' msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#'
msgsi_mdl <- function(dat_in, nreps, nburn, thin, nchains, nadapt = 0, keep_burn = FALSE, cond_gsi = TRUE, file_path = NULL, seed = NULL, iden_output = TRUE, p1_prior_weight = NULL, p2_prior_weight = NULL) {

  # save test file (specs) ----
  if(!is.null(file_path)) {
    specs <- data.frame(name = c("nreps", "nburn", "thin", "nchains", "keep_burn"),
                        value = c(nreps, nburn, thin, nchains, keep_burn))
    message(paste0("Ms.GSI specifications saved in ", file_path, "/msgsi_specs.csv"))
    readr::write_csv(specs, file = paste0(file_path, "/msgsi_specs.csv"))
  }

  # Message categories ----
  categories <- c("Live, Werk, Pose", "Bring It Like Royalty", "Face", "Best Mother", "Best Dressed", "High Class In A Fur Coat", "Snow Ball", "Butch Queen Body", "Weather Girl", "Labels", "Mother-Daughter Realness", "Working Girl", "Linen Vs. Silk", "Perfect Tens", "Modele Effet", "Stone Cold Face", "Realness", "Intergalatic Best Dressed", "House Vs. House", "Femme Queen Vogue", "High Fashion In Feathers", "Femme Queen Runway", "Lofting", "Higher Than Heaven", "Once Upon A Time")

  # data input ----
  x <- dat_in$x %>%
    dplyr::select(dplyr::ends_with(as.character(0:9))) %>%
    dplyr::select(order(colnames(.))) %>%
    as.matrix() # mixture 1
  x2 <- dat_in$x2 %>%
    dplyr::select(dplyr::ends_with(as.character(0:9))) %>%
    dplyr::select(order(colnames(.))) %>%
    as.matrix() # mixture 2
  y <- dat_in$y %>%
    dplyr::select(dplyr::ends_with(as.character(0:9))) %>%
    dplyr::select(order(colnames(.))) %>%
    as.matrix() # base 1
  y2 <- dat_in$y2 %>%
    dplyr::select(dplyr::ends_with(as.character(0:9))) %>%
    dplyr::select(order(colnames(.))) %>%
    as.matrix() # base 2

  if (is.null(dat_in$iden)) iden <- rep(NA, nrow(x)) else iden <- dat_in$iden # iden info
  nalleles <- dat_in$nalleles # number of allele types
  nalleles2 <- dat_in$nalleles2 # number of allele types
  grps <- dat_in$y$grpvec # vector id for the 1st tier reporting groups (aka groupvec)
  p2_grps <- dat_in$y2$grpvec # vector id for the 2nd tier reporting groups
  sub_grp <- dat_in$sub_group
  grp_names_t1 <- dat_in$group_names_t1
  grp_names_t2 <- dat_in$group_names_t2
  grp_names <- c(grp_names_t1[-sub_grp], grp_names_t2) # reporting groups for final output

  wildpops <- dat_in$wildpops
  K <- length(wildpops)

  hatcheries <- dat_in$hatcheries
  H <- length(hatcheries)

  allpops <- c(wildpops, hatcheries)

  na_i <- which(is.na(iden))

  iden <- factor(iden, levels = seq(K + H))

  trait_fac <- factor(rep(names(nalleles), nalleles), levels = names(nalleles))

  trait_fac2 <- factor(rep(names(nalleles2), nalleles2), levels = names(nalleles2))

  # specifications ----
  rdirich <- function(alpha0) {
    if (sum(alpha0) > 0) {
      vec = stats::rgamma(length(alpha0), alpha0, 1)
      vec = vec / sum(vec)
      vec[vec == 0] = .Machine$double.xmin
      vec
    } else {
      rep(0, length(alpha0))
    }
  } # og random dirichlet by jj

  message(paste0("Running model (and the category is... ", sample(categories, 1), "!)"))
  run_time <- Sys.time()

  if (isTRUE(cond_gsi)) nadapt = 0
  n_burn <- ifelse(keep_burn, 0, nburn)

  chains <- seq(nchains)
  cl <- parallel::makePSOCKcluster(nchains)
  doParallel::registerDoParallel(cl, cores = nchains)
  if (!is.null(seed)) doRNG::registerDoRNG(seed, once = TRUE)

  # initial values ----
  ## tier 1
  # hyper-param for relative freq q (allele)
  beta <- matrix(0, nrow = nrow(y), ncol = ncol(y))

  beta[1:K, ] <-
    matrix(
      rep(1 / nalleles, nalleles),
      nrow = K, # number of wildpops (i.e. collection)
      ncol = sum(nalleles),
      byrow = TRUE
    ) # genetic part of prior (beta)

  t_q <- apply(y + beta, 1, function(rw) {
    unlist(tapply(rw, trait_fac, function(betty) {
      if (sum(betty) > 0) {
        betty / sum(betty)
      } else {
        rep(1, length(betty))
      }
    }, simplify = FALSE)[names(nalleles)])
  }) # transposed allele freq

  freq <- matrix(
    0,
    nrow = nrow(x),
    ncol = K + H,
    dimnames = list(rownames(x), allpops)
  )

  if (H > 0 & length(na_i) < nrow(x)) {
    freq[-na_i, hatcheries] <-
      t(sapply(as.integer(iden[-na_i]) - K, function(m) {
        ans = rep(0L, H)
        ans[m] = 1L
        ans
      }))
  } # only when both reporting groups and sample include hatcheries

  # genotype freq prod f(x_m|q_k)h(a_m|pi_k)
  # rows = indiv, cols = pops
  freq[na_i, wildpops] <- exp(x[na_i,] %*% log(t_q[, 1:K]))

  p_prior <- # alpha, hyper-param for p (pop props)
    (1 / table(grps) / max(grps))[grps]

  if (!is.null(p1_prior_weight)) {
    p_pr_wt <- p1_prior_weight %>%
      dplyr::mutate(dplyr::across(weight, ~ replace(., . == 0, 1e-5))) %>%
      dplyr::arrange(grpvec) %>%
      dplyr::pull(weight)

    p_prior <- prop.table(p_prior * p_pr_wt[grps])
  }

  iden[na_i] <- unlist(lapply(na_i, function(m) {
    sample(K, 1, FALSE, (p_prior * freq[m, ])[seq.int(K)]) # only for wild pops
  }))

  p <- rdirich(table(iden) + p_prior)

  ## tier 2
  beta2  <-
    matrix(
      rep(1 / nalleles2, nalleles2),
      nrow = nrow(y2), # tier 2 has no hatchery
      ncol = ncol(y2),
      byrow = TRUE
    )

  t_q2 <- apply(y2 + beta2, 1, function(rw) {
    unlist(tapply(rw, trait_fac2, function(betty) {
      if (sum(betty) > 0) {
        betty / sum(betty)
      } else {
        rep(1, length(betty))
      }
    }, simplify = FALSE)[names(nalleles2)])
  }) # transposed allele freq

  freq2 <- exp(x2 %*% log(t_q2))

  p2_prior <- # alpha, hyper-param for p2 (pop props)
    (1 / table(p2_grps) / max(p2_grps))[p2_grps]

  if (!is.null(p2_prior_weight)) {
    p2_pr_wt <- p2_prior_weight %>%
      dplyr::mutate(dplyr::across(weight, ~ replace(., . == 0, 1e-5))) %>%
      dplyr::arrange(grpvec) %>%
      dplyr::pull(weight)

    p2_prior <- prop.table(p2_prior * p2_pr_wt[p2_grps])
  }

  iden2 <- apply(freq2, 1, function(frq_rw) {
    sample(nrow(y2), 1, FALSE, (p2_prior * frq_rw))
  })

  iden2 <- factor(iden2, levels = seq(nrow(y2)))

  p2 <- rdirich(table(iden2[iden %in% which(grps %in% sub_grp)]) + p2_prior)

  # parallel chains ----
  out_list0 <- foreach::foreach(
    ch = chains, .packages = c("magrittr", "tidyr", "dplyr")
    ) %dorng% {

      p_out <- p2_out <- pp2_out <- list()
      if (iden_output == TRUE) iden1_out <- iden2_out <- list()

    ## gibbs loop ##
    for (rep in seq(nreps + nadapt)) {

      if (!cond_gsi & rep > nadapt) { # no cond gsi or passed adapt stage

        x_sum <- matrix(0L, nrow = nrow(y), ncol = ncol(y))

        x_sum[as.integer(sort(unique(iden))),] <-
          rowsum(x, iden) %>%
          tidyr::replace_na(0) # colsums for new assignment

        beta_prm <- y + beta + x_sum # posterior q ~ dirich(b')

        t_q <- apply(beta_prm, 1, function(rw) {
          unlist(tapply(rw, INDEX = trait_fac, FUN = rdirich))
        })

        freq[na_i, wildpops] <- exp(x[na_i,] %*% log(t_q[, 1:K]))

        x2_sum <- matrix(0L, nrow = nrow(y2), ncol = ncol(y2))

        x2_sum[as.integer(sort(unique(iden2))),] <-
          rowsum(x2, iden2) %>%
          tidyr::replace_na(0) # colsums for new assignment

        beta2_prm <- y2 + beta2 + x2_sum # posterior q ~ dirich(b')

        t_q2 <- apply(beta2_prm, 1, function(rw) {
          unlist(tapply(rw, INDEX = trait_fac2, FUN = rdirich))
        })

        freq2 <- exp(x2 %*% log(t_q2))

      } # if no cond_gsi

      iden[na_i] <- unlist( lapply(na_i, function(m) {
        sample(K, 1, FALSE, (p * freq[m, ])[seq.int(K)])
      }))

      i <- 1
      while (sum(iden %in% which(grps %in% sub_grp)) < 1 & i < 100) {
        iden[na_i] <- unlist( lapply(na_i, function(m) {
          sample(K, 1, FALSE, (p * freq[m, ])[seq.int(K)])
        }))

        i <- i + 1
      } # do this to help prevent no iden == sub_grp (when 1st tier markers are shit)

      p <- rdirich(table(iden) + p_prior)

      iden2 <- apply(freq2, 1, function(frq_rw) {
        sample(nrow(y2), 1, FALSE, (p2 * frq_rw))
      })

      iden2 <- factor(iden2, levels = seq(nrow(y2)))

      p2 <- rdirich(table(iden2[iden %in% which(grps %in% sub_grp)]) + p2_prior)

      # record output based on keep or not keep burn-ins
      if (rep > nadapt) { # after adaptation stage
        if ((rep - nadapt) > n_burn & (rep - nadapt - n_burn) %% thin == 0) {

          it <- (rep - nadapt - n_burn) / thin
          # trace output in reporting groups
          # p_grp <- tapply(p, grps, sum)
          # p2_grp <- tapply(p2, p2_grps, sum)
          # p_out[[it]] <- c(p_grp, it, ch)
          # p2_out[[it]] <- c(p2_grp, it, ch)
          # pp2_out[[it]] <- c(p_grp[-sub_grp], p2_grp * sum(p_grp[sub_grp]), it, ch)

          # trace output in collections
          p_out[[it]] <- c(p, it, ch)
          p2_out[[it]] <- c(p2, it, ch)
          pp2_out[[it]] <- c(p[which(!grps %in% sub_grp)], p2 * sum(p[which(grps %in% sub_grp)]), it, ch)
          if (iden_output == TRUE) {
            iden1_out[[it]] <- iden
            iden2_out[[it]] <- iden2
          }

        } # if rep > nburn & (rep-nburn) %% thin == 0
      } # if rep > nadapt

    } # end gibbs loop

      if (iden_output == TRUE) {
        out_items <- list(p_out, p2_out, pp2_out, iden1_out, iden2_out)
      } else {
        out_items <- list(p_out, p2_out, pp2_out)
      }

      lapply(out_items, function(oi) {
        sapply(oi, rbind) %>%
          t() %>%
          dplyr::as_tibble()
        })

  } # end parallel chains
  # return list[[chains]][[posteriors]]

  parallel::stopCluster(cl)

  # prepare output ----
  keep_list <- ((nburn*keep_burn + 1):(nreps - nburn * isFALSE(keep_burn)))[!((nburn*keep_burn + 1):(nreps - nburn * isFALSE(keep_burn))) %% thin] / thin # for summary only

  ## tier 1
  out_list1 <- lapply(out_list0, function(ol) ol[[1]])

  p1_combo <-
    lapply(out_list1, function(ol) {
      ol %>%
        dplyr::select(1:(ncol(.)-2)) %>%
        t() %>% rowsum(., grps) %>% t() %>% as.data.frame() %>%
        stats::setNames(grp_names_t1)
      })

  mc_pop1 <- coda::as.mcmc.list(
    lapply(p1_combo, function(rlist) coda::mcmc(rlist[keep_list,]))
    )

  summ_pop1 <- summ_func(p1_combo, keep_list, mc_pop1, grp_names_t1, nchains)

  ## tier 2
  out_list2 <- lapply(out_list0, function(ol) ol[[2]])

  p2_combo <-
    lapply(out_list2, function(ol) {
      ol %>%
        dplyr::select(1:(ncol(.)-2)) %>%
        t() %>% rowsum(., p2_grps) %>% t() %>% as.data.frame() %>%
        stats::setNames(grp_names_t2)
      })

  mc_pop2 <- coda::as.mcmc.list(
    lapply(p2_combo, function(rlist) coda::mcmc(rlist[keep_list,]))
    )

  summ_pop2 <- summ_func(p2_combo, keep_list, mc_pop2, grp_names_t2, nchains)

  ## combine tiers
  out_list <- lapply(out_list0, function(ol) ol[[3]])

  p_combo <-
    lapply(out_list, function(ol) {
      ol %>%
        dplyr::select(1:(ncol(.)-2)) %>%
        t() %>%
        rowsum(., c(grps[which(!grps %in% sub_grp)], (p2_grps + max(grps)))) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(grp_names)
      })

  mc_pop <- coda::as.mcmc.list(
    lapply(p_combo, function(rlist) coda::mcmc(rlist[keep_list,]))
    )

  summ_pop <- summ_func(p_combo, keep_list, mc_pop, grp_names, nchains)

  # get output together
  msgsi_out <- list()

  msgsi_out$summ_t1 <- summ_pop1

  msgsi_out$trace_t1 <-
    out_list1 %>%
    dplyr::bind_rows() %>%
    stats::setNames(c(dat_in$y$collection, "itr", "chain"))

  msgsi_out$summ_t2 <- summ_pop2

  msgsi_out$trace_t2 <-
    out_list2 %>%
    dplyr::bind_rows() %>%
    stats::setNames(c(dat_in$y2$collection, "itr", "chain"))

  msgsi_out$summ_comb <- summ_pop

  msgsi_out$trace_comb <-
    out_list %>%
    dplyr::bind_rows() %>%
    stats::setNames(c(c(dat_in$y$collection[which(!grps %in% sub_grp)], dat_in$y2$collection),
                      "itr", "chain"))

  msgsi_out$comb_groups <- dat_in$comb_groups

  if (iden_output == TRUE) {
    msgsi_out$idens_t1 <-
      lapply(out_list0, function(ol) ol[[4]]) %>%
      dplyr::bind_rows()

    msgsi_out$idens_t2 <-
      lapply(out_list0, function(ol) ol[[5]]) %>%
      dplyr::bind_rows()
  }

  # if (!is.null(file_path)) save(msgsi_out, file = file_path)
  if (!is.null(file_path)) {
    message(paste("Ms.GSI output saved in", file_path))
    out_files <- c("summ_t1", "trace_t1", "summ_t2", "trace_t2", "summ_comb", "trace_comb", "comb_groups")
    sapply(out_files, function(i) {
      readr::write_csv(msgsi_out[[i]], file = paste0(file_path, "/", i, ".csv"))
    })
    if (iden_output == TRUE) {
      message(paste("IA posteriors saved in", file_path))
      iden_files <- c("idens_t1", "idens_t2")
      sapply(iden_files, function(j) {
        readr::write_csv(msgsi_out[[j]], file = paste0(file_path, "/", j, ".csv"))
      })
    } # if iden_output
  } # if save file

  print(Sys.time() - run_time)
  message(format(Sys.time(), "%B-%d-%Y %H:%M"))

  return(msgsi_out)

}


#' Summary function
#'
#' Calculate statistics for summary output.
#'
#' @param combo_file Trace file.
#' @param keeplist Which iteration to keep in output.
#' @param mc_file Trace formatted as MC file.
#' @param groupnames Group names for specific stage.
#' @param n_ch Number of MCMC chains
#'
#' @noRd
summ_func <- function(combo_file, keeplist, mc_file, groupnames, n_ch) {

  lapply(combo_file, function(rlist) rlist[keeplist, ]) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_longer(cols = tidyr::everything()) %>%
    dplyr::summarise(
      mean = mean(value),
      median = stats::median(value),
      sd = stats::sd(value),
      ci.05 = stats::quantile(value, 0.05),
      ci.95 = stats::quantile(value, 0.95),
      p0 = mean(value < 5e-7),
      .by = name
    ) %>%
    dplyr::mutate(
      GR = { if (n_ch > 1) {
        coda::gelman.diag(mc_file,
                          transform = FALSE,
                          autoburnin = FALSE,
                          multivariate = FALSE)$psrf[,"Point est."]
      } else NA },
      n_eff = coda::effectiveSize(mc_file) # in alphabetical order like tidyverse summarise()
    ) %>%
    dplyr::mutate(name_fac = factor(name, levels = groupnames)) %>%
    dplyr::arrange(name_fac) %>%
    dplyr::select(-name_fac) %>%
    dplyr::rename(group = name)

}

utils::globalVariables(c(".", "ch", "chain", "itr", "name", "name_fac", "value"))






























