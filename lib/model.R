#' ---
#' title: model.R
#' author: Bruce Kendall
#' ---
#' 
#' Contains functions for simulating the *Arabidopsis* model. To generate a version 
#' with more easily readable documentation, compile the document using
#' `rmarkdown::render("model.R")`
#' 
#' The biological parameters, experimental design parameters, and simulation settings are
#' passed in (and around) through the lists `plant_params`, `expt_params`, and
#' `sim_settings`, respectively.
#' 
#' ## `plant_params`
#' This is a list containing all of the biological parameters for single genotype. For
#' Ler, this is identical to the `Ler_params` list that is produced by
#' `munge/10-make-Ler-params.R`. For the RILs, this is identical to a top-level subset of
#' the `RIL_params` list that is produced by `munge/20-make-RIL-params.R` (e.g.,
#' `RIL_params[[1]]` gives the parameter list for the first RIL).
#' 
#' All elements are real-valued scalars, except as noted
#'  
#' `a_Gompertz` 
#' :    Intercept of the Gompertz model of density dependence. 
#' Should be positive (or else the population will not grow from a single individual)
#' 
#' `b_Gompertz` 
#' :    Slope of the Gompertz model of density dependence. Should be between -1 and zero
#' 
#' `sigma_seed_time` (must be positive)
#' :    Standard deviation of temporal stochasticity in log seed production
#' 
#' `sigma_seed_rep` (must be positive)
#' :    Standard deviation of among-rep stochasticity in log seed production
#' 
#' `theta_seed` (must be positive)
#' :    Variance inflation factor for quasi-Poisson demographic stochasticity
#' 
#' <!--Need to add kernel parameters, max_pots. Also need to add max_pots to munge scripts --> 
#' 
#' ## `expt_params`
#' A list of values that describe the experimental setup in the greenhouse. 
#' All are scalar.
#' 
#' `n_reps` (integer)
#' :    Number of replicate populations of each design
#' 
#' `new_pots` (integer)
#' :    The number of new (empty) pots added to the end of each runway, beyond the 
#' furthest dispersed seed from the prior generation. In the Ler experiments, this 
#' value was 8. 
#' 
#' `pot_width` (numeric)
#' :    Width of a pot in cm. Was 7 in all experiments
#' 
#' ## `sim_settings`
#' A list of settings that turn the various sources of stochasticity on and off, as well
#' as setting which landscape is being simulated.
#' 
#' `n_pots` (integer; calculated by model)
#' :    Number of pots in the array
#' 
#' `ES_seeds` (logical)
#' :    Set to `TRUE` to include environmental stochasticity in seed production
#' 
#' `DS_seeds` (logical)
#' :    Set to `TRUE` to include demographic stochasticity in seed production. If `FALSE`,
#' then seed number is simply rounded to the nearest integer.
#' 
#' `kernel_stoch` (logical)
#' :    Set to `TRUE` to include dispersal kernel heterogeneity.
#' 
#' `seed_sampling` (logical)
#' :    Set to `TRUE` to have each seed sample the dispersal kernel independently. If
#' `FALSE`, then the number of seeds dispersing to each pot is the expected number
#' rounded to the nearest integer.
#' 
#' `gap_size` (non-negative integer)
#' :    Size of gaps, measured in number of pots. Set to zero for continuous landscape
#' 
#' 
#' ### State variables
#' The state variables are `Adults` and (internally) `Seeds`. These are structured as
#' a matrix with each row being a replicate and each column a pot. 
#' 
#' <!-- ############################################################################# --> 
#' # `iterate_genotype()`
#' Iterates the single genotype model one time step.
#' If this is part of a multi-genotype simulation, then `N_tot` needs to provided; it
#' should be the summed numbers of individuals across all genotypes in each pot/rep (to
#' use for density dependence).
iterate_genotype <- function(Adults, plant_params, expt_params, sim_settings, 
                             ES_seed_time = NULL, N_tot = Adults) {
  n_pots <- ncol(Adults)
  if (is.null(sim_settings$max_pots)) sim_settings$max_pots <- 10
  if (sim_settings$ES_seeds & is.null(ES_seed_time)) {
    ES_seed_time <- rnorm(1, 0, plant_params$sigma_seed_time)
  }
  
  #### RECORD ACTUAL LENGTH OF EACH RUNWAY ####
  farthest_adult <- last_occupied_pot(Adults, zero = 1) 
  runway_end <- farthest_adult + expt_params$new_pots
  
  #### SEED PRODUCTION ####
  # Density dependence in seed production
  Seeds <- Gompertz_seeds(Adults, plant_params, N_tot)
  
  # Environmental stochasticity in seed production?
  if (sim_settings$ES_seeds) {
    Seeds <- ES_seeds(Seeds, plant_params, expt_params$n_reps, ES_seed_time)
  }
  
  # Demographic stochasticity in seed production?
  if (sim_settings$DS_seeds) {
    Seeds <- DS_seeds(Seeds, plant_params)
  } else {
    Seeds <- round(Seeds)
  }
  
  #### DISPERSAL ####
  ## This is hard-coded for the generalized gamma distribution
  
  ## Calculated dispersal from each pot
  # Kernel stochasticity?
  if (sim_settings$kernel_stoch) {
    kernel_params <- kernel_stoch(plant_params, expt_params, sim_settings, n_pots)
  } else { # Distribute the genotype-specific parameters across pots and reps
    array_dim <- c(expt_params$n_reps,
                   n_pots)
    kernel_params <- list(
      frac_dispersing = array(plant_params$frac_dispersing, array_dim),
      gg_mu           = array(plant_params$gg_mu,           array_dim),
      gg_sigma        = array(plant_params$gg_sigma,        array_dim),
      gg_Q            = array(plant_params$gg_Q,            array_dim)
    )
  }

  # Seed sampling?
  if (sim_settings$seed_sampling) {
    dispersed_seeds_by_pot <- seed_sampling(Seeds, kernel_params, expt_params$n_reps,
                                            expt_params$pot_width, n_pots)
  } else {
    dispersed_seeds_by_pot <- 
      det_kernel(Seeds, kernel_params, sim_settings$max_pots, 
                 expt_params$n_reps, n_pots) %>%
        lapply(round)
  }
  
  ## Truncate ridiculous dispersal events
  if (dispersed_seeds_by_pot$max_dist > sim_settings$max_pots) {
    dispersed_seeds_by_pot <- within(dispersed_seeds_by_pot, {
      forward_dispersal <- forward_dispersal[, , 1:sim_settings$max_pots, drop = FALSE]
      backward_dispersal <- backward_dispersal[, , 1:sim_settings$max_pots, drop = FALSE]
      max_dist <- sim_settings$max_pots
    })
  }
  
  ## Combine all the dispersed seeds
  Seeds <- combine_dispersed_seeds(dispersed_seeds_by_pot, expt_params$n_reps, 
                                   n_pots, runway_end)
  
  n_pots <- ncol(Seeds)
  
  # Zero out the seeds in the gaps
  Seeds <- gapify(Seeds, sim_settings$gap_size, expt_params$n_reps, n_pots)
  
  return(Seeds)
}

#' <!-- ############################################################################# --> 
#' # `Gompertz_seeds()`
#' Uses the Gompertz model to create density dependence in seed production. 
#' The model is:
#' $$\log(S/A) = a + b \log A$$
#' $$\log S = \log A + a + b \log A$$
#' $$S = \exp[a + (b+1) \log A].$$
Gompertz_seeds <- function(Adults, plant_params, N_tot) {
  with(plant_params, {
    expected_seeds <- Adults * exp(a_Gompertz) * N_tot ^ b_Gompertz
    expected_seeds[Adults * N_tot == 0] <- 0
    return(expected_seeds)
  })
}

#' <!-- ############################################################################# --> 
#' # `ES_seeds()`
#' Add environmental stochasticity to the seed production. There are two components: a
#' temporal component (with standard deviation `sigma_seed_time`) that applies equally
#' to all reps, and an among-rep component (with standard deviation `sigma_seed_rep`)
#' that applies equally to all pots within each rep.
#' 
#' The variation is assumed to be log-normal; the sigma parameters are on the 
#' log-transformed scale.
ES_seeds <- function(Seeds, plant_params, n_rep, ES_seed_time) {
  with(plant_params, {
    ## Log-transform and apply temporal ES equally to all reps
    lseeds <- log(Seeds) + rnorm(1, 0, sigma_seed_time)
    
    ## Apply inter-rep ES equally to all pots within each rep
    ## This uses recycling, and depends on each row being a rep
    lseeds <- lseeds + ES_seed_time
    
    ## Return anti-log-transformed result
    return(exp(lseeds))
  })
}

#' <!-- ############################################################################# --> 
#' # `rqpois()`
#' Generate random numbers from a quasi-Poisson "distribution," following logic and code
#' from https://www.r-bloggers.com/generate-quasi-poisson-distribution-random-variable/
#' and https://www.r-bloggers.com/generating-a-quasi-poisson-distribution-version-2/.
#' The basic idea is to use a negative binomial distribution with mean $\mu$ and size
#' parameter $\mu/(\theta - 1)$ to generate variates with mean $\mu$ and variance 
#' $\theta \mu$, where $\theta$ is the variance inflation parameter from the GLM.
#' 
#' ### Parameters
#' `n` (integer)
#' :    Number of random numbers to be drawn
#' 
#' `mu` (positive scalar)
#' :    Mean of the distribution
#' 
#' `theta` (positive scalar)
#' :    Variance inflation factor
#' 
#' It is conceivable that this will work with vectors of `mu` and `theta`, but this
#' has not been tested.
#' 
rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}

#' <!-- ############################################################################# --> 
#' # `DS_seeds()`
#' Add demographic stochasticity to the seed production, using the negative binomial
#' approximation to the quasi-Poisson.
DS_seeds <- function(Seeds, plant_params) {
  n <- prod(dim(Seeds))
  # Don't want to see the warnings thrown when mu = 0
  rand_seeds <- array(suppressWarnings(rqpois(n, Seeds, plant_params$theta)), dim(Seeds))
  rand_seeds[Seeds == 0] <- 0 # rqpois returns NA for these cases
  rand_seeds
}

#' <!-- ############################################################################# --> 
#' # `kernel_stoch()`
#' Calculate rep- and pot-specific dispersal kernels
kernel_stoch <- function(plant_params, expt_params, sim_settings, n_pots) {
  library(MASS)
  with(sim_settings, with(expt_params, with(plant_params, {
    if (kernel_stoch_pots) { # Each pot gets different parameters
      ndraw <-  n_pots * n_reps
      
    } else { # All pots w/in rep are identical
      ndraw <- n_reps
    }
    fd_draws <- rbeta2(ndraw, frac_dispersing, fd_sdev)
    mv_draws <- mvrnorm(ndraw, c(gg_mu, gg_sigma, gg_Q), gg_cov)
    array_dim <- c(n_reps, n_pots)
    kernel_params <- list(
      frac_dispersing = matrix(fd_draws, n_reps, n_pots, byrow = FALSE),
      gg_mu           = matrix(mv_draws[, 1], n_reps, n_pots, byrow = FALSE),
      gg_sigma        = matrix(mv_draws[, 2], n_reps, n_pots, byrow = FALSE),
      gg_Q            = matrix(mv_draws[, 3], n_reps, n_pots, byrow = FALSE)
    )
    
    # Check for, and replace, negative values of sigma
    neg_sigma <- sum(kernel_params$gg_sigma <= 0)
    while(neg_sigma > 0) {
      indx <- kernel_params$gg_sigma <= 0
      new_rvs <- mvrnorm(neg_sigma + 3, c(gg_mu, gg_sigma, gg_Q), gg_cov)[-(1:3), , 
                                                                          drop = FALSE]
      kernel_params$gg_mu[indx] <- new_rvs[, 1]
      kernel_params$gg_sigma[indx] <- new_rvs[, 2]
      kernel_params$gg_Q[indx] <- new_rvs[, 3]
      neg_sigma <- sum(kernel_params$gg_sigma <= 0)
    }
    return(kernel_params)
  })))
}

#' <!-- ############################################################################# --> 
#' # `det_kernel()`
#' Distribute seeds according to a deterministic kernel
det_kernel <- function(Seeds, kernel_params, max_dist, n_reps, n_pots){
  #### Seeds staying in maternal (home) pot ####
  home_pot <- Seeds * (1 - kernel_params$frac_dispersing)
  
  #### Seeds dispersing forward ####
  disp_seeds <- Seeds * kernel_params$frac_dispersing/2
  
  
  # `forward_dispersal` is a 3-d array, with dimensions `n_reps`, `n_pots`, `max_dist`
  # It will hold the expected dispersal number at each distance for each rep x pot
  forward_dispersal <- array(dim=c(dim(Seeds), max_dist)) 
  
  # `dvec` is the vector of pot boundaries. We use `diff(pgengamma(dvec, ...))` to get the
  # fraction of the distribution in each pot.
  dvec <- expt_params$pot_width * (0:max_dist)
  
  # There might be a clever way to do this with apply(), but it would take 
  # lots of brain power to figure it out...
  for (pot in 1:n_pots) {
    for (rep in 1:n_reps) {
      forward_dispersal[rep, pot, ] <- diff(pgengamma(dvec,
                                                      kernel_params$gg_mu[rep, pot],
                                                      kernel_params$gg_sigma[rep, pot],
                                                      kernel_params$gg_Q[rep, pot])) *
        disp_seeds[rep, pot]
    }
  }
  
  #### Backward dispersal is identical to forward dispersal ####
  return(list(home_pot = home_pot, 
              forward_dispersal = forward_dispersal,
              backward_dispersal = forward_dispersal, 
              max_dist = max_dist))
}

#' <!-- ############################################################################# --> 
#' # `seed_sampling()`
#' Distribute seeds according to independent draws from a kernel
#' 
seed_sampling <- function(Seeds, kernel_params, n_reps, pot_width, n_pots) {
  #### Seeds staying in maternal (home) pot ####
  np <- array(c(Seeds, (1 - kernel_params$frac_dispersing)), dim = c(dim(Seeds), 2))
  home_pot <- apply(np, c(1, 2), function(x) rbinom(1, x[1], x[2]))
  
  #### Seeds leaving home pot ####
  disp_seeds <- Seeds - home_pot
  max_ds <- max(disp_seeds) # to set the array dimension to pad to
  
  # Generate a vector of seed-specific dispersal distances for each pot/rep.
  # To get conformable dimensions, pad results for pots w/ less than `max_ds` 
  # seeds with zeros

  # Put dispersing seeds and all params into a common array
  ngg <- array(c(disp_seeds, unlist(kernel_params[2:5])), dim = c(dim(Seeds), 4))

  # Make unwrapped vectors of all the parameter values.
  # Requires multiple calls because apply can only return a vector
  mu_vec <-    unlist(apply(ngg, c(2,1), function(x) rep(x[2], x[1]), simplify = FALSE))
  sigma_vec <- unlist(apply(ngg, c(2,1), function(x) rep(x[3], x[1]), simplify = FALSE))
  Q_vec <-     unlist(apply(ngg, c(2,1), function(x) rep(x[4], x[1]), simplify = FALSE))
  n_seed_tot <- length(Q_vec)
  
  # Get the random numbers
  disp_dist_vec <- rgengamma(n_seed_tot, mu_vec, sigma_vec, Q_vec)

  # Fill the RNs into an array, with all zeros for empty pots and padded zeros to get
  #   to max_ds
  disp_dist <- array(0, dim = c(dim(Seeds), max_ds))
  k_end <- 0
  for (rep in 1:n_reps) {
    occupied_pots <- (1:n_pots)[disp_seeds[rep,] > 0]
    for (pot in occupied_pots) {
      k_start <- k_end + 1
      ndisp <- disp_seeds[rep, pot]
      k_end <- k_end + ndisp
      disp_dist[rep, pot, 1:ndisp] <- disp_dist_vec[k_start:k_end]
    }
  }
      
  # Distribute seeds into forward and backward dispersal, and rescale distance to pots
  forward_draw <- rbernoulli(prod(dim(disp_dist)))
  disp_forward <- ceiling(forward_draw * disp_dist / pot_width)
  disp_backward <- ceiling((!forward_draw) * disp_dist / pot_width)
  print(c(max(disp_backward), max(disp_forward)))
  max_dist <- max(c(disp_forward, disp_backward)) # farthest dispersing seed

  # Tabulate the number at each distance
  forward_dispersal <- apply(disp_forward, c(1, 2), disp_table, 
                             max_dist = max_dist, method = "tabulate") %>%
     aperm(c(2, 3, 1))
  backward_dispersal <- apply(disp_backward, c(1, 2), disp_table, 
                              max_dist = max_dist, method = "tabulate") %>%
     aperm(c(2, 3, 1))
  
  return(list(home_pot = home_pot, 
              forward_dispersal = forward_dispersal,
              backward_dispersal = backward_dispersal, 
              max_dist = max_dist))
}

disp_table <- function(dists, max_dist, method = "table") {
  dist_counts <- numeric(max_dist)
  if (method == "table") {
    raw_table <- table(dists)
    if (names(raw_table)[1] == "0") { # Drop the zeros, if any
      raw_table <- raw_table[-1]
    }
    dist_vals <- as.numeric(names(raw_table))
    dist_counts[dist_vals] <- raw_table
  } else if (method == "tabulate") {
    raw_table <- tabulate(dists)
    dist_counts[1:length(raw_table)] <- raw_table
  }
  return(dist_counts)
}

#' <!-- ############################################################################# --> 
#' # `combine_dispersed_seeds()`
#' Take the dispersed seeds from each pot and combine them to get net dispersal across
#' the whole runway
#' 
combine_dispersed_seeds <- function(seeds_by_pot, n_reps, n_pots, runway_end) {
  with(seeds_by_pot, {
    dist_x_reps <- max_dist * n_reps
    ### Non-dispersing seeds
    disp_seeds <- home_pot
  
    #### Backwards dispersing seeds  ####
    # Add a temporary buffer to hold the seeds that disperse off the back end of 
    # the runway
    disp_seeds <- matrix(c(rep(0, dist_x_reps), disp_seeds), n_reps, max_dist + n_pots)
    # Sum up the seeds, with an appropriate backshift
    for (dist in 1:max_dist) {
      disp_seeds[ , (max_dist - dist) + (1:n_pots)] <- backward_dispersal[ , , dist] +
        disp_seeds[ , (max_dist - dist) + (1:n_pots)]
    }
    # Get rid of the temporary buffer
    disp_seeds <- disp_seeds[ , -(1:max_dist), drop = FALSE]
    
    #### Forwards dispersing seeds  ####
    # Add a buffer for seeds dispersing into new territory
    disp_seeds <- matrix(c(disp_seeds, rep(0, dist_x_reps)), n_reps, max_dist + n_pots)
    # Sum up the seeds, with an appropriate forward shift
    for (dist in 1:max_dist) {
      disp_seeds[ , dist + (1:n_pots)] <- disp_seeds[ , dist + (1:n_pots)] + 
        forward_dispersal[ , , dist]
    }
    # Zero out the seeds that dispersed beyond the end of the rep-specific runway
    disp_cols <- ncol(disp_seeds)
    for (rep in 1:n_reps) {
      if (disp_cols > runway_end[rep]) {
        disp_seeds[rep, (runway_end[rep] + 1):disp_cols] <- 0
      }
    }
    
    #### Truncate the all-zero columns to keep dimensions under control  ####
    tot_seed_by_pot <- colSums(disp_seeds)
    max_dist <- max(seq_along(tot_seed_by_pot)[tot_seed_by_pot>0])
    if(is.infinite((max_dist))) max_dist <- 1
    disp_seeds <- disp_seeds[, 1:max_dist]
    
    return(disp_seeds)
  })
}

#' <!-- ############################################################################# --> 
#' # `gapify()`
#' Set the seed abundance to zero in the "pots" that are actually gaps 
#' 
gapify <- function(Seeds, gap, n_rep, n_pot) {
  if (gap > 0) { 
    gap_mask <- rep(c(1, rep(0, gap)), length = n_pot)
    gap_mask <- matrix(gap_mask, n_rep, n_pot, byrow = TRUE)
    Seeds <- Seeds * gap_mask
  }
  
  # Drop any trailing zeros
  rep_sum <- cummax(apply(Seeds, 2, sum)[n_pot:1])[n_pot:1]
  
  Seeds[, rep_sum > 0, drop = FALSE]
}

