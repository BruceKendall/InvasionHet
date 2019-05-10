#' ---
#' title: model.R
#' author: Bruce Kendall
#' ---
#' 
#' Contains functions for simulating the *Arabidopsis* model. To generate a version 
#' with more easily readable documentation, compile the document using
#' `rmarkdown::render("model.R")`
#' 
#' The functions all use elements of lists called `params` and `controls`. 
#' The elements of these lists are:
#' 
#' ### Elements of `params`
#'  
#' `n_types` (integer)
#' :    Number of genotypes. Ignored by `iterate_model_ler()`; for this model, all 
#' remaining parameters should be scalars (other than the matrix of kernel parameters)
#' 
#' `a_Gompertz` (scalar, or vector of length `n_types`)
#' :    Intercept of the Gompertz model of density dependence. 
#' Should be positive (or else the population will not grow from a single individual)
#' 
#' `b_Gompertz` (scalar, or vector of length `n_types`)
#' :    Slope of the Gompertz model of density dependence. Should be between -1 and zero
#' 
#' `sigma_seed_time` (positive scalar)
#' :    Standard deviation of temporal stochasticity in log seed production
#' 
#' `sigma_seed_rep` (positive scalar)
#' :    Standard deviation of among-rep stochasticity in log seed production
#' 
#' `theta_seed` (positive scalar)
#' :    Variance inflation factor for quasi-Poisson demographic stochasticity
#' 
#' <!--Need to add kernel parameters, pot_width --> 
#' 
#' ### Elements of `controls`
#' 
#' `n_pots` (integer; calculated by model)
#' :    Number of pots in the array
#' 
#' `n_reps` (integer)
#' :    Number of replicate simulations
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
#' ### State variables
#' The state variables are `Adults` and (internally) `Seeds`. These are structured as
#' a matrix with each row being a replicate and each column a pot. 
#' 
#' <!-- ############################################################################# --> 
#' # `iterate_model_ler()`
#' Iterates a single instance of the Ler (single genotype) model one time step
iterate_model_ler <- function(Adults, params, controls) {
  n_pots <- ncol(Adults)
  #### SEED PRODUCTION ####
  # Density dependence in seed production
  Seeds <- Gompertz_seeds(Adults, params)
  
  # Environmental stochasticicy in seed production?
  if (controls$ES_seeds) {
    Seeds <- ES_seeds(Seeds, params, controls$n_reps)
  }
  
  # Demographic stochasticity in seed production?
  if (controls$DS_seeds) {
    Seeds <- DS_seeds(Seeds, params)
  } else {
    Seeds <- round(Seeds)
  }
  
  #### DISPERSAL ####
  ## This is hard-coded for the generalized gamma distribution
  
  ## Calculated dispersal from each pot
  # Kernel stochasticity?
  if (cotrols$kernel_stoch) {
    kernel_params <- kernal_stoch(params, controls)
  } else { # Distribute the genotype-specific parameters across pots and reps
    array_dim <- c(controls$n_reps,
                   n_pots)
    kernel_params <- list(
      frac_dispersing = array(params$frac_dispersing, array_dim),
      gg_mu           = array(params$gg_mu,           array_dim),
      gg_sigma        = array(params$gg_sigma,        array_dim),
      gg_Q            = array(params$gg_Q,            array_dim)
    )
  }
  
  # Seed sampling?
  if (controls$seed_sampling) {
    dispersed_seeds_by_pot <- seed_sampling(Seeds, kernel_params, params, controls)
  } else {
    dispersed_seeds_by_pot <- round(det_kernel(Seeds, kernel_params, params, controls))
  }
  
  ## Combine all the dispersed seeds
  Seeds <- combine_dispersed_seeds(dispersed_seeds_by_pot, controls$n_reps, 
                                   n_pots + dispersed_seeds_by_pots$max_dist)
  
  return(Seeds)
}

#' <!-- ############################################################################# --> 
#' # `Gompertz_seeds()`
#' Uses the Gompertz model to create density dependence in seed production. 
#' The model is:
#' $$\log(S/A) = a + b \log A$$
#' $$\log S = \log A + a + b \log A$$
#' $$S = \exp[a + (b+1) \log A].$$
Gompertz_seeds <- function(Adults, params) {
  with(params, {
    expected_seeds <- exp(a_Gompertz + (1 + b_Gompertz) * log(Adults))
    return(expected_seeds)
  })
}

#' <!-- ############################################################################# --> 
#' # `ES_seeds()`
#' Add environmental stochasticity to the seed production. There are two components: a
#' temporal component (with standarad deviation `sigma_seed_time`) that applies equally
#' to all reps, and an among-rep component (with standarad deviation `sigma_seed_rep`)
#' that applies equally to all pots within each rep.
#' 
#' The variation is assumed to be log-normal; the sigma parameters are on the 
#' log-transformed scale.
ES_seeds <- function(Seeds, params, n_rep) {
  with(params, {
    ## Log-transform and apply temporal ES equally to all reps
    lseeds <- log(Seeds) + rnorm(1, 0, sigma_seed_time)
    
    ## Apply inter-rep ES equally to all pots within each rep
    ## This uses recyling, and depends on each row being a rep
    lseeds <- lseeds + rnorm(n_rep, 0, sigma_seed_rep)
    
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
DS_seeds <- function(Seeds, params) {
  n <- prod(dim(Seeds))
  rqpois(n, Seeds, params$theta)
}

#' <!-- ############################################################################# --> 
#' # `kernel_stoch()`
#' Calculate rep- and pot-specific dispersal kernels
kernel_stoch <- function(params, controls) {
  library(MASS)
  with(controls, with(params, {
    if (kernel_stoch_pots) { # Each pot gets different parameters
      ndraw <-  n_pots * n_reps
      
    } else { # All pots w/in rep are identical
      ndraw <- n_reps
    }
    fd_draws <- rbeta2(ndraw, frac_dispersing, fd_sdev)
    mv_draws <- mvrnorm(ndraw, c(gg_mu, gg_sigma, gg_Q), gg_cov, empirical = TRUE)
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
      new_rvs <- mvrnorm(neg_sigma + 3, c(gg_mu, gg_sigma, gg_Q), gg_cov, 
                         empirical = TRUE)[-(1:3), ]
      kernel_params$gg_mu[indx] <- new_rvs[, 1]
      kernel_params$gg_sigma[indx] <- new_rvs[, 2]
      kernel_params$gg_Q[indx] <- new_rvs[, 3]
      neg_sigma <- sum(kernel_params$gg_sigma <= 0)
    }
    return(kernel_params)
  }))
}

#' <!-- ############################################################################# --> 
#' # `det_kernel()`
#' Distribute seeds according to a deterministic kernel
det_kernel <- function(Seeds, kernel_params, params, controls){
  #### Seeds staying in maternal (home) pot ####
  home_pot <- Seeds * (1 - kernel_params$frac_dispersing)
  
  #### Seeds dispersing forward ####
  disp_seeds <- Seeds * kernel_params$frac_dispersing/2
  
  # Because we don't know how far seeds might disperse, we have to grow the
  # array using a while loop until the expected number of seeds at all distances
  # is less than 0.5 for all pots and all reps
  max_dist <- 6
  reached_max_dist <- FALSE
  while(!reached_max_dist) {
    # forward_dispersal is a 3-d array, with dimensions n_reps, n_pots, max_dist
    # It will hold the expected dispersal number at each distance for each rep x pot
    forward_dispersal <- array(dim=c(dim(Seeds), max_dist)) 
    # dvec is the pot boundaries. We use diff(pgengamma(dvec, ...)) to get the 
    # fraction of the distribution in each pot.
    dvec <- controls$pot_width * (0:max_dist)
    # There might be a clever way to do this with apply(), but it would take 
    # lots of brain power to figure it out...
    for (pot in 1:ncol(Seeds)) {
      for (rep in 1:nrow(Seeds)) {
        forward_dispersal[rep, pot, ] <- diff(pgengamma(dvec,
                                            kernel_params$gg_mu[rep, pot],
                                            kernel_params$gg_sigma[rep, pot],
                                            kernel_params$gg_Q[rep, pot])) *
                                         disp_seeds[rep, pot]
      }
    }
    if (min(forward_dispersal) < 0.5) { # End the while loop
      reached_max_dist <- TRUE
    } else { # increment max_dist and continue on
      max_dist <- max_dist + 1
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
seed_sampling <- function(Seeds, kernel_params, params, controls) {
  #### Seeds staying in maternal (home) pot ####
  np <- array(c(Seeds, (1 - kernel_params$frac_dispersing)), dim = c(dim(Seeds), 2))
  home_pot <- apply(np, c(1, 2), function(x) rbinom(1, x[1], x[2]))
  
  #### Seeds leaving home pot ####
  disp_seeds <- Seeds - home_pot
  max_ds <- max(disp_seeds) # to set the array dimension to pad to
  
  # Put dispersing seeds and all params into a common array
  ngg <- array(c(disp_seeds, unlist(kernel_params)), dim = c(dim(Seeds), 5))
  # Generate a vector of seed-specific dispersal distances for each pot/rep.
  # To get conformable dimensions, pad results for pots w/ less than max_ds 
  # seeds with zeros
  disp_dist <- apply(ngg, c(1,2), 
                     function(x) c(rgengamma(x[1], x[3], x[4], x[5]), 
                                   rep(0, times = max_ds - x[1]))) %>%
    aperm(c(2, 3, 1)) # apply puts the calculated value in the first dim
  
  # Distribute seeds into forward and backward dispersal, and rescale distance to pots
  forward_draw <- rbernoulli(prod(dim(disp_dist)))
  disp_forward <- ceiling(forward_draw * disp_dist / controls$pot_width)
  disp_backward <- ceiling((!forward_draw) * disp_dist / controls$pot_width)
  max_dist <- max(c(disp_forward, disp_backward)) # farthest dispersing seed
  # Tabulate the number at each distance
  forward_dispersal <- apply(disp_forward, c(1, 2), disp_table, 
                             max_dist = max_dist) %>%
    aperm(c(2, 3, 1))
  backward_dispersal <- apply(disp_backward, c(1, 2), disp_table, 
                              max_dist = max_dist) %>%
    aperm(c(2, 3, 1))
  
  return(list(home_pot = home_pot, 
              forward_dispersal = forward_dispersal,
              backward_dispersal = backward_dispersal, 
              max_dist = max_dist))
}

disp_table <- function(dists, max_dist) {
  raw_table <- table(dists)
  if (names(raw_table)[1] == "0") { # Drop the zeros, if any
    raw_table <- raw_table[-1]
  }
  dist_vals <- as.numeric(names(raw_table))
  dist_counts <- numeric(max_dist)
  dist_counts[dist_vals] <- raw_table
  return(dist_counts)
}

#' <!-- ############################################################################# --> 
#' # `combine_dispersed_seeds()`
#' Take the dispersed seeds from each pot and combine them to get net dispersal across
#' the whole runway
#' 
combine_dispersed_seeds <- function(seeds_by_pot, n_reps, n_pots) {
  with(seeds_by_pot, {
    dist_x_reps <- max_dist * n_reps
    ### Non-dispersing seeds
    disp_seeds <- home_pot
  
    ### Backwards dispersing seeds
    # Add a temporary buffer to hold the seeds that disperse off the back end of 
    # the runway
    disp_seeds <- matrix(c(rep(0, dist_x_reps), disp_seeds), n_reps, max_dist + n_pots)
    # Sum up the seeds, with an appropriate backshift
    for (dist in 1:max_dist) {
      disp_seeds[ , (max_dist - dist) + (1:n_pots)] <- backward_dispersal[ , , dist] +
        disp_seeds[ , (max_dist - dist) + (1:n_pots)]
    }
    # Get rid of the temporary buffer
    disp_seeds <- disp_seeds[ , -(1:max_dist)]
    
    ### forwards dispersing seeds
    # Add a buffer for seeds dispersing into new territory
    disp_seeds <- matrix(c(disp_seeds, rep(0, dist_x_reps)), n_reps, max_dist + n_pots)
    # Sum up the seeds, with an appropriate forward shift
    for (dist in 1:max_dist) {
      disp_seeds[ , dist + (1:n_pots)] <- disp_seeds[ , dist + (1:n_pots)] + 
        forward_dispersal[ , , dist]
    }
    
    return(disp_seeds)
  })
}
