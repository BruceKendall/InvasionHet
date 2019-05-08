### Functions to test components of model.R
### Prior to invoking these functions, run ProjectTemplate::load.project()

test_kernel_stoch <- function() {
  controls <- list(n_pots = 3,
                   n_reps = 2,
                   kernel_stoch_pots = TRUE)
  cov_mat <- matrix(c(0.15, -0.026, 0.17,
                      -0.026, 0.015, -0.025,
                      0.17, -0.025, 0.28),
                    3, 3)
  params <- list(gg_mu = 1.5,
                 gg_sigma = 0.75,
                 gg_Q = 1,
                 gg_cov = cov_mat,
                 frac_dispersing = 0.75,
                 fd_sdev = 0.1)

  # Test that the code runs
  print(kernel_stoch(params, controls))
  
  # Test for no within-rep ES
  controls$kernel_stoch_pots <- FALSE
  controls$n_reps <- 4 # mvrnorm fails if n < length(mu) and empirical == TRUE
  print(kernel_stoch(params, controls))
  
  # Generate large sample to test means, (co)variances
  controls <- list(n_pots = 100,
                   n_reps = 100,
                   kernel_stoch_pots = TRUE)
  rv_sample <- kernel_stoch(params, controls)
  print(mean(rv_sample$frac_dispersing))
  print(sqrt(var(as.vector(rv_sample$frac_dispersing))))
  print(c(mean(rv_sample$gg_mu), mean(rv_sample$gg_sigma), mean(rv_sample$gg_Q)))
  print(cov(cbind(as.vector(rv_sample$gg_mu), as.vector(rv_sample$gg_sigma), 
                  as.vector(rv_sample$gg_Q))))
  
  # Look for negative values of sigma
  print(sum(rv_sample$gg_sigma <= 0))
}

test_det_kernel <- function() {
  library(flexsurv)
  nrep <- 2
  npot <- 3
  Seeds <- matrix(100, nrep, npot)
  print(Seeds)
  
  params <- list(gg_mu = 1.5,
                 gg_sigma = 0.75,
                 gg_Q = 1,
                 frac_dispersing = 0.75)
  controls <- list(pot_width = 7)
  
  ### Tests for constant kernel
  with(params, plot(dgengamma(0:30, gg_mu, gg_sigma, gg_Q)))
  with(params, print(pgengamma(14, gg_mu, gg_sigma, gg_Q, lower = FALSE)  * 75/2))
  array_dim <- c(nrep, npot)
  kernel_params <- list(
    frac_dispersing = array(params$frac_dispersing, array_dim),
    gg_mu           = array(params$gg_mu,           array_dim),
    gg_sigma        = array(params$gg_sigma,        array_dim),
    gg_Q            = array(params$gg_Q,            array_dim)
  )
  
  d1 <- det_kernel(Seeds, kernel_params, params, controls)
  # should be identical for all pots/reps
  # home_pot should be 25
  # forward and backward should be identical
  # max_dist should be 6
  print(d1)
  
  # Expected number of seeds beyond pot 2:
  with(params, print(pgengamma(14, gg_mu, gg_sigma, gg_Q, lower = FALSE)  * 75/2))
  
  # Expected number of seeds placed in pots 3-6 (should be very close to previous)
  sum(d1$forward_dispersal[1,1,3:6])
  
  ### Tests for variable kernels
  cov_mat <- matrix(c(0.15, -0.026, 0.17,
                      -0.026, 0.015, -0.025,
                      0.17, -0.025, 0.28),
                    3, 3)
  params <- list(gg_mu = 1.5,
                 gg_sigma = 0.75,
                 gg_Q = 1,
                 gg_cov = cov_mat,
                 frac_dispersing = 0.75,
                 fd_sdev = 0.1)
  controls <- list(pot_width = 7,
                   n_pots = 2,
                   n_reps = 3,
                   kernel_stoch_pots = TRUE)
  kernel_params <- kernel_stoch(params, controls)
  Seeds <- with(controls, matrix(100, n_reps, n_pots))
  d2 <- det_kernel(Seeds, kernel_params, params, controls)
  print(d2)
  
  controls$kernel_stoch_pots = FALSE
  kernel_params <- kernel_stoch(params, controls)
  d3 <- det_kernel(Seeds, kernel_params, params, controls)
  print(d3)
}

test_seed_sampling <- function() {
  library(flexsurv)
  nrep <- 2
  npot <- 3
  Seeds <- matrix(100, nrep, npot)
  print(Seeds)
  
  params <- list(gg_mu = 1.5,
                 gg_sigma = 0.75,
                 gg_Q = 1,
                 frac_dispersing = 0.75)
  controls <- list(pot_width = 7)
  
  ### Tests for constant kernel
  array_dim <- c(nrep, npot)
  kernel_params <- list(
    frac_dispersing = array(params$frac_dispersing, array_dim),
    gg_mu           = array(params$gg_mu,           array_dim),
    gg_sigma        = array(params$gg_sigma,        array_dim),
    gg_Q            = array(params$gg_Q,            array_dim)
  )
  d1 <- seed_sampling(Seeds, kernel_params, params, controls)
  print(d1)
  ### Tests for variable kernels
  cov_mat <- matrix(c(0.15, -0.026, 0.17,
                      -0.026, 0.015, -0.025,
                      0.17, -0.025, 0.28),
                    3, 3)
  params <- list(gg_mu = 1.5,
                 gg_sigma = 0.75,
                 gg_Q = 1,
                 gg_cov = cov_mat,
                 frac_dispersing = 0.75,
                 fd_sdev = 0.1)
  controls <- list(pot_width = 7,
                   n_pots = 2,
                   n_reps = 3,
                   kernel_stoch_pots = TRUE)
  kernel_params <- kernel_stoch(params, controls)
  Seeds <- with(controls, matrix(1000, n_reps, n_pots))
  d2 <- seed_sampling(Seeds, kernel_params, params, controls)
  print(d2)
  
  controls$kernel_stoch_pots = FALSE
  kernel_params <- kernel_stoch(params, controls)
  d3 <- seed_sampling(Seeds, kernel_params, params, controls)
  print(d3)
}
