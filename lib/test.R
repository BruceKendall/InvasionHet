### Functions to test components of model.R
### Prior to invoking these functions, run ProjectTemplate::load.project()

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
}
