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
  
  with(params, plot(dgengamma(0:30, gg_mu, gg_sigma, gg_Q)))
  
  array_dim <- c(nrep, npot)
  kernel_params <- list(
    frac_dispersing = array(params$frac_dispersing, array_dim),
    gg_mu           = array(params$gg_mu,           array_dim),
    gg_sigma        = array(params$gg_sigma,        array_dim),
    gg_Q            = array(params$gg_Q,            array_dim)
  )
  
  print(det_kernel(Seeds, kernel_params, params, controls))
    
}
