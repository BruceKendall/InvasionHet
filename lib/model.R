#' ---
#' title: model.R
#' author: Bruce Kendall
#' ---
#' 
#' Contains functions for simulating the *Arabidopsis* model.
#' 
#' The functions all use elements of lists called `params` and `controls`. 
#' The elements of these lists are:
#' 
#' ### Elements of `params`
#'  
#' `n_types` (integer)
#' :    Number of genotypes
#' 
#' `a_Gompertz` (scalar, or vector of length `n_types`)
#' :    Intercept of the Gompertz model of density dependence. 
#' Should be positive (or else the population will not grow from a single individual)
#' 
#' `b_Gompertz` (scalar, or vector of length `n_types`)
#' :    Slope of the Gompertz model of density dependence. Should be between -1 and zero
#' 
#' 
#' ### Elements of `controls`
#' 
#' `n_pots` (integer; calculated by model)
#' :    Number of pots in the array
#' 
#' `n_sims` (integer)
#' :    Number of replicate simulations
#' 
#' 
#' <!-- ############################################################################# --> 
#' # `iterate_model()`
#' Iterates the model one time step
iterate_model <- function(Adults, params, controls) {
  #### SEED PRODUCTION ####
  # Density dependence in seed production
  Seeds <- Gompertz_seeds(Adults, params)
  
  # Environmental stochasticicy in seed production?
  if (controls$ES_seeds) {
    Seeds <- ES_seeds(Seeds, params)
  }
  
  # Demographic stochasticity in seed production?
  if (controls$DS_seeds) {
    Seeds <- DS_seeds(Seeds, params)
  } else {
    Seeds <- round(Seeds)
  }
  
  #### DISPERSAL ####
  # Kernel stochasticity?
  if (cotrols$kernel_stoch) {
    kernel_params <- kernal_stoch(params, controls)
  } else { # Distribute the genotype-specific parameters across pots and reps
    array_dim <- c(params$n_types,
                   controls$n_pots,
                   controls$n_sims)
    kernel_params <- list(
      frac_dispersing = array(params$frac_dispersing, array_dim),
      mulog           = array(params$mulog,           array_dim),
      sdlog           = array(params$sdlog,           array_dim)
    )
  }
  
  # Seed sampling?
  if (contrls$seed_sampling) {
    Seeds <- seed_sampling(Seeds, kernel_params, params, controls)
  } else {
    Seeds <- round(det_kernel(Seeds, kernel_params, params, controls))
  }
  
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
