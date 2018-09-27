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
#' :    Number of genotypes
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
#' ### Elements of `controls`
#' 
#' `n_pots` (integer; calculated by model)
#' :    Number of pots in the array
#' 
#' `n_sims` (integer)
#' :    Number of replicate simulations
#' 
#' `ES_seeds` (logical)
#' :    Set to `TRUE` to include environmental stochasticity in seed production
#' 
#' `DS_seeds` (logical)
#' :    Set to `TRUE` to include demographic stochasticity in seed production. If `FALSE`,
#' then seed number is simply rounded to the nearest integer.
#' 
#' ### State variables
#' The state variables are `Adults` and (internally) `Seeds`. These are structured as
#' a matrix with each row being a replicate and each column a pot. 
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
    Seeds <- ES_seeds(Seeds, params, controls$n_sims)
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
  if (controls$seed_sampling) {
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
