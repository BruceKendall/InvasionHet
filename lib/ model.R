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
#' # Elements of `params``
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
#' # Elements of `params``
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
