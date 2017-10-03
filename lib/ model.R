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
#'  **params**
#'  
#' `n_gene` (integer)
#' :    Number of genotypes
#' 
#' `a_Gompertz` (scalar, or vector of length `n_gene`)
#' :    Intercept of the Gompertz model of density dependence. 
#' Should be positive (or else the population will not grow from a single individual)
#' 
#' `b_Gompertz` (scalar, or vector of length `n_gene`)
#' :    Slope of the Gompertz model of density dependence. Should be between -1 and zero
#' 
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
