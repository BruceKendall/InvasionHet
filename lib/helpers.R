# Calculate empirical dispersal statistics
# dispersal_data must be a data frame containing columns
#   ID, Density, Siliques, Seedlings, Distance
calc_dispersal_stats <- function(dispersal_data) {
  dispersal_stats <- group_by(dispersal_data, ID, Density, Siliques) %>%
    summarise(
      Total_seeds = sum(Seedlings),
      Home_seeds = Seedlings[1],
      Dispersing_seeds = Total_seeds - Home_seeds,
      Dispersal_fraction = Dispersing_seeds / Total_seeds,
      # Dispersal stats including all seeds
      Mean_dispersal_all = sum(Seedlings * (Distance - 4)) / Total_seeds,
      RMS_dispersal_all = sqrt(sum(Seedlings * (Distance - 4)^2) / Total_seeds),
      # Dispersal stats including just dispersing seeds
      Mean_dispersal = sum(Seedlings * (Distance - 4)) / Dispersing_seeds,
      RMS_dispersal = sqrt(sum(Seedlings * (Distance - 4)^2) / Dispersing_seeds)
    )
  return (dispersal_stats)
}

########################################################################################
# PDF and CDF for the left-truncated normal distribution
# low is the trunctation location, with a default appropriate to the edge of pot 1 in cm
dtnorm <- function(x, mean, sd, low = 3.5)
{
  PU <- 1
  PL <- pnorm(low, mean = mean, sd = sd)
  dnorm(x, mean, sd) / (PU-PL) * (x >= low) 
}
ptnorm <- function(q, mean, sd, low = 3.5)
{
  PU <- 1
  PL <- pnorm(low, mean = mean, sd = sd)
  (pnorm(q, mean, sd)-PL) / (PU-PL) * (q >= low)
}

########################################################################################
# PDF and CDF for the left-truncated lognormal distribution
# low is the trunctation location, with a default appropriate to the edge of pot 1 in cm
dtlnorm <- function(x, meanlog, sdlog, low = 3.5)
{
  PU <- 1
  PL <- plnorm(low, meanlog = meanlog, sdlog = sdlog)
  dlnorm(x, meanlog, sdlog) / (PU-PL) * (x >= low) 
}
ptlnorm <- function(q, meanlog, sdlog, low = 3.5)
{
  PU <- 1
  PL <- plnorm(low, meanlog = meanlog, sdlog = sdlog)
  (plnorm(q, meanlog, sdlog)-PL) / (PU-PL) * (q >= low)
}

########################################################################################
# Fit dispersal models (currently tnorm and tlnorm) to data
# dispersal_data must be a data frame containing columns
#   ID, Density, Siliques, Seedlings, Distance
# All data in dispersal_data are used, so if only a single rep is to be analyzed, it
#   should be subset outside this function
fit_dispersal_models <- function(dispersal_data, zero = 3.5, plot.it = TRUE) {
  cens_data_tble <- cens_dispersal_data(data_vec, zero)
  fit_tnorm <- fitdistcens(cens_data_tble, "tnorm", start = list(mean = 6, sd = 10))
  fit_tlnorm <- fitdistcens(cens_data_tble, "tlnorm", 
                            start = list(meanlog = log(6), sdlog = log(10)))
  if (plot.it) {
    cdfcompcens(list(fit_tnorm, fit_tlnorm), Turnbull.confint = TRUE, 
                main = dispersal_data$ID[1],
                legendtext = c("truncated normal", "truncated lognormal"))
  }
  stnorm <- summary(fit_tnorm)
  stlnorm <- summary(fit_tlnorm)
  data.frame(ID = dispersal_data$ID[1], AICnorm = stnorm$aic, AIClnorm = stlnorm$aic, 
             mu = stnorm$est[1], sd = stnorm$est[2],
             mulog = stlnorm$est[1], sdlog = stlnorm$est[2], 
             se_mu = stnorm$sd[1], se_sd = stnorm$sd[2],
             se_mulog = stlnorm$sd[1], se_sdlog = stlnorm$sd[2])
}

########################################################################################
# Convert the dispersal data into a form used by fitdistcens()
# dispersal_data must contain columns Distance and Seedlings
# zero is the zero point for the dispersal kernel. Default (3.5) puts the origin in the
#   middle of the maternal pot. Use zero = 0 to set the origin to the trailing edge of
#   the home pot, and zero = 7 to set the origin to the leading edge of the home pot.
# Returns a data frame giving the left and right values of the interval for each seed
#   in the dataset
cens_dispersal_data <- function(dispersal_data, zero = 3.5) {
  data_loc <- subset(dispersal_data, Distance > 4)
  data_vec <- rep(data_loc$Distance, data_loc$Seedlings)
  data.frame(left = data_vec - (zero + 1), right = data_vec - zero)
}

########################################################################################
# Estimate starting parameter values for use by fitdist or fitdistcens
# x is the data. If it is a vector it is left unchanged. If it is a two-colum matrix
#   then it is assumed to be set up for fitdistcens, and the midpoint of each interval
#   is calculated. There is no testing for Inf values (only interval censoring allowed)
# dist is a character string giving the root name of the distribution (e.g., "norm")
# ... Other arguments for specialized start functions. Possibilities include "truncated"
#   (T/F, default F), for indicating to start_gengamma() whether to used truncated forms.
# Returns a list with named start values, unless "dist" hasn't had a method defined, in
#   which case it returns NULL with a warning.
start_params <- function(x, dist, ...) {
  x_orig <- x
  if (dim(as.matrix(x))[2] == 2) {
    x <- apply(x, 1, mean) # set each value to the middle of its interval
  } else if (dim(as.matrix(x))[2] > 2) {
    stop("x must be a vector or two-column matrix")
  }
  

  start_pars <- switch(dist,
    invgauss = list(mean = mean(x),
                    shape = mean(x)^3 / var(x)),
    gengamma = start_gengamma(x_orig, ...),
    NULL
  )
  if (is.null(start_pars)) {
    warning("No method exists for setting start values for ", dist)
  }
  return(start_pars)
}

start_gengamma <- function(x, truncated = FALSE) {
  if (dim(as.matrix(x))[2] != 2) {
    stop("Only interval-censored methods have been developed in start_gengamma")
    # Dealing with this requires a switch between fitdistcens() and fitdist().
    # I may not need it.
  }
  
  # Set up the empty data frame for the base dist fits
  dist_list <- c("lnorm", "weibull", "gamma")
  n <- length(dist_list)
  base_fits <- data.frame(dist = dist_list, AIC = numeric(n), p1 = numeric(n), 
                          p2 = numeric(n), stringsAsFactors = FALSE)
  base_starts <- array(list(NULL), n)
  
  if (truncated) {
    stop("Truncated methods have not yet been developed in start_gengamma")
    # Need to add code to get start pars for base dists
  }
  
  # Fit the base distributions
  for (i in 1:n) {
    fit <- fitdistcens(x, base_fits$dist[i], base_starts[[i]])
    base_fits$AIC[i] <- fit$aic
    base_fits$p1[i] <- coef(fit)[1]
    base_fits$p2[i] <- coef(fit)[2]
  }
  
  # Get the AIC-best one and use it to set the base parameters
  best <- base_fits[which.min(base_fits$AIC), ]
  start_pars <- with(best, switch(dist,
    lnorm = list(mu = p1, sigma = p2, Q = 2),
    weibull = list(mu = log(p1), sigma = 1/p2, Q = 1),
    gamma = list(mu = log(p1/p2), sigma = sqrt(1/p1), Q = sqrt(1/p1))
  ))
  
  return(start_pars)
}
