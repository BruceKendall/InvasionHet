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
fit_dispersal_models <- function(dispersal_data, plot.it = TRUE) {
  data_loc <- subset(dispersal_data, Distance > 4)
  data_vec <- rep(data_loc$Distance, data_loc$Seedlings)
  cens_data_tble <- data.frame(left = data_vec - 4.5, right = data_vec - 3.5)
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
             mulog = stlnorm$est[1], sdlog = stlnorm$est[2])
}
