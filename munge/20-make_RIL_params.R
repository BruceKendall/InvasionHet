## make_RIL_params.R
## Construct the parameters from the RIL experiments

## We will have a list of parameter lists, one for each RIL
RIL_list <- levels(disperseRIL$RIL)
nRIL <- length(RIL_list)
RIL_params <- vector("list", nRIL)  # Empty list
names(RIL_params) <- RIL_list       # with names

## Loop over RILs (NOT going to try to do some massive lapply...)
for (i in 1:nRIL) {
  disperseRILi <- subset(disperseRIL, RIL == RIL_list[i])
  
  ## Fraction dispersing
  nondispersers <- subset(disperseRILi, Pot == 0, c("ID", "Seedlings"))
  dispersers <- filter(disperseRILi, Pot >= 1) %>% group_by(ID) %>% summarise(dispersers = sum(Seedlings))
  disperse_num <- merge(nondispersers, dispersers)
  names(disperse_num)[2] <- "nondispersers"
  disperse_num$dispersers <- round(2 * disperse_num$dispersers)
  
  library(VGAM)
  bbfit <- vglm(cbind(dispersers, nondispersers) ~ 1, betabinomial, data = disperse_num)
  frac_dispersing <- Coef(bbfit)[1]
  fd_stdev <- sqrt(frac_dispersing * (1 - frac_dispersing) * Coef(bbfit)[2])
  
  
  ## Dispersal kernels
  n_min <- 10 # Set the minimum number of dispersing seeds
  dispersing_seeds <- group_by(disperseRILi, ID) %>% 
    filter(Distance > 4) %>% 
    filter(!(ID %in% c("19", "40", "77"))) %>%
    summarize(tot_seeds=sum(Seedlings))
  good_reps <- filter(dispersing_seeds, tot_seeds >= n_min) %>%
    pull(ID)
  disperseRILi2 <- filter(disperseRILi, ID %in% good_reps)
  fiteach <- fiteach_disp_unt(disperseRILi2, model = "gengamma", control = list(maxit = 1000))
  gg_mean <- apply(fiteach[, 4:6], 2, mean)
  gg_cov <- cov(fiteach[, 4:6])
  
  ## Density dependent seed production comes from RIL.stats and Ler variances
  
  
  # Extract parameters
  RIL_params[[i]] <- list(
    a_Gompertz = RIL.stats$a_seed[i],
    b_Gompertz = - RIL.stats$b_seed[i],
    sigma_seed_time = Ler_params$sigma_seed_time,
    sigma_seed_rep = Ler_params$sigma_seed_rep,
    theta_seed = Ler_params$theta_seed,
    frac_dispersing = frac_dispersing,
    fd_sdev = fd_stdev,
    gg_mu = gg_mean[1],
    gg_sigma = gg_mean[2],
    gg_Q = gg_mean[3],
    gg_cov = gg_cov
  )
}

## Unroll stuff so we can calculate means
mat_pars <- matrix(unlist(RIL_params), nRIL, 19, byrow = TRUE)
par_means <- apply(mat_pars, 2, mean, na.rm = TRUE)

## Fix covariance for RIL 133
RIL_params$`133`$gg_cov <- matrix(par_means[11:19], 3, 3)

ProjectTemplate::cache("RIL_params")

## Make "mean" RIL
RIL_bar_params <- list(
  a_Gompertz = par_means[1],
  b_Gompertz = par_means[2],
  sigma_seed_time = Ler_params$sigma_seed_time,
  sigma_seed_rep = Ler_params$sigma_seed_rep,
  theta_seed = Ler_params$theta_seed,
  frac_dispersing = par_means[6],
  fd_sdev = par_means[7],
  gg_mu = par_means[8],
  gg_sigma = par_means[9],
  gg_Q = par_means[10],
  gg_cov = matrix(par_means[11:19], 3, 3)
)

ProjectTemplate::cache("RIL_bar_params")
