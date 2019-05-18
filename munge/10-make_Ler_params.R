## make_Ler_params.R
## Construct the parameters from the Ler experiments


## Fraction dispersing
nondispersers <- subset(disperseLer, Pot == 0, c("ID", "Seedlings"))
dispersers <- filter(disperseLer, Pot == 1) %>% group_by(ID) %>% summarise(dispersers = sum(Seedlings))
disperse_num <- merge(nondispersers, dispersers)
names(disperse_num)[2] <- "nondispersers"
disperse_num$dispersers <- round(2 * disperse_num$dispersers)

library(VGAM)
bbfit <- vglm(cbind(dispersers, nondispersers) ~ 1, betabinomial, data = disperse_num)
frac_dispersing <- Coef(bbfit)[1]
fd_stdev <- sqrt(frac_dispersing * (1 - frac_dispersing) * Coef(bbfit)[2])


## Dispersal kernels
disperseLer2 <- filter(disperseLer, ID != "79_0", ID != "90_1", ID != "131_0")
fiteach <- fiteach_disp_unt(disperseLer2, model = "gengamma")
gg_mean <- apply(fiteach[, 4:6], 2, mean)
gg_cov <- cov(fiteach[, 4:6])

## Density dependent seed production
# Ler_fecundity is the data; inflate the home seedlings by the average fraction dispersing
Ler_fecundity$Seedlings <- round(frac_dispersing * Ler_fecundity$Seedlings)

library(lme4)
seeds_DD <- glmer(Seedlings ~ log(Nm1) + (1 | Gen) + (1 | GenID), 
                  data = Ler_fecundity, family = poisson)

# Extract parameters
Ler_params <- list(
  a_Gompertz = fixef(seeds_DD)[1],
  b_Gompertz = fixef(seeds_DD)[2] - 1,
  sigma_seed_time = as.data.frame(VarCorr(seeds_DD))$sdcor[2],
  sigma_seed_rep = as.data.frame(VarCorr(seeds_DD))$sdcor[1],
  theta_seed = sum(residuals(seeds_DD, type="pearson")^2)/df.residual(seeds_DD),
  frac_dispersing = frac_dispersing,
  fd_sdev = fd_stdev,
  gg_mu = gg_mean[1],
  gg_sigma = gg_mean[2],
  gg_Q = gg_mean[3],
  gg_cov = gg_cov
)

ProjectTemplate::cache("Ler_params")
