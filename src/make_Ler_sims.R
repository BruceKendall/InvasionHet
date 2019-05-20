# make_Ler_sims.R
ProjectTemplate::load.project()

nruns <- 20
n_reps <- 10

n_init <- 50
controls <- list(
  n_reps = n_reps,
  DS_seeds = TRUE,
  ES_seeds = TRUE,
  kernel_stoch = TRUE,
  kernel_stoch_pots = TRUE,
  seed_sampling = TRUE,
  pot_width = 7
)
sim_mean_var <- function() {
  Adults <- matrix(n_init, controls$n_reps, 1)
  for (i in 1:6) {
    Adults <- iterate_genotype(Adults, Ler_params, controls)
  }
  npot <- ncol(Adults)
  rep_sum <- t(apply(Adults[, npot:1], 1, cummax))[, npot:1]
  maxd <- apply(rep_sum, 1, function(x) max((1:length(x))[x > 0]))
  maxd <- maxd[is.finite(maxd)]
  result <- c(mean(maxd), var(maxd))
  names(result) <- c("Mean", "Variance")
  result
}

Ler_spread_stats <- data.frame(
  Gap = numeric(),
  DS = logical(),
  ES = logical(),
  KS = logical(),
  SS = logical(),
  Mean = numeric(),
  Variance = numeric()
)
for(gap_size in 0:3) {
  Ler_params$gap_size <- gap_size
  for (DS in c(TRUE, FALSE)) {
    controls$DS_seeds <- DS
    for (ES in c(TRUE, FALSE)) {
      controls$ES_seeds <- ES
      for (KS in c(TRUE, FALSE)) {
        controls$kernel_stoch <- KS
        for (SS in c(TRUE, FALSE)) {
          controls$seed_sampling <- SS
          print(c(gap_size, DS, ES, KS, SS))
          rep_spread_stats <- t(replicate(nruns, sim_mean_var(), 
                                          simplify = TRUE))
         # print(rep_spread_stats)
          Ler_spread_stats <- rbind(Ler_spread_stats,
                                    data.frame(Gap = gap_size,
                                               DS = DS,
                                               ES = ES,
                                               KS = KS,
                                               SS = SS,
                                               Mean = rep_spread_stats[, 1],
                                               Variance = rep_spread_stats[, 2]
                                    ))
        }
      }
    }
  }
}

ProjectTemplate::cache("Ler_spread_stats")
