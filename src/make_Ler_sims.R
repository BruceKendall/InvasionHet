# make_Ler_sims.R
ProjectTemplate::load.project()

nruns <- 1000
n_reps <- 10

n_init <- 50
sim_settings <- list(
  DS_seeds = TRUE,
  ES_seeds = TRUE,
  kernel_stoch = TRUE,
  kernel_stoch_pots = TRUE,
  seed_sampling = TRUE
)
expt_params <- list(
  n_reps = n_reps,  
  pot_width = 7,
  new_pots = 8
)

Ler_params$max_pots <- 20

sim_mean_var <- function(ES_seed_time = NULL) {
  Adults <- matrix(n_init, expt_params$n_reps, 1)
  for (i in 1:6) {
    Adults <- iterate_genotype(Adults, Ler_params, expt_params, sim_settings, ES_seed_time[i])
  }
  npot <- ncol(Adults)
  rep_sum <- apply(Adults[, npot:1, drop = FALSE], 1, cummax)
  if(dim(Adults)[2] == 1) { # Deal with the fact that apply() drops indicies
    rep_sum <- array(rep_sum, dim(Adults))
  } else {
    rep_sum <- t(rep_sum)[, npot:1]
  }
  maxd <- last_occupied_pot(Adults, zero = 0)
  #result <- c(mean(maxd), var(maxd))
  #names(result) <- c("Mean", "Variance")
  #result
  maxd
}

Ler_spread_stats <- data.frame(
  Gap = numeric(),
  DS = logical(),
  ES = logical(),
  KS = logical(),
  SS = logical(),
  run = numeric()
)
for (DS in c(TRUE, FALSE)) {
  sim_settings$DS_seeds <- DS
  for (ES in c(TRUE, FALSE)) {
    sim_settings$ES_seeds <- ES
    for (KS in c(TRUE, FALSE)) {
      sim_settings$kernel_stoch <- KS
      for (SS in c(TRUE, FALSE)) {
        sim_settings$seed_sampling <- SS
        for (i in 1:nruns) {
          ES_seed_time <- rnorm(6, 0, Ler_params$sigma_seed_time)
          for(gap_size in 0:3) {
            print(c(gap_size, DS, ES, KS, SS, i))
            sim_settings$gap_size <- gap_size
            rep_spread_stats <- sim_mean_var(ES_seed_time)
            #          rep_spread_stats <- t(replicate(nruns, sim_mean_var(), 
            #                                          simplify = TRUE))
            # print(rep_spread_stats)
            Ler_spread_stats <- rbind(Ler_spread_stats,
                                      data.frame(Gap = gap_size,
                                                 DS = DS,
                                                 ES = ES,
                                                 KS = KS,
                                                 SS = SS,
                                                 run = i,
                                                 #   Mean = rep_spread_stats[, 1],
                                                 #   Variance = rep_spread_stats[, 2]
                                                 Max_Dist = rep_spread_stats
                                      ))
          }
        }
      }
    }
  }
}

Ler_sim_maxd_1000 <- Ler_spread_stats
ProjectTemplate::cache("Ler_sim_maxd_1000")
