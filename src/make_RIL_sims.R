# make_RIL_sims.R
ProjectTemplate::load.project()

nruns <- 2
n_reps <- 10

n_init <- 3
controls <- list(
  n_reps = n_reps,
  DS_seeds = TRUE,
  ES_seeds = TRUE,
  kernel_stoch = TRUE,
  kernel_stoch_pots = TRUE,
  seed_sampling = TRUE,
  pot_width = 7
)
nRIL <- length(levels(disperseRIL$RIL))
#for (i in 1:14) RIL_params[[i]]$a_Gompertz <- RIL_params[[i]]$a_Gompertz -1
for (j in 1:nRIL) RIL_params[[j]]$gap_size <- 0
library(abind)
sim_RIL_evol <- function() {
  Adults <- array(n_init, c(controls$n_reps, 1, nRIL))
  dist_max <- 1
  for (i in 1:6) {
    tot_Adults <- as.matrix(apply(Adults, c(1, 2), sum))
    Adult_next <- array(0, dim(Adults))
    for (j in 1:nRIL) {
      Adultj <- iterate_genotype(as.matrix(Adults[, , j]), RIL_params[[j]], controls, tot_Adults)
      distj <- ncol(Adultj)
      if (distj > dim(Adult_next)[2]) { # Stretch the array
        pad_dim <- dim(Adult_next) - c(0, dim(Adult_next)[2] - distj, 0)
        pad <- array(0, pad_dim)
        Adult_next <- abind(Adult_next, pad, along = 2)
      }
      Adult_next[, 1:distj, j] <- Adultj
    }
    Adults <- Adult_next
  }
  Adults <- as.matrix(apply(Adults, 1, sum))
  npot <- ncol(Adults)
  rep_sum <- apply(Adults[, npot:1, drop = FALSE], 1, cummax)
  if(dim(Adults)[2] == 1) { # Deal with the fact that apply() drops indicies
    rep_sum <- array(rep_sum, dim(Adults))
  } else {
    rep_sum <- t(rep_sum)[, npot:1]
  }
  maxd <- apply(rep_sum, 1, function(x) max((1:length(x))[x > 0]))
  maxd <- maxd[is.finite(maxd)]
  maxd
}

sim_RIL_evol()

# RIL_evol_spread_stats <- data.frame(
#   Gap = numeric(),
#   DS = logical(),
#   ES = logical(),
#   KS = logical(),
#   SS = logical(),
#   Mean = numeric(),
#   Variance = numeric()
# )
# for(gap_size in 0:3) {
#   Ler_params$gap_size <- gap_size
#   for (DS in c(TRUE, FALSE)) {
#     controls$DS_seeds <- DS
#     for (ES in c(TRUE, FALSE)) {
#       controls$ES_seeds <- ES
#       for (KS in c(TRUE, FALSE)) {
#         controls$kernel_stoch <- KS
#         for (SS in c(TRUE, FALSE)) {
#           controls$seed_sampling <- SS
#           print(c(gap_size, DS, ES, KS, SS))
#           for (i in 1:nruns) {
#             rep_spread_stats <- sim_mean_var()
# #          rep_spread_stats <- t(replicate(nruns, sim_mean_var(), 
# #                                          simplify = TRUE))
#          # print(rep_spread_stats)
#             Ler_spread_stats <- rbind(Ler_spread_stats,
#                                     data.frame(Gap = gap_size,
#                                                DS = DS,
#                                                ES = ES,
#                                                KS = KS,
#                                                SS = SS,
#                                             #   Mean = rep_spread_stats[, 1],
#                                             #   Variance = rep_spread_stats[, 2]
#                                                Max_Dist = rep_spread_stats
#                                     ))
#           }
#         }
#       }
#     }
#   }
# }

# ProjectTemplate::cache("Ler_spread_stats")
