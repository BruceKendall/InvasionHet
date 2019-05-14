# Try out the various combinations of control variables with ler-like parameters
ProjectTemplate::load.project()

# All stoch -- finds errors relating to array dims and zero arguments
test_iterate_genotype(10,nreps = 40, ES_seeds = TRUE,DS_seeds = TRUE, kernel_stoch = TRUE, 
                      seed_sampling = TRUE,kernel_stoch_pots = TRUE, gap_size = 0) # OK

test_iterate_genotype(6,nreps = 10, ES_seeds = TRUE,DS_seeds = TRUE, kernel_stoch = TRUE, 
                      seed_sampling = TRUE,kernel_stoch_pots = TRUE, gap_size = 1) # OK

test_iterate_genotype(6,nreps = 10, ES_seeds = TRUE,DS_seeds = TRUE, kernel_stoch = TRUE, 
                      seed_sampling = TRUE,kernel_stoch_pots = TRUE, gap_size = 2) # OK

test_iterate_genotype(6,nreps = 10, ES_seeds = TRUE,DS_seeds = TRUE, kernel_stoch = TRUE, 
                      seed_sampling = TRUE,kernel_stoch_pots = TRUE, gap_size = 3) # OK

# All deterministic -- reps should go in parallel
test_iterate_genotype(4, ES_seeds = FALSE,DS_seeds = FALSE, kernel_stoch = FALSE, 
                      seed_sampling = FALSE,kernel_stoch_pots = FALSE, gap_size = 0) # OK

test_iterate_genotype(4, ES_seeds = FALSE,DS_seeds = FALSE, kernel_stoch = FALSE, 
                      seed_sampling = FALSE,kernel_stoch_pots = FALSE, gap_size = 1) # OK

test_iterate_genotype(4, ES_seeds = FALSE,DS_seeds = FALSE, kernel_stoch = FALSE, 
                      seed_sampling = FALSE,kernel_stoch_pots = FALSE, gap_size = 2) # OK

# ES_stoch: should see more variation among i (inter-generation stochasticity)
# than among reps within i (rep stochasitity).
# Reps with same total seeds should have identical dispersal
for (i in 1:5) {
  test_iterate_genotype(1, nreps = 5, ES_seeds = TRUE,DS_seeds = FALSE, kernel_stoch = FALSE, 
                      seed_sampling = FALSE,kernel_stoch_pots = FALSE, gap_size = 0) # OK
}
