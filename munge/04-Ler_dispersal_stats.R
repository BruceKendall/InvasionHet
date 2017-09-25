# 04-Ler_dispersal_stats.R
# Empirical and model-based dispersal statistics from Ler sticky paper experiments

# Estimate the empirical stats
Ler_dispersal_stats <- calc_dispersal_stats(disperseLer)

# Fit the kernels and add the estimates to the data frame
all_Ler_fits <- ddply(disperseLer, "ID", fit_dispersal_models, plot.it = FALSE) 
Ler_dispersal_stats <- merge(Ler_dispersal_stats, all_Ler_fits, by = "ID")

# Get the measurement error variance for Dispersal_fraction
Ler_dispersal_stats <- mutate(Ler_dispersal_stats,
                              binom_var = Dispersal_fraction * (1 - Dispersal_fraction) /
                                Total_seeds)

# Clean up and auto-cache the result
rm(all_Ler_fits)
ProjectTemplate::cache("Ler_dispersal_stats")
