#' ---
#' title: Ler_fecundity.R
#' author: Bruce Kendall
#' ---
#' 
#' Code that generates data for evaluating density dependence and stochasticity
#' in the Ler population data. Uses `popLer` for input and creates `Ler_fecundity`. 
#' The latter is augmented with variables `Nm1`, which has the number of seedlings
#' in the pot the previous generation, and `GenID`, which is an interaction factor
#' with a unique value for each combindation of `Gen` and `ID`.
#' 
#' The data are restricted to treatments B and C, and gaps 1p and 2p. Only GenID
#' combinations with multiple pots in the preceding generation are kept

# Select the treatments
Ler_fecundity <- subset(popLer, Treatment %in% c("B", "C"))

# Create the lagged variable. For treatment B it is always 1
Ler_fecundity <- group_by(Ler_fecundity, ID, Pot) %>%
  mutate(Nm1 = 1 + (Treatment == "C") * (lag(Seedlings) - 1))

# Select the desired records
Ler_fecundity <- subset(Ler_fecundity, 
                          Generation > 1 & 
                          Gap %in% c("1p", "2p") &
                          !is.na(Nm1)
                        )

# Make the interaction variable
Ler_fecundity$ID <- as.factor(Ler_fecundity$ID) 
Ler_fecundity$GenID <- with(Ler_fecundity, interaction(Gen, ID))

# Drop cases where there is only one pot in a GenID level
GenID_counts <- table(Ler_fecundity$GenID)
singletons <- rownames(GenID_counts)[GenID_counts == 1]
Ler_fecundity <- droplevels(Ler_fecundity[-match(singletons, Ler_fecundity$GenID), ])

# Clean up and auto-cache the result
rm(singletons, GenID_counts)
ProjectTemplate::cache("Ler_fecundity")
