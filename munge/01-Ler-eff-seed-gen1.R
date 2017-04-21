# Calculate the "effective seed number" from the first generation of the Ler experiment

# extract those data for the relevant treatments
Ler1BC <- subset(popLer, Gap=="0p" & Generation == 1 & Treatment != "A")
Ler1BC$Density <- 1
Ler1BC$Density[Ler1BC$Treatment == "C"] <- 50

# We calculate the "effective seed number" as the number falling in the mother pot plus
# twice the number falling in the other pots (the latter gets at bidirectional dispersal,
# and estimates the number of seeds that would fall somewhere on the runway if the home
# pot was in the middle of the runway):
Ler_eff_seed_gen1 <- group_by(Ler1BC, ID, Density) %>% 
  summarise(eff_sd_no = sum(Seedlings) + sum(Seedlings * Pot > 0))

# Clean up
rm("Ler1BC")
