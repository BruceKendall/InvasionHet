## Calculate Ler spread rates

# First we look at furthest distance reached.
LerC_spread <- filter(popLer, Treatment == "C") %>%
  group_by(Gap, Rep, Generation, Gen) %>%
  summarise(Furthest = max(Pot))

# Now calculate per-generation spread rates
# The "default" argument to lag() replaces the leading NA with the specified value
LerC_spread <- group_by(LerC_spread, Gap, Rep) %>%
  mutate(speed =  Furthest - lag(Furthest, default = 0),
         speed_m1 = lag(speed))

#Hmm, there is one case (rep 8, 3p gap) where the forward pot seems to go extinct, resulting in a negative speed. Need to check the original data, but for now let's just set that to zero.

LerC_spread <- within(LerC_spread, speed[speed < 0] <- 0)
LerC_spread <- within(LerC_spread, speed_m1[speed_m1 < 0] <- 0)
