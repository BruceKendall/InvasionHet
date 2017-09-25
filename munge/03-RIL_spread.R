## Calculate RIL spread rates

# First we look at furthest distance reached.
RIL_spread <- group_by(popRIL, Treatment, Gap, Rep, Generation, Gen) %>%
  summarise(Furthest = max(Pot))

# Now calculate per-generation spread rates
# The "default" argument to lag() replaces the leading NA with the specified value
RIL_spread <- group_by(RIL_spread, Treatment, Gap, Rep) %>%
  mutate(speed =  Furthest - lag(Furthest, default = 0),
         speed_m1 = lag(speed))

ProjectTemplate::cache("RIL_spread")
