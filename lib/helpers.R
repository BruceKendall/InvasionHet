calc_dispersal_stats <- function(dispersal_data) {
  dispersal_stats <- group_by(dispersal_data, ID, Density, Siliques) %>%
    summarise(
      Total_seeds = sum(Seedlings),
      Home_seeds = Seedlings[1],
      Dispersing_seeds = Total_seeds - Home_seeds,
      Dispersal_fraction = Dispersing_seeds / Total_seeds,
      # Dispersal stats including all seeds
      Mean_dispersal_all = sum(Seedlings * (Distance - 4)) / Total_seeds,
      RMS_dispersal_all = sqrt(sum(Seedlings * (Distance - 4)^2) / Total_seeds),
      # Dispersal stats including just dispersing seeds
      Mean_dispersal = sum(Seedlings * (Distance - 4)) / Dispersing_seeds,
      RMS_dispersal = sqrt(sum(Seedlings * (Distance - 4)^2) / Dispersing_seeds)
    )
  return (dispersal_stats)
}
