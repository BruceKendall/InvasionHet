Ler_maxd_viz <- function(gap, Ler_summ = NULL, 
                         model_stat = "Variance", Lss = NULL) {
  if (is.null(Ler_summ)) {
    Ler_summ <- Ler_maxd_summary(Lss) 
  }
  
  real_stats <- filter(LerC_spread, Generation==6) %>% group_by(Gap) %>%
    summarize(Mean = mean(Furthest), Var = var(Furthest))
  
  if (model_stat == "Mean") {
    data_val <- pull(filter(real_stats, 
                            Gap == paste(gap,"p", sep="")), 
                     Mean)
    model_max <- max(pull(filter(Ler_summ, Gap == gap), Mean))
    ggplot(filter(Ler_summ, Gap == gap), 
           aes(y = Mean, x = KS, fill = SS)) +
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
      facet_grid(DS2 ~ ES2) +
      ggtitle(paste("Mean, ", gap, "-pot gaps", sep="")) +
      geom_hline(yintercept = data_val)# +
#      ylim(c(0, min(model_max, 3*data_val)))
  } else if (model_stat == "Variance") {
    data_val <- pull(filter(real_stats, 
                            Gap == paste(gap,"p", sep="")), 
                     Var)
    model_max <- max(pull(filter(Ler_summ, Gap == gap), Variance))
    ggplot(filter(Ler_summ, Gap == gap), 
           aes(y = Variance, x = KS, fill = SS)) +
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
      facet_grid(DS2 ~ ES2) +
      ggtitle(paste("Variance, ", gap, "-pot gaps", sep="")) +
      geom_hline(yintercept = data_val) +
      ylim(c(0, min(model_max, 3*data_val)))
  }
}

Ler_maxd_summary <- function(Lss = Ler_spread_stats) {
  filter(Lss, Max_Dist < 60) %>% 
    group_by(Gap, DS, ES, KS, SS, run) %>%
    summarize(Mean = mean(Max_Dist), Variance = var(Max_Dist)) %>%
    mutate(DS2 = paste("DS =", DS),
           ES2 = paste("ES =", ES))
}
