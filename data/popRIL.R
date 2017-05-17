### Creates the data objects popRIL and popRIL_cm, representing the RIL populations
###     experiments

# Get data from Jenn's cleaned up file
data_dir <- "~/Dropbox/Arabidopsis/analysis"
popRIL_cm <- read.csv(file.path(data_dir, '2016_03_04_seedlingsALL_corrected.csv'), 
                      header = TRUE)
# Clean up column names and get a useful order
popRIL_cm <- popRIL_cm[-1]
names(popRIL_cm) <- c("ID", "Treatment", "Replicate", "Gap", "Generation", "Pot", "Distance",
                      "Seedlings")
popRIL_cm <- popRIL_cm[!is.na(popRIL_cm$ID),]

ord <- with(popRIL_cm, order(Treatment, Gap, Replicate, Generation, Pot, Distance))
popRIL_cm <- popRIL_cm[ord,]

# Make some factor variables
popRIL_cm$Rep <- as.factor(popRIL_cm$Replicate)
popRIL_cm$Gen <- as.factor(popRIL_cm$Generation)

# Make a version that just has pot totals
popRIL <- plyr::ddply(popRIL_cm, 
                      plyr::.(ID, Gap, Replicate, Rep, Treatment, Generation, Gen, Pot),
                      summarize, 
                      Seedlings = sum(Seedlings))
ord <- with(popRIL, order(Treatment, Gap, Rep, Generation, Pot))
popRIL <- popRIL[ord,]

# Clean up the workspace
rm(data_dir, ord)

# Auto-cache the data
ProjectTemplate::cache("popRIL")
ProjectTemplate::cache("popRIL_cm")
