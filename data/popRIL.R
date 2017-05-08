### Creates the data objects popRIL and popRIL_cm, representing the RIL populations
###     experiments

# Raw data consists of one file per generation
# Final column is named inconsistently, so needs to be corrected before merge

data_dir <- "~/Dropbox/Arabidopsis/Data/Exp2"
data_fname <- list.files(data_dir, "EVONO&YES.xls")

popRIL_cm <- NULL
for (i in 1:length(data_fname)) {
  tempdata <- xlsx::read.xlsx(file.path(data_dir, data_fname[i]), sheetIndex = 1, 
                              colIndex = 1:9, header = TRUE)
  names(tempdata)[9] <- "seedlings"
  names(tempdata)[4] <- "Replicate"
  popRIL_cm <- rbind(popRIL_cm, tempdata)
  rm(tempdata)
}

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
rm(data_dir, data_fname, i, ord)

# Auto-cache the data
ProjectTemplate::cache("popRIL")
ProjectTemplate::cache("popRIL_cm")
