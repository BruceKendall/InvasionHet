### Creates the data objects popLer and popLer_cm, representing the Ler populations
###     experiments

# Raw data consists of one file per generation
# Final column is named inconsistently, so needs to be corrected before merge

data_dir <- "~/Dropbox/Arabidopsis/Data/Exp1"
data_fname <- list.files(data_dir, "seedposition")

popLer_cm <- NULL
for (i in 1:length(data_fname)) {
  tempdata <- xlsx::read.xlsx(file.path(data_dir, data_fname[i]), sheetName = "Data", 
                              header = TRUE)
  names(tempdata)[9] <- "seedlings"
  popLer_cm <- rbind(popLer_cm, tempdata)
  rm(tempdata)
}

# Clean up column names and get a useful order
popLer_cm <- popLer_cm[-1]
names(popLer_cm) <- c("ID", "Treatment", "Rep", "Gap", "Generation", "Pot", "Distance",
                      "Seedlings")
popLer_cm <- popLer_cm[!is.na(popLer_cm$ID),]

ord <- with(popLer_cm, order(Treatment, Gap, Rep, Generation, Pot, Distance))
popLer_cm <- popLer_cm[ord,]

# Make a version that just has pot totals
popLer <- plyr::ddply(popLer_cm, .(ID, Gap, Rep, Treatment, Generation, Pot), summarize,
                Seedlings = sum(Seedlings))
ord <- with(popLer, order(Treatment, Gap, Rep, Generation, Pot))
popLer <- popLer[ord,]

# Clean up the workspace
rm(data_dir, data_fname, i, ord)

# Auto-cache the data
ProjectTemplate::cache("popLer")
ProjectTemplate::cache("popLer_cm")
