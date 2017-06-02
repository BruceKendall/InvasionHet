### Creates the data objects popLer_sil, Containing silique counts from the parent
###   generation in Ler populations (treatments A and B)

# Raw data consists of one file per generation
# Columns are named inconsistently, so needs to be corrected before merge

data_dir <- "~/Dropbox/Arabidopsis/analysis"
data_fname <- list.files(data_dir, "siliques.csv")

popLer_sil <- NULL
for (i in 1:length(data_fname)) {
  tempdata <- read.csv(file.path(data_dir, data_fname[i]))
  names(tempdata) <- c("Generation", "ID", "Treatment", "Gap", "Pot", "Siliques")
  popLer_sil <- rbind(popLer_sil, tempdata)
  rm(tempdata)
}

# Fix an error in gen 1
popLer_sil[popLer_sil$ID == 48 & popLer_sil$Siliques == 187, ]$Gap <- "1p"
popLer_sil[popLer_sil$ID == 48 & popLer_sil$Siliques == 187, ]$ID <- 49

# Fill in the gap sizes in generation zero
Gen1Pot0 <- subset(popLer_sil, Generation == 1 & Pot == 0)
popLer_sil[popLer_sil$Generation == 0, 4] <- 
  Gen1Pot0$Gap[match(subset(popLer_sil, Generation == 0)$ID, Gen1Pot0$ID)]

# Get a useful order
ord <- with(popLer_sil, order(Treatment, Gap, ID, Generation, Pot))
popLer_sil <- popLer_sil[ord,]

# Make some factor variables
popLer_sil$Gen <- as.factor(popLer_sil$Generation)
popLer_sil$ID <- as.factor(popLer_sil$ID)


# Clean up the workspace
rm(data_dir, data_fname, i, ord, Gen1Pot0)

# Auto-cache the data
ProjectTemplate::cache("popLer_sil")
