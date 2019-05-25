### Creates the data object disperseRIL, representing the RIL dispersal experiment

# Get data from Jenn's file
data_dir <- "~/Dropbox/Arabidopsis/analysis"
disperseRIL <- read.csv(file.path(data_dir, '2015_06_30_RilsDispersal.csv'), 
                        header = TRUE)

# Make some factor variables
disperseRIL$ID <- as.factor(disperseRIL$ID)
disperseRIL$RILs <- as.factor(disperseRIL$RILs)

# Drop the columns with the (irrelevant) info about Ecotype and Replicate
disperseRIL <- disperseRIL[, -c(2, 4)]

# Clean up column names 
names(disperseRIL) <- c("ID", "RIL", "Pot", "Distance", "Seedlings", "Siliques", "Height")


# Clean up the workspace
rm(data_dir)

# Auto-cache the data
ProjectTemplate::cache("disperseRIL")
