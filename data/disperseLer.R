### Creates the data object disperseLer, representing the Ler dispersal experiment

# Get data from Jenn's file
data_dir <- "~/Dropbox/Arabidopsis/analysis"
disperseLer <- read.csv(file.path(data_dir, '2013_08_08_Exp1_Spray.csv'), 
                        header = TRUE)

# Drop the "clipped" treatment
disperseLer <- droplevels(subset(disperseLer, new_trt != "clipped", drop = TRUE))

# Drop the columns with the (irrelevant) info about where the mom pots came from
disperseLer <- disperseLer[, -c(1, 3:6)]

# Clean up column names 
names(disperseLer) <- c("ID", "Pot", "Distance", "Seedlings", "Siliques", "Density",
                        "Treatment")

# Make some factor variables
disperseLer$ID <- as.factor(disperseLer$ID)

# Clean up the workspace
rm(data_dir)

# Auto-cache the data
ProjectTemplate::cache("disperseLer")
