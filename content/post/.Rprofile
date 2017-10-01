# Load global .Rprofile
if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# Knitr options. Note that these mean:
#   - All output will be cached. This is desireable in case code/data changes in the 
#     future; but it means that all chunks should be labeled for reliable behavior. 
#     DEPENDENCIES ACROSS CODE CHUNKS MAY BE AN ISSUE.
#   - R code will run in the project root directory
project_root <- normalizePath("../..")
knitr::opts_chunk$set(comment = '', cache = TRUE)
knitr::opts_knit$set(root.dir = project_root)

# Clean up
rm(project_root)
