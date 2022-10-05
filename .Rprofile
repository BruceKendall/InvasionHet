# Load global .Rprofile
if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# Knitr options. Note that these mean:
#   - All output will be cached. This is desirable in case code/data changes in the 
#     future; but it means that all chunks should be labeled for reliable behavior. 
#     DEPENDENCIES ACROSS CODE CHUNKS MAY BE AN ISSUE.
#   - R code will run in the project root directory
project_root <- normalizePath(getwd())
knitr::opts_chunk$set(comment = '', cache = TRUE)
knitr::opts_knit$set(root.dir = project_root)

# Blogdown options.
#   Prevent the new system where all posts are named index.Rmd
#   Prevent automatic knitting on save (which takes nontrivial time because of the 
#     ProjectTemplate overhead)
options(blogdown.new_bundle = FALSE)
options(blogdown.knit.on_save = FALSE)

# Clean up
rm(project_root)
