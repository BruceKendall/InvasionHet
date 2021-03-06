---
title: "Invasion Heterogeneity Project Notebook"
author: "Bruce E. Kendall"
date: "Last updated on 2017-10-03"
knit: "bookdown::render_book"
site: "bookdown::bookdown_site"
output:
  bookdown::gitbook:
    toc-depth: 1
documentclass: book
---



# Preface {-}
This is the project notebook for the Invasion Heterogeneity project, which uses Jennifer Williams' *Arabidopsis* experiments to motivate explorations of the effects of heterogeneity,  stochasticity, and evolution on the variation in spread rate.

## Usage {-}
This notebook is prepared in *bookdown*. I ran into a number of poorly documented issues.

- To use the build tab, you need to go into the Rproj file and set build type to "Website." In addition, you need to make sure that the lines `knit: "bookdown::render_book"`,  `site: "bookdown::bookdown_site"`, and `output:  bookdown::gitbook` are in the yaml header (either in index.Rmd or, presumably, _bookdown.yml)
- I'm trying to suppress the chapter/section numbers, and only show top-level stuff in the toc. But that doesn't seem to be working.

Individual content files should be named with the date, in format yyyy-mm-dd.Rmd. The first line should also include the date, in the format, e.g., `# (PART) 7 April 2017 {-}`. This creates a "part" with the date, and all subjects within the day's work can be top-level headers.
