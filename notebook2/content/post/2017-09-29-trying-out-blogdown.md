---
title: Trying out BlogDown
author: Bruce
date: '2017-09-29'
slug: trying-out-blogdown
categories:
  - Housekeeping
tags: []
---

I'm trying out blogdown (https://bookdown.org/yihui/blogdown/) for serving up the project notebook. Advantages:

- Rebuilds only update changed files
- Dialog box for creating new posts
- Automatically shows posts in date order, presumably even if they've been edited
- *Might* be possible to automate serving it up on the github page, so Jenn can just look at it in a web browser

(Potential) Disadvantages:

- Whole new file structure to figure out (in particular, I may have to modify the knitr option for setting the working directory, which was based on a relative path)
- Presumably I can set up the post creation script to include my ProjectTemplate preamble, but that will take work/trial and error
- I will have to figure out how caching works with this.
- Out of the box, doesn't seem to have any way to do universal search (like bookdown has) or easy way to complile all the pages to pdf
- The default theme doesn't inlude any links to allow filtering by categories from within the website (however, there is a `find_tags()` and similar functions within the package)
- The index file is rebuilt every time, which might get slow as the number of posts gets large (unless there's a way to have static archives)

# Installation
After installing the **blogdown** package from CRAN, I:

- Installed Hugo using `blogdown::install_hugo()`
- Created a new site using `blogdown::new_site()`
- Deleted the preexisting posts (after looking at them) from the `content/` subdirectory
- In `config.toml`:
  - Changed the title (although this doesn't appear anywhere in the server window)
  - Switched the GitHub link to the InvasionHet page
  - Got rid of the Twitter link
  - Stole a runway picture from one of Jenn's talks, putting it in `public/images/Runway.png`, and set it as the icon
- In my first file with the ProjectTemplate header, I found that the `root.dir` has to be set to `../../..` (knitr is running from within the `content/posts/` directory)  

# Issues

- There is a mismatch between line length used by knitr to output data and the page width in the html file (which is actually rather narrow, given the font size). I think the solution is to make the html with larger, somehow.
