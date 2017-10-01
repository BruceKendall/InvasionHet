---
title: Customizing blogdown
author: Bruce Kendall
date: '2017-09-30'
slug: customizing-blogdown
categories:
  - Housekeeping
tags:
  - blogdown
---

I am now on my computer at home; after updating packages and rstudio, and installing **blogdown** and **hugo**, everything is running fine (although I still need to update my `~/.Rprofile`).

# Tags and categories
Getting links to tags and categories was as simple as editing the menu section of `config.toml`; the one trick is to add `weight` variables to get the order I want (the default is alphabetical). Here is the relevant bit of code:
```
[[menu.main]]
    name = "About"
    url = "/about/"
    weight = 1
[[menu.main]]
    name = "Categories"
    url = "/categories/"
    weight = 2
[[menu.main]]
    name = "Tags"
    url = "/tags/"
    weight = 3
[[menu.main]]
    name = "GitHub"
    url = "https://github.com/BruceKendall/InvasionHet"
    weight = 4
```
The one bit that is incomplete is that hugo is not building `index.html` files for the `categories` and `tags` directories.

One thing to try is to explictly set the `taxonomies` section of the config file, although it should be using these values by default (see https://gohugo.io/getting-started/configuration/).

Nope, that didn't do anything. Of note: the `index.xml` files *are* being updated; and the `index.html` files *within* each of the category and tag directories are being updated. I found a sample file at https://bwaycer.github.io/hugo_tutorial.hugo/templates/terms/; I had to take out the `{{ .Site.LanguagePrefix }}` from the alphabetical example to get it to work. Now I have category and tag lists, although the formatting is inconsistent with the rest of template.

The code in `terms.html` is:
```
{{ partial "header.html" . }}

<section id="main">
  <div>
    <h1 id="title">{{ .Title }}</h1>
    <ul>
    {{ $data := .Data }}
    {{ range $key, $value := .Data.Terms.Alphabetical }}
      <li><a href="/{{ $data.Plural }}/{{ $value.Name | urlize }}">{{ $value.Name }}</a> {{ $value.Count }}</li>
    {{ end }}
    </ul>
  </div>
</section>
{{ partial "footer.html" . }}
```

# Setting up `.Rprofile`
There are a number of blogdown options that can be set in `.Rprofile`, including the default page author, the default file type, and whether to deamonize the server. I have set the following in my global `~/.Rprofile`:
```{r global_Rprofile, eval = FALSE}
# Blogdown options
options(blogdown.author = "Bruce Kendall",
        blogdown.ext = ".Rmd")
```

I tried moving the knitr options and the **ProjectTemplate** loading into a local `.Rprofile`. The first thing I found is that the `.Rprofile` needs ot be in the same directory as the `.Rmd` files (`content/posts/`). The second thing I found was that, while the knitr options from, e.g., `knitr::opts_knit$set()` work that way, the call to `ProjectTemplate::load.project()` didn't make the datasets and functions available to the chunks in the `.Rmd` file.

So I can use the following local `.Rprofile`:
```{r local_Rprofile, eval=FALSE}
# Load global .Rprofile
if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# Knitr options. Note that these mean:
#   - All output will be cached. This is desireable in case code/data changes in the 
#     future; but it means that all chunks should be labeled for reliable behavior. 
#     DEPENDENCIES ACROSS CODE CHUNKS MAY BE AN ISSUE.
#   - R code will run in the project root directory
project_root <- normalizePath("../../..")
knitr::opts_chunk$set(comment = '', cache = TRUE)
knitr::opts_knit$set(root.dir = project_root)

# Clean up
rm(project_root, local_dir)
```
However, I still need to start each Rmarkdown file with `ProjectTemplate::load.project()`.

# Setting up to use the blogdown notebook as a project webpage
We can serve up the notebook as a githob project webpage if the html is published to the `/docs/` directory of the project. The easiest way to do this will be to move the **blogdown** directories to the top level of the project, and set Hugo to publish to `/docs/` instead of `/public/`. We may also want the local (preview) version of the html to go somewhere that is not under version control, to avoid delays with Rstudio's version control (although at the moment I can't find the place where I read that tip).

One benefit is that I can then launch the webserver without changing directory first.

So the first step is to move all the contents of `/notebook2/` to the root directory .... Done.

Then I need to reduce the upward movement in the local `.Rprofile` by one level ... Done.

I confirm that this works for the local build.

I set `publishDir = "docs/"` in `config.toml`. This sends the local build there, which is perhaps not what I want.

When I pushed this, and activated the website feature on github, I did have a home page at https://brucekendall.github.io/InvasionHet/. But it didn't have any styling, and the links didn't work. I've now changed the `baseurl` in `config.toml` to point to this site. Let's see how that works... It didn't, but I left the trailing slash off. Let's add that and try...

Success! But the runway image got lost in the transition, I just moved that from `public` to `docs`. All is now good.

# More tweaks
I adjusted the line width in `main.css`, and borrowed material from `lists.html` to make `terms.html` look more like the rest of the theme. I think that's good enough on the look and feel. 

I also figured out that setting `weight: 1` in the yaml header of a post will push it below unweighted posts with the same date. (This was to get the ordering right from yesterday).

I do still need to write an "about" page (and for that matter a project readme), and make a pre-dated post that links to the bookdown version of the previous notebook (converting the individual posts would be more work than is warranted).

