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
