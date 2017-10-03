---
title: Housekeeping
author: Bruce Kendall
date: '2017-09-25'
slug: housekeeping
categories:
  - Housekeeping
tags: []
---

After repeated nagging from ProjectTemplate, I ran the script to update the project structure to v. 0.8. This changed the doc directory to docs, and added some new variables to global.dcf. Unfortunately the latter don't seem to be documented.... I'll also need to make sure that I update the package on my computer at home.

I added a new munge script to calculate the dispersal stats and fits. To prevent this calculating every time, I cached the result; but now I need to add caching to all the munge scripts, and, after running once, turn off caching... Done!
