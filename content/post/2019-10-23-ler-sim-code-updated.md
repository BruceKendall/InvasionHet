---
title: Ler sim code updated
author: Bruce Kendall
date: '2019-10-23'
slug: ler-sim-code-updated
categories:
  - Code development
tags:
  - Ler
---


I've gotten the 8-pot runway extension limit into the Ler simulation code.
Along the way I tracked down some bugs that may have been causing problems when I abandoned the project last spring (I think when I was trying to run RIL models).
After much chasing of strange errors and intermediate output, I discovered that most (all?) of the headaches arose from `aaply()` dropping array dimesions; I fixed these by using the `.drop = FALSE` argument.
