---
title: Dispersal data
author: Bruce Kendall
date: '2017-09-19'
slug: dispersal-data
categories:
  - Data evaluation
tags:
  - Ler
  - dispersal
---

Jenn sent info about the Ler and RIL dispersal data.

### Ler
> The data from the sticky paper kernels for Ler are called `2013_08_08_Exp1_Spray.csv`. And I'm attaching the script I used to analyze them [`FitKernels_Exp1_SPRAY_aug2013.R` (in `Arabidopsis/analysis`)].

> I fit negative exponential kernels, which I know isn't necessarily the best fit, but is what I needed for the simulations I ran (for the density dependence paper). For reasons I don't know, the mean dispersal distance on sticky paper was further than in Generation 1. This of course assumes all seeds would have germinated, but we know mostly they do. Number of moms in control ranged from 2- 1200, but I found no significant relationship between density and dispersal distance. I used the estimate from the sticky paper data to keep m = 2 in my models (mean dispersal distance of ~0.5 pots). Otherwise, my simulated invasions moved much more slowly than they did in reality.

I then asked 

> I'm not quite sure how to interpret the Ler data you sent -- e.g., sometimes "pot" is > 0, but most of the seeds in "pot_gen3" are in pot 0.

Jenn replied

> As for the Ler data, we had started the experiment several generations earlier and then for a number of reasons decided to scrap it and start over. So we then took pots from a wide range of densities (from different places in the runways) to put next to the sticky paper. This means they weren’t all pot 0. Shouldn’t matter which pot they are (at least that was the idea). The pot numbers are just relics of this past.

### RILs
> You also asked about sticky paper data for the RILs. I don't think we ever dispersed the RILs onto sticky paper, but we did have 2 trials onto empty pots (with solitary moms). Script that I used attached, which references the appropriate csv files. [`FitDispersal_RILs_30May2016.R` (in `Arabidopsis/analysis`)]

I asked

>  It looks like I've already used "2015_06_30_RilsDispersal.csv"; I'll add the 2016 data (is there any reason not to combine them?). 

Jenn replied

> I don’t recall any reason not to combine the 2015 & 2016 RIL data. That said, when I ran through that script, it definitely looks like you can get a really different set of ranks depending on which data you use. I’ll look in the morning (at my paper notes) to see if there’s anything obviously different about how the data were collected.

I think that the RIL data were the ones where I identified some data entry issues when I was in Zurich. I'll need to double check that this file has the corrected data.
