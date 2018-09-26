---
title: Some thoughts on approaches
author: Bruce Kendall
date: '2018-09-26'
slug: some-thoughts-on-approaches
categories:
  - Planning
tags: []
---

Some things I thought about before I got up:

- Testing array manipulation: I'm pretty confident in my accuracy in iterating the models using for loops, so I should write a (slow) version with loops that will be the check for faster `apply()` approaches.
- Another approach to code verification is to compare various outputs (e.g., seed production as a function of density) to the means and residual variances that are intended to go in. This can be built up incrementally with various sources of variability turned on or off.
- If I set a RNG seed, I should tie it to an R version number. I also need to look into the various generators to get a sense of which are better.
- When it comes to "validating" my models against the population data, I can generate multiple (200?) sets of *n* replicates (where *n* is the number of reps in the data), and compare those with the mean and variance of 1-step spread through time, and mean and variance of cumulative spread through time. I can even generate P values. Graphically, I think I show the time series (e.g., of mean 1-step spread vs. generation) for each simulation in light line, and then the data in a solid line.
- Although the TRACE document should have a table of parameters (and their SEs), it doesn't seem like there's a place to include the actual estimation in TRACE. So I probably need a separate supplementary document for that. EDIT: the section on data does say that it should include "the observed patterns that were used to design the overall model structure." So perhaps that is sufficient. But it does seem that showing the data analysis code would be desireable? Maybe not, as then we would need to share the raw data. And the focus of the paper is not the empirical analysis per se, but the model analysis.
