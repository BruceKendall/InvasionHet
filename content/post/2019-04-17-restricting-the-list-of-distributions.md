---
title: Restricting the list of distributions
author: Bruce Kendall
date: '2019-04-17'
slug: restricting-the-list-of-distributions
categories:
  - Parameter estimation
tags:
  - dispersal
  - Ler
---

I've just realized that any dispersal distribution that I'm actually going to use in the model requires the ability to generate random numbers. Thus, for the "custom" distributions I'd have to write a RNG, which I really don't want to do. 

So I think that means ditching the 2Dt, log-sech, and inverse power models.

If the best fit is a truncated distribution then I might need to write a simple wrapper to discard the truncated bit.
