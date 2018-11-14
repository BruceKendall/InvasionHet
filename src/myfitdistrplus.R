computegetparam <- function(argdistname) {
  # Add "y" to the following list
  nonparaminR <- c("x", "p", "q", "n", "log", "log.p", "lower.tail", 
                   "ncp", "y")
  nonparaminActuar <- c("limit", "order", "t")
  nonparaminGamlssdist <- "fast"
  nonparamspecial <- c("...", "..1", "..2")
  nonparaminGenHyperbolic <- c("param", "KOmega", "ibfTol", 
                               "nmax", "method", "intTol", "valueOnly", "nInterpol", 
                               "uniTol", "subdivisions", "logPars")
  nonparamsn <- "dp"
  plist <- setdiff(argdistname, nonparaminR)
  plist <- setdiff(plist, nonparaminActuar)
  plist <- setdiff(plist, nonparaminGamlssdist)
  plist <- setdiff(plist, nonparamspecial)
  plist <- setdiff(plist, nonparaminGenHyperbolic)
  plist <- setdiff(plist, nonparamsn)
  plist
}

fitdistcens <- function (censdata, distr, start = NULL, fix.arg = NULL, keepdata = TRUE, 
          keepdata.nb = 100, ...) 
{
  if (missing(censdata) || !(is.vector(censdata$left) & is.vector(censdata$right) & 
                             length(censdata[, 1]) > 1)) 
    stop("datacens must be a dataframe with two columns named left \n            and right and more than one line")
  leftsupright <- censdata$left > censdata$right
  leftsupright <- leftsupright[!is.na(leftsupright)]
  if (any(leftsupright)) 
    stop("each left value must be less or equal to the corresponding right value")
  if (!is.character(distr)) 
    distname <- substring(as.character(match.call()$distr), 
                          2)
  else distname <- distr
  ddistname <- paste("d", distname, sep = "")
  if (!exists(ddistname, mode = "function")) 
    stop(paste("The ", ddistname, " function must be defined"))
  pdistname <- paste("p", distname, sep = "")
  if (!exists(pdistname, mode = "function")) 
    stop(paste("The ", pdistname, " function must be defined"))
  if (!is.logical(keepdata) || !is.numeric(keepdata.nb) || 
      keepdata.nb < 3) 
    stop("wrong arguments 'keepdata' and 'keepdata.nb'.")
  my3dots <- list(...)
  if (length(my3dots) == 0) 
    my3dots <- NULL
  pseudodata <- cens2pseudo(censdata)$pseudo
  arg_startfix <- manageparam(start.arg = start, fix.arg = fix.arg, 
                              obs = pseudodata, distname = distname)
  argddistname <- names(formals(ddistname))
  hasnodefaultval <- sapply(formals(ddistname), is.name)
  arg_startfix <- checkparamlist(arg_startfix$start.arg, arg_startfix$fix.arg, 
                                 argddistname, hasnodefaultval)
  if (is.function(fix.arg)) 
    fix.arg.fun <- fix.arg
  else fix.arg.fun <- NULL
  dpq2test <- c("d", "p")
  resdpq <- testdpqfun(distname, dpq2test, start.arg = arg_startfix$start.arg, 
                       fix.arg = arg_startfix$fix.arg, discrete = FALSE)
  if (any(!resdpq$ok)) {
    for (x in resdpq[!resdpq$ok, "txt"]) warning(x)
  }
  mle <- mledist(censdata, distname, start = arg_startfix$start.arg, 
                 fix.arg = arg_startfix$fix.arg, checkstartfix = TRUE, 
                 ...)
  if (mle$convergence > 0) 
    stop("the function mle failed to estimate the parameters, \n        with the error code ", 
         mle$convergence)
  estimate <- mle$estimate
  if (!is.null(mle$hessian)) {
    if (all(!is.na(mle$hessian)) && qr(mle$hessian)$rank == 
        NCOL(mle$hessian)) {
      varcovar <- solve(mle$hessian)
      sd <- sqrt(diag(varcovar))
      correl <- cov2cor(varcovar)
    }
    else {
      varcovar <- NA
      sd <- NA
      correl <- NA
    }
  }
  else {
    varcovar <- NA
    sd <- NA
    correl <- NA
  }
  loglik <- mle$loglik
  n <- nrow(censdata)
  npar <- length(estimate)
  aic <- -2 * loglik + 2 * npar
  bic <- -2 * loglik + log(n) * npar
  fix.arg <- mle$fix.arg
  weights <- mle$weights
  if (!is.null(fix.arg)) 
    fix.arg <- as.list(fix.arg)
  if (keepdata) {
    reslist <- list(estimate = estimate, method = "mle", 
                    sd = sd, cor = correl, vcov = varcovar, loglik = loglik, 
                    aic = aic, bic = bic, n = n, censdata = censdata, 
                    distname = distname, fix.arg = fix.arg, fix.arg.fun = fix.arg.fun, 
                    dots = my3dots, convergence = mle$convergence, discrete = FALSE, 
                    weights = weights)
  }
  else {
    n2keep <- min(keepdata.nb, n) - 4
    imin <- unique(apply(censdata, 2, which.min))
    imax <- unique(apply(censdata, 2, which.max))
    subdata <- censdata[sample((1:n)[-c(imin, imax)], size = n2keep, 
                               replace = FALSE), ]
    subdata <- rbind.data.frame(subdata, censdata[c(imin, 
                                                    imax), ])
    reslist <- list(estimate = estimate, method = "mle", 
                    sd = sd, cor = correl, vcov = varcovar, loglik = loglik, 
                    aic = aic, bic = bic, n = n, censdata = subdata, 
                    distname = distname, fix.arg = fix.arg, fix.arg.fun = fix.arg.fun, 
                    dots = my3dots, convergence = mle$convergence, discrete = FALSE, 
                    weights = weights)
  }
  return(structure(reslist, class = "fitdistcens"))
}
#environment(fitdistcens) <- asNamespace("fitdistrplus")
