### Half-normal distribution
dhnorm <- function(x, sigma = 1, log = FALSE) {
  (x > 0) * 2 * dnorm(x, 0, sigma, log)
}

phnorm <- function(q, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  phn <- (q > 0) * (1 - 2 * pnorm(q, 0, sigma, lower.tail = FALSE))
  if (!lower.tail) phn <- (q > 0) * (1 - phn)
  if (log.p) phn <- log(phn)
  phn
}

rbeta2 <- function (n, mean, sdev) 
{
  stopifnot(is.numeric(n), is.numeric(mean), is.numeric(sdev), 
            length(n) == 1, length(mean) == 1, length(sdev) == 1)
  if (mean > 1 || mean < 0) {
    stop("Attempted to use a mean for the beta distribution that is outside of [0,1]")
  }
  else if (sdev == 0) {
    warning("Requesting random variates with no variation. Returning replicates of mean")
    return(rep(mean, n))
  }
  else {
    sigma2 <- sdev^2
    if (sigma2 >= (1 - mean) * mean) {
      stop("Standard deviation too high for beta distribution")
    }
    temp <- ((mean * (1 - mean)/(sigma2)) - 1)
    a <- mean * temp
    b <- (1 - mean) * temp
    return(rbeta(n, a, b))
  }
}
