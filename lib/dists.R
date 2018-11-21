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
