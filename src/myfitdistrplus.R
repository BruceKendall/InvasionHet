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
