### generalized gamma distribution
###
pggamma <- function(q, s, m, f){
#  if(any(q<=0)) return(0)
  if(any(m<=0)) return(NaN)
  if(any(s<=0)) return(NaN)
  if(any(f<=0)) return(NaN)
  pgamma(q^f,s,scale=(m/s)^f)}

dggamma <- function(x, s, m, f, log=FALSE){
#  if(any(x<=0)) return(0)
  if(any(m<=0)) return(NaN)
  if(any(s<=0)) return(NaN)
  if(any(f<=0)) return(NaN)
  y <- x
  tmp <- log(f)+(f-1)*log(y)+dgamma(y^f,s,scale=(m/s)^f,log=TRUE)
  if(!log)tmp <- exp(tmp)
  tmp}

qggamma <- function(p, s, m, f) {
  if(any(m<=0)) return(NaN)
  if(any(s<=0)) return(NaN)
  if(any(f<=0)) return(NaN)
  qgamma(p,s,scale=(m/s)^f)^(1/f)}

rggamma <- function(n=1, s, m, f){
  if(any(m<=0)) return(NaN)
  if(any(s<=0)) return(NaN)
  if(any(f<=0)) return(NaN)
  qgamma(runif(n),s,scale=(m/s)^f)^(1/f)}
