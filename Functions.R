


#Normal Scores----------------------------------------------------------

#modified function from AS #nscore_examples.R from Nov 18th lecture
#same file has function to go back
#NOTE: our's is not sorted and his was
nscore <- function(x) {
  # Takes a vector of values x and calculates their normal scores. Returns 
  # a list with the scores and an ordered table of original values and
  # scores, which is useful as a back-transform table. See backtr().
  nscore <- qqnorm(x, plot.it = FALSE)$x  # normal score 
  trn.table <- data.frame(x=x,nscore=nscore)
  
  return (list(nscore=nscore, trn.table=trn.table))
}

# Transform back normal scores function from Ashton's lecture-------------

#modified function from AS #nscore_examples.R from Nov 18th lecture
# this one also take the spdf as input
# instead of the nscore object output

ns.backtr <- function(scores, nscore, tails='none', draw=TRUE) {
  if(tails=='separate') { 
    mean.x <- mean(nscore$x)
    small.x <- nscore$x < mean.x
    large.x <- nscore$x > mean.x
    small.sd <- sqrt(sum((nscore$x[small.x]-mean.x)^2)/
                       (length(nscore$x[small.x])-1))
    large.sd <- sqrt(sum((nscore$x[large.x]-mean.x)^2)/
                       (length(nscore$x[large.x])-1))
    min.x <- mean(nscore$x) + (min(scores) * small.sd)
    max.x <- mean(nscore$x) + (max(scores) * large.sd)
    # check to see if these values are LESS extreme than the
    # initial data - if so, use the initial data.
    #print(paste('lg.sd is:',large.sd,'max.x is:',max.x,'max nsc.x is:',max(nscore$trn.table$x)))
    if(min.x > min(nscore$x)) {min.x <- min(nscore$x)}
    if(max.x < max(nscore$x)) {max.x <- max(nscore$x)}
  }
  if(tails=='none') {   # No extrapolation
    min.x <- min(nscore$x)
    max.x <- max(nscore$x)
  }
  min.sc <- min(scores)
  max.sc <- max(scores)
  x <- c(min.x, nscore$x, max.x)
  nsc <- c(min.sc, nscore$nscore, max.sc)
  
  if(draw) {plot(nsc,x, main='Transform Function')}
  back.xf <- approxfun(nsc,x) # Develop the back transform function
  val <- back.xf(scores)
  return(val)
}