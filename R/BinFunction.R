#' @title Binomial Simulation
#' @description Simulates a binomial distribution
#'
#' @param iter The number of iterations that will be simulated
#' @param n The number of draws in each iterated sample
#' @param p The probabilty of a "success"
#'
#' @return
#' @export
#'
#' @examples
#' mybin(10)
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

#' @title Fill the area under a curve
#' @description Graphs a normal distribution and fills in the lower tail probability of some point a
#'
#' @param mu Mean of the function
#' @param sigma Standard devaition of the function
#' @param a Alpha level
#'
#' @return
#' @export
#'
#' @examples
#' function(4,8, .05)
myncurve = function(mu, sigma,a){
    curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
    # Find the area less than a
    prob = pnorm(a, mu, sigma)
    # Round area
    prob=round(prob,4)
    # x values corresponding to the x - cords of points on the curve
    xcurve=seq(mu-3*sigma,a,length=1000)

    # Y values corresponding t0 the x values
    ycurve=dnorm(xcurve,mean=mu,sd=sigma)

    # Fill in the polygon
    polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")

    print(as.list(prob))
}
