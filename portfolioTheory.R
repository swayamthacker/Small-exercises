#Here we have an optimization problem with a quadratic objective (efficient frontier is a parabolic
#function) and linear constraint functions.Thus, we need to use a quadratic program.

#The constraints are that portfolio weights must sum to 1, portfolio weights cannot be negative
#(only buy and hold allowed,no shorts) and max allocation for a given security is 50% (arbitrary decision).


#Using quadprog's solve.QP for modelling the efficient frontier.
#Each time the solve.QP function minimizes the variance, and then the risk premiums (returns) are 
#varied along the efficient frontier. The sharpe ratio is defined as 
#expectedReturns/StandardDeviation. The portfolio with the largest sharpe ratio is the most 
#efficient portfolio (does mean-variance optimization).


library(stockPortfolio) #getReturns function
library(quadprog) #Needed for solve.QP

stocks <- c('ISP', 'CSV', 'RGC', 'WMS', 'GYB', 'KCC', 'BPL', 'WTW', 'GS', 'SPR')

returns <- getReturns(stocks, 'd',start="2013-12-31") 

#Here the ticker WMS only has returns starting from July 2014. Thus, in order to maintain
#a positive definite covariance matrix for the quadprog solver, all the returns data will
#be taken from july 2014 to present.


#returns <= subset(returns, )
#returns$R[is.na(returns$R)] <- 0

# In the below function, 
#returns is a matrix containing returns for each security
#maximumAllocation is the maximum % allowed for any one security (reduces concentration) 
#mktRiskPremium is the upper limit of the mkt risk premium modeled
#incrementalMktRiskPremium is the value to increment risk premium by; from 0 to mktRiskPremium;
#it is useful for determining the efficient frontier for different risk premium profiles

efficientFrontier <- function (returns, maximumAllocation=NULL,
                          mktRiskPremium=1, incrementalMktRiskPremium=0.001)
{
  
  covariance = cov(returns)
  num = ncol(covariance)
  

  Amat <- matrix (1, nrow=num)
  #print(Amat)
  bvec <- 1
  meq <- 1
  #print(Amat)
  #print(bvec)
  #print(meq)
  Amat <- cbind(1, diag(num))
  #print(Amat)
  bvec <- c(bvec, rep(0, num))
  
  # Adjust for maximum allocation
  if(!is.null(maximumAllocation)){
    if(maximumAllocation > 1 | maximumAllocation <0){
      stop("Invalid value for max Allocation")
    }
    if(maximumAllocation * num < 1){
      stop("Need to set maximumAllocation higher; assets dont add up to 1")
    }
    Amat <- cbind(Amat, -diag(num))
    bvec <- c(bvec, rep(-maximumAllocation, num))
  }
  
  # Calculating the number of loops
  loops <- mktRiskPremium / incrementalMktRiskPremium + 1
  loop <- 1
  
  effMatrix <- matrix(nrow=loops, ncol=num+3)
  colnames(effMatrix) <- c(colnames(returns), "StandardDeviation", "ExpectedReturn", "SharpeRatio")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=mktRiskPremium, by=incrementalMktRiskPremium))
    {
      dvec <- colMeans(returns) * i # This moves the solution along the EF
      sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
      effMatrix[loop,"StandardDeviation"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
      effMatrix[loop,"ExpectedReturn"] <- as.numeric(sol$solution %*% colMeans(returns))
      effMatrix[loop,"SharpeRatio"] <- effMatrix[loop,"ExpectedReturn"]/effMatrix[loop,"StandardDeviation"]
      effMatrix[loop,1:num] <- sol$solution
      loop <- loop+1
    }
  
  return(as.data.frame(effMatrix))
}

# Run the eff.frontier function based on no short and 50% alloc. restrictions
frontier <- efficientFrontier(returns=returns$R,maximumAllocation=0.5,
                    mktRiskPremium=1, incrementalMktRiskPremium=.001)

# Find the optimal portfolio
frontier.optimal.point <- frontier[frontier$SharpeRatio==max(frontier$SharpeRatio),]

print (frontier.optimal.point)


