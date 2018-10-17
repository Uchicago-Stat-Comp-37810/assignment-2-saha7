trueA <- 5 # Here we specify true A
trueB <- 0 # and true B
trueSd <- 10 # and finally true sd
samplesize <- 31 # and the sample size

# create independent x-values
x <- (-(samplesize-1)/2):((samplesize-1)/2) # gives the x values from -15 to 15
# create dependent values according to ax+b+N(0, sd)
y <- trueA*x+trueB+rnorm(n=samplesize, mean=0, sd=trueSd) 
# using the true parameters that we specified above, and by adding soem noise from Normal distribution with mean 0 and sd with true sd that we specified above, we create y values.

# This likelihood function gives the sum of all likelihoods of y by using dnorm functions. We also set that it yields log values. (=> logf(x))
likelihood <- function(param){
  a=param[1]
  b=param[2]
  sd=param[3]
  
  pred=a*x+b
  singlelikelihoods = dnorm(y, mean = pred, sd=sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))} # This function yields the sum of likelihoods when the parameter for slope is a, and the other parameters are set as above.

# Prior distribution
# Here, we specify the prior distribution of parameters. This function gives the sum of log(density) of 3 parameters. We just add 3 values since all of them are in log term. 

prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}

# Next, the posterior function adds the sum of loglikelihoods of y and log(prior density) of 3 parameters.
posterior <- function(param){
  return(likelihood(param)+prior(param))
}

# Metropolis algorithm

# Proposal function gives the possible parameters from normal distribution with mean of formerly specified values. (previously determined values)
proposalfunction <- function(param){
  return(rnorm(3, mean = param, sd=c(0.1, 0.5, 0.3)))
}

# Then, we can finally run Metropolis-Hastings MCMC by determining the initial value and the number of iterations. This function gives the chain, which includes 3 parameters (intercept, slope and sd) with the process of how they have changed as the iterations went on. It starts with putting the initial value that we specified into the proposalfunction, and if exp(log(likelihood from proposalfunction)-log(the previous likelihood)) is bigger than the random probability then we move to the next step. If it is not bigger, then the chain has the same values. 

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

summaryplot <- function(chain, burnIn, truevec){
  par(mfrow = c(2,3))
  param <- c("a","b","sd")
  
  for(i in 1:3){
    hist(chain[-(1:burnIn),i], nclass=30, main=paste0("Posterior of ",param[i]), xlab="True value = red line" )
    abline(v = mean(chain[-(1:burnIn),i]))
    abline(v = true[i], col="red")
  }
  
  for(i in 1:3){
    plot(chain[-(1:burnIn),i], type = "l", xlab="True value = red line" , main = paste0("Chain values of ",param[i]))
    abline(h = true[i], col="red" )
  }
}

# 5. New function

compare_outcomes <- function(iteration){
  result <- matrix(rep(0,20),10,2)
  
  for(i in 1:10){
    a <- runif(1, 0, 10)
    b <- rnorm(1, 0, 5)
    sd <- runif(1, 0, 30)
    param <- c(a, b, sd)
    chain <- run_metropolis_MCMC(param, iteration)
    result[i, ] <- c(mean(chain[,1]), sd(chain[,1]))
  }
  return(result)
}

