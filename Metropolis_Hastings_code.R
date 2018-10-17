trueA <- 5 # Here we specify true A
trueB <- 0 # and true B
trueSd <- 10 # and finally true sd
samplesize <- 31 # and the sample size

# create independent x-values
x <- (-(samplesize-1)/2):((samplesize-1)/2) # gives the x values from -15 to 15
# create dependent values according to ax+b+N(0, sd)
y <- trueA*x+trueB+rnorm(n=samplesize, mean=0, sd=trueSd) 
# using the true parameters that we specified above, and by adding soem noise from Normal distribution with mean 0 and sd with true sd that we specified above, we create y values.

plot(x,y, main="Test Data") # we draw a plot of x and y values. 

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

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))} # This function yields the sum of likelihoods when the parameter for slope is a, and the other parameters are set as above.
slopelikelihoods <- lapply(seq(3,7,by=.05), slopevalues) # slopelikelihoods has the sum of likelihoods of slope from 3 to 7 by 0.05 as list format.
plot(seq(3,7,by=.05), slopelikelihoods, type="l", xlab="values of slope parameter a", ylab = "Log likelihood") # This draws a plot with horizontal axis with 3 to 7 by 0.05 with its sum of likelihoods respectively. That is, this gives the graph of sum of likelihoods of slope as the slope changes from 3 to 7.

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

# Here, we set up the startvalue and iterations as below:
startvalue = c(4,0,10)
chain=run_metropolis_MCMC(startvalue, 10000)
burnIn=5000 # we also set burnIn so that we discard the values that maybe influenced by the initial value.
acceptance=1-mean(duplicated(chain[-(1:burnIn),]))
# We compute the acceptance rate that tells us how often the proposal was accepted by the metropolis-hastings acceptance criterion. 

# Summary: Here, we draw 6 plots that show the posterior of parameters and how chain values have changed. 

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1], nclass=30, main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a")
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b")
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd")
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))
