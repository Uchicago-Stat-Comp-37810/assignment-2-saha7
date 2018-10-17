

source("source.R")

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

# Example: plot the likelihood profile of the slope a
slopelikelihoods <- lapply(seq(3,7,by=.05), slopevalues) # slopelikelihoods has the sum of likelihoods of slope from 3 to 7 by 0.05 as list format.
plot(seq(3,7,by=.05), slopelikelihoods, type="l", xlab="values of slope parameter a", ylab = "Log likelihood") # This draws a plot with horizontal axis with 3 to 7 by 0.05 with its sum of likelihoods respectively. That is, this gives the graph of sum of likelihoods of slope as the slope changes from 3 to 7.

# Here, we set up the startvalue and iterations as below:
startvalue = c(4,0,10)
chain=run_metropolis_MCMC(startvalue, 10000)
burnIn=5000 # we also set burnIn so that we discard the values that maybe influenced by the initial value.
acceptance=1-mean(duplicated(chain[-(1:burnIn),]))
# We compute the acceptance rate that tells us how often the proposal was accepted by the metropolis-hastings acceptance criterion. 

# Summary: Here, we draw 6 plots that show the posterior of parameters and how chain values have changed. 

true <- c(5,0,10)
summaryplot(chain, burnIn = 5000, true)

# for comparison:
summary(lm(y~x))

# Part1:R #5 New function

# We want to make a function called 'compare_outcomes' with input of iteration number
compare_outcomes <- function(iteration){
  result <- matrix(rep(0,20),10,2) # We will later assign values to this result matrix
  
  for(i in 1:10){
    a <- runif(1, 0, 10) # We generate a random a from a prior distribuion for each loop
    b <- rnorm(1, 0, 5) # and also a random b
    sd <- runif(1, 0, 30) # and also a random sd 
    param <- c(a, b, sd) # we combine those parameters into a vector
    
    # We make 'chain' for 'iteration' that we will assign
    chain <- run_metropolis_MCMC(param, iteration)
    # Put mean value at the first column and the std at the second 
    result[i, ] <- c(mean(chain[,1]), sd(chain[,1]))   
    }
  return(result)
}

# Here are the outcome matrices for iteration of 1000, 10000 and 100000. 
compare_outcomes(1000)
compare_outcomes(10000)
compare_outcomes(100000)

summaryplot(chain, burnIn = 5000, true)
