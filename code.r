## Different from strategy 3, in strategy 1 and 2, the success of one prisoner is not independent of 
## the success of other prisoners since they depend on the distribution of 2n numbers.

## p(2n prisoners all escape successfully)=p(maximu loops length less than or equal to n)
## =1-p(maximum loops length longer than n)=1-1/(n+1)-1/(n+2)-...-1/(2n)=0.31, which is surprising large.

## Group member:
## Changliang Wei S2345097
## Simin Wang S2417531
## Zheng Wang S2283040

## Contribution: 
## Each member contributes roughly the same to this project.
## Changliang Wei and Simin Wang contribute more on the first function.
## Zheng Wang contributes more on the comments.

## Overview comments:
## This code gives a method to solve the 2n prisoner problem, the code estimates the probabilities of success for a single
## prisoner under strategy 1,2 and 3. 
## Then the code to estimate a probability distribution is provided.
## Example code using functions to estimate the individual and joint success probabilities under
## each strategy for different n is also given, with probabilities visualised. 


## 1
strategy1 <- function(n, k, S) {
  card <- k
  index <- rep(0, 2*n)
  index[k] <- 1
  
  for (i in 1:(n+1)) {
    if (S[card] == k) break
    else {
      card <- S[card]
      index[card] <- 1
    }
  }
  
  if (sum(index) <= n) {1}
  else {0}
}

strategy2 <- function(n, k, S) {
  card <- sample(1:(2*n), 1)
  index <- rep(0, 2*n)
  index[card] <- 1
  
  S <- sample(1:(2*n), 2*n)
  for (i in 1:(n+1)) {
    if (S[card] == k) break
    else {
      card <- S[card]
      index[card] <- 1
    }
  }
  
  if (sum(index) <= n) {1}
  else {0}
}

strategy3 <- function(n, k, S) {
  if (k %in% S[1:n]) {1}
  else {0}
}


Pone <- function(n, k, strategy, nreps) {
  pass <- rep(0, nreps)
  if (strategy == 1) {
    for (i in 1:nreps) {
      S <- sample(1:(2*n), 2*n)
      pass[i] <- strategy1(n, k, S)
    }
  } else if (strategy == 2) {
    for (i in 1:nreps) {
      S <- sample(1:(2*n), 2*n)
      pass[i] <- strategy2(n, k, S)
    }
  } else {
    for (i in 1:nreps) {
      S <- sample(1:(2*n), 2*n)
      pass[i] <- strategy3(n, k, S)
    }
  }
  sum(pass)/nreps
}

Pone(5,1,1,10000)
Pone(5,1,2,10000)
Pone(5,1,3,10000)

##2 
Pall <- function(n, strategy, nreps) {
  success <- rep(0, nreps)
  if (strategy == 1) {
    for (i in 1:nreps) {
      for (j in 1:(2*n)) {
        success[i] <- strategy1(n, j) + success[i]
      }
      }
    sum(success == 2*n) / nreps
    }
  else if (strategy == 2) {
    for (i in 1:nreps) {
      for (j in 1:(2*n)) {
        success[i] <- strategy2(n) + success[i]
      }
      }
    sum(success == 2*n) / nreps
    }
  else if (strategy == 3) {
    for (i in 1:nreps) {
      for (j in 1:(2*n)) {
        success[i] <- strategy3(n, j) + success[i]
      }
      }
    sum(success == 2*n) / nreps
  }
  
  }
