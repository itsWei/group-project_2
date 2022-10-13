## Different from strategy 3, in strategy 1 and 2, the success of one prisoner is not independent of 
## the success of other prisoners since they depend on the distribution of 2n numbers.


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


## function Pone estimates the probabilities of success for a single prisoner under each strategy.

# strategy 1: give a function to estimate the probabilities of success for a single prisoner under strategy 1;
# the function takes n, k (the prison's number), S () as inputs and the function returns ???
strategy1 <- function(n, k, S) {
  card <- k  # the prisoner starts at the box with their number on it, opens it and reads the number on the card: k.
  index <- rep(0, 2*n)  # create a zero vector with length 2n to represent the index of card number
  index[k] <- 1  # if cark k is found, denote 1 at index k
  
  for (i in 1:(n+1)) {
    if (S[card] == k) break # stop opening the boxes when find their owm number k.
    else {
      card <- S[card] # continue to open boxes to find new cards.
      index[card] <- 1 # denote 1 for all card in boxes that have been opened.
    }
  }
  
  if (sum(index) <= n) {1}  # find their own number with opening boxes within n times.
  else {0}  # without finding their own number within n times.
}

## same as strategy 1, but the prisoner starts from a randomly selected box.
strategy2 <- function(n, k, S) {
  card <- sample(1:(2*n), 1) #  the prisoner starts from a randomly selected box.
  index <- rep(0, 2*n)
  index[card] <- 1
  
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

## randomly choose n boxes from 2n boxes
strategy3 <- function(n, k, S) {
  if (k %in% S[1:n]) {1}  # the selected n cards contain number k
  else {0}  # the selected n cards dose not contain number k.
}

## function Pone takes n, k (the prisoner’s number), strategy (1, 2 or 3) 
## and nreps (the number of replicate simulations to run in order to estimate the probability) as inputs.
## function Pone returns the probability estimate.

Pone <- function(n, k, strategy, nreps) {
  pass <- rep(0, nreps)
  if (strategy == 1) {  # choose strategy 1
    for (i in 1:nreps) {  # simulate strategy 1 nreps times
      S <- sample(1:(2*n), 2*n)
      pass[i] <- strategy1(n, k, S)
    } 
  } else if (strategy == 2) {  # choose strategy 2
    for (i in 1:nreps) {  # simulate strategy 2 nreps times
      S <- sample(1:(2*n), 2*n)
      pass[i] <- strategy2(n, k, S)
    }
  }else {  # choose strategy 3
    for (i in 1:nreps) {  # simulate strategy 3 nreps times
      S <- sample(1:(2*n), 2*n)
      pass[i] <- strategy3(n, k, S)
    }
  }
  sum(pass)/nreps  # the probabilities of success for a single prisoner.
}


Pone(5,1,1,10000)
Pone(5,1,2,10000)
Pone(5,1,3,10000)

##2 
Pall <- function(n, strategy, nreps) {
  success <- rep(0, nreps)
  if (strategy == 1) {
    for (i in 1:nreps) {
      S <- sample(1:(2*n), 2*n)
      for (j in 1:(2*n)) {
        success[i] <- strategy1(n, j) + success[i]
      }
      }
    sum(success == 2*n) / nreps
    }
  else if (strategy == 2) {
    for (i in 1:nreps) {
      S <- sample(1:(2*n), 2*n)
      for (j in 1:(2*n)) {
        success[i] <- strategy2(n) + success[i]
      }
      }
    sum(success == 2*n) / nreps
    }
  else if (strategy == 3) {
    for (i in 1:nreps) {
      S <- sample(1:(2*n), 2*n)
      for (j in 1:(2*n)) {
        success[i] <- strategy3(n, j) + success[i]
      }
      }
    sum(success == 2*n) / nreps
  }
  }

##3
##  example code using function Pone to estimate the individual probabilities under strategy 1, 2 and 3 for n = 5.
Pone(5,1,1,10000)
Pone(5,1,2,10000)
Pone(5,1,3,10000)
##  example code using function Pone to estimate the individual probabilities under strategy 1, 2 and 3 for n = 50.
Pone(50,1,1,10000)
Pone(50,1,2,10000)
Pone(50,1,3,10000)

##  example code using function Pall to estimate the joint success probabilities under strategy 1, 2 and 3 for n = 5.
Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)
##  example code using function Pall to estimate the joint success probabilities under strategy 1, 2 and 3 for n = 50.
Pall(50,1,10000)
Pall(50,2,10000)
Pall(50,3,10000)


##4
## The probability that all prisoners find their own number is: 
## p(2n prisoners all escape successfully)
## =p(maximu loops length less than or equal to n)
## =1-p(maximum loops length longer than n)
## =1-1/(n+1)-1/(n+2)-...-1/(2n)=0.31
## which is surprising large.



