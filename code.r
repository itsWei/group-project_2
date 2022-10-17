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
#  prisoner under strategy 1,2 and 3. 

## Then the code to estimate a probability distribution is provided.

## Example code using functions to estimate the individual and joint success probabilities under
#  each strategy for different n is also given, with probabilities visualised. 


## Function Pone estimates the probabilities of success for a single prisoner under each strategy.
# Strategy 1: give a function to estimate the probabilities of success for a single prisoner under strategy 1;
# Function inputs: n, k (the prison's number), cards_sequence (a randomly generated sequence of cards).
# Function output: 1 or 0; 1:the k-th prisoner can find his card within n times; 0: the k-th prisoner can't find his card within n times.

strategy1 <- function(n, k, cards_sequence) {
  card <- k  # The prisoner starts at the box with their number on it, opens it and reads the number on the card: k.
  index <- rep(0, 2*n)  # Create a zero vector with length 2n to represent the index of card number.
  index[k] <- 1  # If card k is found, denote 1 at index k.
  
  for (i in 1:(n+1)) {
    if (cards_sequence[card] == k) break  # Stop opening the boxes when find their owm number k.
    else {
      card <- cards_sequence[card]  # Continue to open boxes to find new cards.
      index[card] <- 1  # Denote 1 for all card in boxes that have been opened.
    }
  }
  
  if (sum(index) <= n) {1}  # Find their own number with opening boxes within n times.
  else {0}  # Without finding their own number within n times.
}

##  Same as strategy 1, but the prisoner starts from a randomly selected box.
strategy2 <- function(n, k, cards_sequence) {
  card <- sample(1:(2*n), 1) #  The prisoner starts from a randomly selected box.
  index <- rep(0, 2*n)
  index[card] <- 1
  
  for (i in 1:(n+1)) {
    if (cards_sequence[card] == k) break
    else {
      card <- cards_sequence[card]
      index[card] <- 1
    }
  }
  
  if (sum(index) <= n) {1}
  else {0}
}

## Randomly choose n boxes from 2n boxes.
strategy3 <- function(n, k, cards_sequence) {
  if (k %in% S[1:n]) {1}  # The selected n cards contain number k.
  else {0}  # The selected n cards dose not contain number k.
}

## Function Pone takes n, k (the prisoner’s number), strategy (1, 2 or 3) .
## And nreps (the number of replicate simulations to run in order to estimate the probability) as inputs.
## Function Pone returns the probability estimate.

Pone <- function(n, k, strategy, nreps) {
  pass <- rep(0, nreps)
  if (strategy == 1) {  # Choose strategy 1.
    for (i in 1:nreps) { 
      cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
      pass[i] <- strategy1(n, k, cards_sequence)  # If the prisoner can find his number in the i-th experiment then return 1 else return 0.
    } 
  } else if (strategy == 2) {  # Choose strategy 2.
    for (i in 1:nreps) {  
      cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
      pass[i] <- strategy2(n, k, cards_sequence)  # If the prisoner can find his number in the i-th experiment then return 1 else return 0.
    }
  }else {  # Choose strategy 3.
    for (i in 1:nreps) {  
      cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
      pass[i] <- strategy3(n, k, cards_sequence)  # If the prisoner can find his number in the i-th experiment then return 1 else return 0.
    }
  }
  sum(pass)/nreps  # The probabilities of success for a single prisoner.
}



## Function Pall estimates the probability of all prisoners finding their number, so that all are released. 

# Function Pall takes n, strategy (1, 2 or 3) and nreps (the number of replicate simulations) as inputs.
# Function Pone returns the probability estimate.

Pall <- function(n, strategy, nreps) {
  success <- rep(0, nreps)
  if (strategy == 1) {  # Choose strategy 1.
    for (i in 1:nreps) {
      cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
      for (j in 1:(2*n)) {
        success[i] <- strategy1(n, j, cards_sequence) + success[i]  # The total number of prisoners who succeeded in the ith experiment.
      }
      }
    sum(success == 2*n) / nreps  # Estimated probability.
    }
  else if (strategy == 2) {  # Choose strategy 2.
    for (i in 1:nreps) {
      cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
      for (j in 1:(2*n)) {
        success[i] <- strategy2(n, j, cards_sequence) + success[i]  # The total number of prisoners who succeeded in the ith experiment.
      }
      }
    sum(success == 2*n) / nreps  # Estimated probability .
    }
  else if (strategy == 3) {  # Choose strategy 3.
    for (i in 1:nreps) {
      for (j in 1:(2*n)) {
        cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
        success[i] <- strategy3(n, j, cards_sequence) + success[i]  # The total number of prisoners who succeeded in the ith experiment.
      }
      }
    sum(success == 2*n) / nreps  # Estimated probability.
  }
  }



##  Example code using function Pone to estimate the individual probabilities under strategy 1, 2 and 3 for n = 5.
Pone(5,1,1,10000)
Pone(5,1,2,10000)
Pone(5,1,3,10000)

##  Example code using function Pone to estimate the individual probabilities under strategy 1, 2 and 3 for n = 50.
Pone(50,1,1,10000)
Pone(50,1,2,10000)
Pone(50,1,3,10000)


##  Example code using function Pall to estimate the joint success probabilities under strategy 1, 2 and 3 for n = 5.
Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)

##  Example code using function Pall to estimate the joint success probabilities under strategy 1, 2 and 3 for n = 50.
Pall(50,1,10000)
Pall(50,2,10000)
Pall(50,3,10000)



## The results are suprisingly large is because that the prisoners can use cards from each drawer they have opened to decide which drawer to open next, 
## so each prisoner's success is not independent of the others, they all depend on the distribution of cards.



## Function dloop will be given to estimate the probability of each loop occurs at least once in a random shuffling of cards to boxes.

## Function times_to_find_kcard, is to calculate the number of times to find he k card
#  Input: n, k (the prison's number), cards_sequence (a randomly generated sequence of cards).
#  Output: The number of times to find card k.

 times_to_find_kcard<- function(n, k, cards_sequence) {
  card <- k  # The prisoner starts at the box with their number on it, opens it and reads the number on the card: k.
  index <- rep(0, 2*n)  # Create a zero vector with length 2n to represent the index of card number.
  index[k] <- 1  # If card k is found, denote 1 at index k.
  
  for (i in 1:(2*n)) {
    if (cards_sequence[card] == k) break # Stop opening the boxes when find their owm number k.
    else {
      card <- cards_sequence[card] # Continue to open boxes to find new cards.
      index[card] <- 1 # Denote 1 for all card in boxes that have been opened.
    }
  }
  sum(index)  # The number of time to find card k.
}

## Function dloop is to estimate the probability of each loop occurs at least once in a random shuffling of cards to boxes.
# Input: n, nreps(the number of replicate simulations).
# Output: the probability of each loop occurs at least once in a random shuffling of cards to boxes.
dloop <- function(n, nreps){
  sum_b <- rep(0,2*n)
  
  for (i in 1:nreps){
    loop_length <- rep(0,2*n)
    c <- rep(0,2*n)
    cards_sequence <- sample(1:(2*n), 2*n)  # A randomly generated sequence of cards with length 2n.
    for (j in 1:(2*n)){
      loop_length[j] <- times_to_find_kcard(n, j, cards_sequence)  # Times for prisoner j to find his card i.e. the loop length.
    }
    loop_length_index <- unique(loop_length)  # The indices of all lengths' loops.
    c[loop_length_index] <- 1  # Denote 1 for all lengths' loops.
    sum_b <- sum_b + c   # The number of loops of each length.
  }
  sum_b/nreps  # The probability of each loop occurs at least once.
}

##6
dloop(50,10000)



