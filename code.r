## 1
strategy1 <- function(n, k) {
  card <- k
  index <- rep(0, 2*n)
  index[k] <- 1
  
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

strategy2 <- function(n, k) {
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

strategy3 <- function(n, k) {
  cards = sample(1:(2*n))
  if (k %in% cards[1:n]) {1}
  else {0}
}


Pone <- function(n, k, strategy, nreps) {
  pass <- rep(0, nreps)
  if (strategy == 1) {
    for (i in 1:nreps) {
      pass[i] <- strategy1(n, k)
    }
  } else if (strategy == 2) {
    for (i in 1:nreps) {
      pass[i] <- strategy2(n, k)
    }
  } else {
    for (i in 1:nreps) {
      pass[i] <- strategy3(n, k)
    }
  }
  sum(pass)/nreps
}

Pone(5,1,1,10000)

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
