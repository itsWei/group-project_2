strategy1 <- function(n, k) {
  index <- rep(0, 2*n)
  index[k] <- 1
  
  S <- sample(1:(2*n), 2*n)
  for (i in 1:(n+1)) {
    if (S[k] == k) break
    else {
      k <- S[k]
      index[k] <- 1
    }
  }
  
  if (sum(index) <= n) {1}
  else {0}
}

strategy2 <- function(n) {
  k <- sample(1:(2*n), 1)
  index <- rep(0, 2*n)
  index[k] <- 1
  
  S <- sample(1:(2*n), 2*n)
  for (i in 1:(n+1)) {
    if (S[k] == k) break
    else {
      k <- S[k]
      index[k] <- 1
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
      pass[i] <- strategy2(n)
    }
  } else {
    for (i in 1:nreps) {
      pass[i] <- strategy3(n, k)
    }
  }
  sum(pass)/nreps
}

Pone(5,1,3,10000)