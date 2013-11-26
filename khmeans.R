## ToDo:
## 1. vectorize
## 2. multi-core
## 3. code in Rcpp
## 4. create package

khmeans <- function(x, centers, max.iter, nstart) {
  
  ## ToDo: add pre-conditions
  
  centers <- as.matrix(x[sample(1:nrow(x), centers), ])
  labels <- rep(0, nrow(x))
  
  K <- nrow(centers)
  N <- nrow(x)
  d <- q <- matrix(0, K, N)
  
  for (run in 1:max.iter) {
    for (i in 1:K) {
      for (j in 1:N) {
        d[i,j] <- sqrt(sum((centers[i,] - x[j,])^2))
      }
    }
    labels <- apply(d, 2, which.min) ## get the cluster labels
    d <- d + 1e-6                    ## avoid numerical issues
    
    ## calculate weights
    for (i in 1:K) {
      for (j in 1:N) {
        q[i,j] <- (d[i,j]^3)*((sum(1/d[,j]^2))^2)
      }
    }
    
    ## calculate weighted mean
    for (i in 1:K) {      
      centers[i,] <- colSums(q[i,labels == i]*x[labels == i,]) / sum(q[i,labels == i])
    }    
        
  }
  
  return(list(cluster=labels, centers=centers))  
}

