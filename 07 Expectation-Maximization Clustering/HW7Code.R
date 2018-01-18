library(MASS)
library(mixtools)
rm(list=ls(all=TRUE))
set.seed(521)
# mean parameters
class_means <- matrix(c(+2.5, +2.5, -2.5, +2.5,-2.5, -2.5,+2.5, -2.5, 0.0, 0.0),2,5)
# covariance parameters
class_covariances <- array(c(+0.8, -0.6, -0.6, +0.8,
                             +0.8, +0.6, +0.6, +0.8,
                             +0.8, -0.6, -0.6, +0.8,
                             +0.8, +0.6, +0.6, +0.8,
                             +1.6, -0.0, -0.0, +1.6), c(2, 2, 5))
# sample sizes
class_sizes <- c(50,50,50,50,100)

# generate random samples
points1 <- mvrnorm(n = class_sizes[1], mu = class_means[,1], Sigma = class_covariances[,,1])
points2 <- mvrnorm(n = class_sizes[2], mu = class_means[,2], Sigma = class_covariances[,,2])
points3 <- mvrnorm(n = class_sizes[3], mu = class_means[,3], Sigma = class_covariances[,,3])
points4 <- mvrnorm(n = class_sizes[4], mu = class_means[,4], Sigma = class_covariances[,,4])
points5 <- mvrnorm(n = class_sizes[5], mu = class_means[,5], Sigma = class_covariances[,,5])

X <- rbind(points1, points2, points3, points4, points5)

# plot data points
plot(points1[,1], points1[,2], type = "p", pch = 19, col = "black", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")
points(points2[,1], points2[,2], type = "p", pch = 19, col = "black")
points(points3[,1], points3[,2], type = "p", pch = 19, col = "black")
points(points4[,1], points4[,2], type = "p", pch = 19, col = "black")
points(points5[,1], points5[,2], type = "p", pch = 19, col = "black")

centroids <<- NULL
assignments <<- NULL
K <- 5
N <- length(class_sizes)
numOfPoints <- sum(class_sizes)


#k-Means step
km_iteration <- 2

for(i in 1:km_iteration ){
  
  if (is.null(centroids) == TRUE) {
    centroids <- X[sample(1:numOfPoints, K),]
  } 
  else {
    for (k in 1:K) {
      centroids[k,] <- colMeans(X[assignments == k,])
    }  
  }
  
  D <- as.matrix(dist(rbind(centroids, X), method = "euclidean"))
  D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(X))]
  assignments <<- sapply(1:ncol(D), function(h) {which.min(D[,h])})

}



#initialize em
# calculate prior probabilities
class_priors <- sapply(X = 1:K, FUN = function(c) {mean(assignments == c)})

# calculate sample covariances
tempcov<- 0*matrix(0,4,K)
sample_covariances<- 0*class_covariances

centroids <- t(centroids)
a <- 0
for (i in 1:2){
  v <- X[,i]
  for (j in 1:2){
    u <- X[,j]
    a <- a+1
    tempcov[a,]<- sapply(X = 1:K, FUN = function(c) {mean((v[assignments == c] - 
                      centroids[i,c])*(u[assignments == c] - centroids[j,c]))})
    
  }}
for (i in 1:K){
  sample_covariances[,,i] <-matrix(tempcov[,i],2,2)
}


#em algorithm

em_iterations <- 100
G <- matrix(0,numOfPoints,K)

for (d in 1:em_iterations){
  
#E-step
  sumG <- matrix(0,sum(class_sizes))
  for (i in 1:numOfPoints){
    for ( j in 1:K){
      
      G[i,j] <- class_priors[j] * ((det(sample_covariances[,,j]))^(-0.5))%*%
        exp((-0.5)*t(X[i,]- centroids[,j])%*% solve(sample_covariances[,,j])%*%(X[i,]- centroids[,j]))
      
      sumG[i] <- sumG[i]+G[i,j]
    }
  }
  
  H=matrix(0,numOfPoints,K)
    for ( j in 1:K){
      H[,j] <- G[,j]/sumG
    }

  
#M-step  
  for (i in 1:K){
    centroids[,i] <- t(X)%*%H[,i]/sum(H[,i])
  }
  
  for (i in 1:K){
    c_var=matrix(0,2,2)
    for (j in 1:numOfPoints){
      c_var=c_var+H[j,i]*(X[j,]-centroids[,i])%*%t(X[j,]-centroids[,i])
    }
    sample_covariances[,,i] <- c_var/sum(H[,i])
  }
  
  class_priors <- colSums(H)/numOfPoints
  
}

Hmax=max.col(H)
colors=c("darkorange","red3", "green4", "#4c93b5", "purple3")  
plot(X[,1][Hmax==1], X[,2][Hmax==1], type = "p", pch = 19, col = colors[1], las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")

for (i in 2:K){
  points(X[,1][Hmax==i], X[,2][Hmax==i], type = "p", pch = 19, col = colors[i])
}

centroids <- t(centroids)
centroids

for(i in 1:N){
  ellipse(class_means[,i], class_covariances[,,i], alpha = .05, npoints = class_sizes[i], newplot = FALSE, draw = TRUE, lty=2, lwd=2)
  ellipse(centroids[i,], sample_covariances[,,i], alpha = .05, npoints = class_sizes[i], newplot = FALSE, draw = TRUE, lwd=2)
}
