
library(MASS)
rm(list=ls(all=TRUE))
set.seed(521)
# mean parameters
class_means <- matrix(c(+2.0, +2.0,
                        -4.0, -4.0,
                        -2.0, +2.0,
                        +4.0, -4.0,
                        -2.0, -2.0,
                        +4.0, +4.0,
                        +2.0, -2.0,
                        -4.0, +4.0), 2, 8)
# covariance parameters
class_covariances <- array(c(+0.8, -0.6, -0.6, +0.8,
                             +0.4, +0.0, +0.0, +0.4,
                             +0.8, +0.6, +0.6, +0.8,
                             +0.4, +0.0, +0.0, +0.4,
                             +0.8, -0.6, -0.6, +0.8,
                             +0.4, +0.0, +0.0, +0.4,
                             +0.8, +0.6, +0.6, +0.8,
                             +0.4, +0.0, +0.0, +0.4), c(2, 2, 8))
# sample sizes
class_sizes <- c(50,50,50,50,50,50,50,50)

# generate random samples
points1 <- mvrnorm(n = class_sizes[1] , mu = class_means[,1], Sigma = class_covariances[,,1])
points2 <- mvrnorm(n = class_sizes[2] , mu = class_means[,2], Sigma = class_covariances[,,2])
points3 <- mvrnorm(n = class_sizes[3] , mu = class_means[,3], Sigma = class_covariances[,,3])
points4 <- mvrnorm(n = class_sizes[4] , mu = class_means[,4], Sigma = class_covariances[,,4])
points5 <- mvrnorm(n = class_sizes[5] , mu = class_means[,5], Sigma = class_covariances[,,5])
points6 <- mvrnorm(n = class_sizes[6] , mu = class_means[,6], Sigma = class_covariances[,,6])
points7 <- mvrnorm(n = class_sizes[7] , mu = class_means[,7], Sigma = class_covariances[,,7])
points8 <- mvrnorm(n = class_sizes[8] , mu = class_means[,8], Sigma = class_covariances[,,8])
X <- rbind(points1, points2, points3, points4, points5, points6, points7, points8)
colnames(X) <- c("x1", "x2")

# generate corresponding labels
y_truth <- c(rep(1, class_sizes[1]+class_sizes[2]),rep(2, class_sizes[3]+class_sizes[4]),
             rep(3, class_sizes[5]+class_sizes[6]),rep(4, class_sizes[7]+class_sizes[8]))
K <- max(y_truth)



y_BinaryTruth = array(0,c((class_sizes[1]+class_sizes[2]+class_sizes[3]+class_sizes[4]+
                             class_sizes[5]+class_sizes[6]+class_sizes[7]+class_sizes[8]),K))

y_BinaryTruth[,1] <- c(rep(1,class_sizes[1]+class_sizes[2]), rep(0,(class_sizes[3]+class_sizes[4]+
                                                                      class_sizes[5]+class_sizes[6]+class_sizes[7]+class_sizes[8])))
y_BinaryTruth[,2] <- c(rep(0,class_sizes[1]+class_sizes[2]), rep(1,class_sizes[3]+class_sizes[4]),
                       rep(0,class_sizes[5]+class_sizes[6]+class_sizes[7]+class_sizes[8]))
y_BinaryTruth[,3] <- c(rep(0,class_sizes[1]+class_sizes[2]+class_sizes[3]+class_sizes[4]), 
                       rep(1,class_sizes[5]+class_sizes[6]),rep(0,class_sizes[7]+class_sizes[8]))
y_BinaryTruth[,4] <- c(rep(0,class_sizes[1]+class_sizes[2]+class_sizes[3]+class_sizes[4]+class_sizes[5]+
                             class_sizes[6]),rep(1,class_sizes[7]+class_sizes[8]))


# write data to a file
write.csv(x = cbind(X, y_BinaryTruth), file = "lab04_data_set.csv", row.names = FALSE)

# plot data points generated
plot(points1[,1], points1[,2], type = "p", pch = 19, col = "red", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")
points(points2[,1], points2[,2], type = "p", pch = 19, col = "red")
points(points3[,1], points3[,2], type = "p", pch = 19, col = "green")
points(points4[,1], points4[,2], type = "p", pch = 19, col = "green")
points(points5[,1], points5[,2], type = "p", pch = 19, col = "blue")
points(points6[,1], points6[,2], type = "p", pch = 19, col = "blue")
points(points7[,1], points7[,2], type = "p", pch = 19, col = "purple")
points(points8[,1], points8[,2], type = "p", pch = 19, col = "purple")

N <- length(y_truth)
D <- ncol(X)

# define the sigmoid function
sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}

softmax <- function(Z,v){
  y_raw <- cbind(1, Z) %*% v
  y_predicted <- matrix(0,nrow(y_raw),ncol(y_raw))
  
  for (t in 1: nrow(y_raw)){
    y_predicted[t,] <- exp(y_raw[t,]) / sum(exp(y_raw[t,]))
  }
  return (y_predicted)
}

# set learning parameters
eta <- 0.1
epsilon <- 1e-3
H <- 20
max_iteration <- 200

# randomly initalize W and v
W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
v <- matrix(runif((H+1)*K, min = -0.01, max = 0.01), H+1,K)
Z <- sigmoid(cbind(1, X) %*% W)
y_predicted <- matrix(0,nrow=nrow(y_BinaryTruth),ncol=ncol(y_BinaryTruth))
y_predicted <- softmax(Z,v)
objective_values <- -sum(y_BinaryTruth * log(y_predicted + 1e-100))


# learn W and v using gradient descent and online learning
iteration <- 1
while (1) {
  for (i in sample(N)) {
    # calculate hidden nodes
    Z[i,] <- sigmoid(c(1, X[i,]) %*% W)
    # calculate output node
    y_predicted[i,] <- softmax(matrix(Z[i,],1,H),v)
    
    update_k = matrix(0,nrow(v),ncol(v))
    for (k in 1:K){
      update_k[,k] <- eta * (y_BinaryTruth[i,k] - y_predicted[i,k]) * c(1, Z[i,])
    }
    
    update_h = matrix(0,nrow(W),ncol(W))
    for (h in 1:H) {
      intermediate <- 0
      for (k in 1:K){
        intermediate <- intermediate + (y_BinaryTruth[i,k] - y_predicted[i,k]) * v[h,k]
      }
      update_h[,h] <- eta * intermediate * Z[i, h] * (1 - Z[i, h]) * c(1, X[i,])
    }
    
    v <- v + update_k
    W <- W + update_h 
    
  }
  
  Z <- sigmoid(cbind(1, X) %*% W)
  y_predicted <- softmax(Z,v)
  objective_values <- c(objective_values, -sum(y_BinaryTruth * log(y_predicted + 1e-100)))
  
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
    break
  }
    iteration <- iteration + 1
    print(iteration)
}
print(W)
print(v)

# plot objective function during iterations
plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

# calculate confusion matrix
y_predictions <- apply(X = y_predicted, MARGIN = 1, FUN = which.max)
confusion_matrix <- table(y_predictions, y_truth)
print(confusion_matrix)

# evaluate discriminat function on a grid
x1_interval <- seq(from = -6, to = +6, by = 0.06)
x2_interval <- seq(from = -6, to = +6, by = 0.06)
x1_grid <- matrix(x1_interval, nrow = length(x1_interval), ncol = length(x1_interval), byrow = FALSE)
x2_grid <- matrix(x2_interval, nrow = length(x2_interval), ncol = length(x2_interval), byrow = TRUE)

f <- function(x1, x2) {
  new <- c(1, sigmoid(c(1, x1, x2) %*% W)) %*% v 
  scoreMax <- apply(X = new, MARGIN = 1, FUN = which.max)
  return (scoreMax)
}
class_assignments <- matrix(mapply(f, x1_grid, x2_grid), nrow(x2_grid), ncol(x2_grid))

#plot points
plot(X[y_truth == 1, 1], X[y_truth == 1, 2], type = "p", pch = 19, col = "red",
     xlim = c(-6, +6),
     ylim = c(-6, +6),
     xlab = "x1", ylab = "x2", las = 1)
points(X[y_truth == 2, 1], X[y_truth == 2, 2], type = "p", pch = 19, col = "green")
points(X[y_truth == 3, 1], X[y_truth == 3, 2], type = "p", pch = 19, col = "blue")
points(X[y_truth == 4, 1], X[y_truth == 4, 2], type = "p", pch = 19, col = "purple")

#indicate wrong classifications
points(X[y_predictions != y_truth, 1], X[y_predictions != y_truth, 2], cex = 1.5, lwd = 2)

#sketch grid according to assignments
points(x1_grid[class_assignments == 1],x2_grid[class_assignments==1], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.03), pch = 16)
points(x1_grid[class_assignments == 2],x2_grid[class_assignments==2], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.03), pch = 16)
points(x1_grid[class_assignments == 3],x2_grid[class_assignments==3], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.03), pch = 16)
points(x1_grid[class_assignments == 4],x2_grid[class_assignments==4], col = rgb(red = 1, green = 0, blue = 1, alpha = 0.03), pch = 16)
