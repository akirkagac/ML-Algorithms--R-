
library(MASS)
library(matlib)
rm(list=ls(all=TRUE))

## Parameters
set.seed(521)
# mean parameters
class_means <- matrix(c(0, +1.5, -2.5, -3.0, +2.5,-3.0), 2, 3)
# covariance parameters
class_covariances <- array(c(+1.0, +0.2, +0.2, +3.2,
                             +1.6, -0.8, -0.8, +1.0,
                             +1.6, +0.8, +0.8, +1.0), c(2, 2, 3))
# sample sizes
class_sizes <- c(100,100,100)

## Data Generation
# generate random samples
points1 <- mvrnorm(n = class_sizes[1], mu = class_means[,1], Sigma = class_covariances[,,1])
points2 <- mvrnorm(n = class_sizes[2], mu = class_means[,2], Sigma = class_covariances[,,2])
points3 <- mvrnorm(n = class_sizes[3], mu = class_means[,3], Sigma = class_covariances[,,3])
X <- rbind(points1, points2,points3 )

# generate truth (however, this will not be used)
y_truth <- c(rep(1, class_sizes[1]), rep(2, class_sizes[2]),rep(3, class_sizes[3]))
K <- max(y_truth)

#generate binary truth table for our operations
y_BinaryTruth = array(0,c((class_sizes[1]+class_sizes[2]+class_sizes[3]),K))
y_BinaryTruth[,1] <- c(rep(1,class_sizes[1]), rep(0,(class_sizes[2]+class_sizes[3])))
y_BinaryTruth[,2] <- c(rep(0,class_sizes[1]), rep(1,class_sizes[2]),rep(0,class_sizes[3]))
y_BinaryTruth[,3] <- c(rep(0,class_sizes[1]+class_sizes[2]), rep(1,class_sizes[3]))

#visualize points
plot(points1[,1], points1[,2], type = "p", pch = 19, col = "red", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")
points(points2[,1], points2[,2], type = "p", pch = 19, col = "green")
points(points3[,1], points3[,2], type = "p", pch = 19, col = "blue")

# define grid
x1_interval <- seq(from = -6, to = +6, by = 0.03)
x2_interval <- seq(from = -6, to = +6, by = 0.03)
x1_grid <- matrix(x1_interval, nrow = length(x1_interval), ncol = length(x1_interval), byrow = FALSE)
x2_grid <- matrix(x2_interval, nrow = length(x2_interval), ncol = length(x2_interval), byrow = TRUE)


#define softmax denominator
softmaxDen <- function(X,w,w0){
  return (exp(X%*%(w[,1])+w0[1])+exp(X%*%(w[,2])+w0[2])+exp(X%*%(w[,3])+w0[3]))
}
#define softmax function
softmaxFcn <- function(X,w,w0){
  denominator <- softmaxDen(X,w,w0)
  softmaxClass1 <- (exp(X%*%(w[,1])+w0[1]))/(denominator)
  softmaxClass2 <- (exp(X%*%(w[,2])+w0[2]))/(denominator)
  softmaxClass3 <- (exp(X%*%(w[,3])+w0[3]))/(denominator)
  
  softmaxOutput <- cbind(softmaxClass1,softmaxClass2,softmaxClass3)
  return (softmaxOutput)
}
  

# define the gradient functions
gradient_w <- function(x, y_BinaryTruth, y_predicted) {
  gradient_w1 <- colSums(matrix(y_BinaryTruth[,1] - y_predicted[,1], nrow = nrow(x), ncol = ncol(x), byrow = FALSE)*x)
  gradient_w2 <- colSums(matrix(y_BinaryTruth[,2] - y_predicted[,2], nrow = nrow(x), ncol = ncol(x), byrow = FALSE)*x)
  gradient_w3 <- colSums(matrix(y_BinaryTruth[,3] - y_predicted[,3], nrow = nrow(x), ncol = ncol(x), byrow = FALSE)*x)
  combinedGr <- cbind(gradient_w1,gradient_w2,gradient_w3)
  return (combinedGr)
}

gradient_w0 <- function(y_BinaryTruth, y_predicted) {
  gradient_w01<- sum(y_BinaryTruth[,1] - y_predicted[,1])
  gradient_w02<- sum(y_BinaryTruth[,2] - y_predicted[,2])
  gradient_w03<- sum(y_BinaryTruth[,3] - y_predicted[,3])
  combinedGr0 <- cbind(gradient_w01,gradient_w02,gradient_w03)
  return (combinedGr0)
}
 set.seed(521)

# set learning parameters
eta <- 0.01
epsilon <- 1e-3

# randomly initalize w and w0 (ncol guarantees the correct number of parameters)

w <- replicate (K,runif(ncol(X),min= -.01, max = .01))
w0 <- replicate (K,runif(1,min=-.01, max =.01))

# learn w and w0 using gradient descent
iteration <- 1
objective_values <- c()
while (1) {
  print(paste0("running iteration#", iteration))
  y_predicted <- softmaxFcn(X, w, w0)
  
  objective_values <- c(objective_values, -sum(y_BinaryTruth * log(y_predicted + 1e-100) + (1 - y_BinaryTruth) * log(1 - y_predicted + 1e-100)))
  
  w_old <- w
  w0_old <- w0
  
  w <- w + eta * gradient_w(X, y_BinaryTruth, y_predicted)
  w0 <- w0 + eta * gradient_w0(y_BinaryTruth, y_predicted)
  
  if (sqrt(sum((w0 - w0_old)^2)+sum((w - w_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}
print(w)
print(w0)

# plot objective function during iterations
plot(1:iteration, objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

#print confusion matrix
y_predictions <- apply(X = y_predicted, MARGIN = 1, FUN = which.max)
confusion_matrix <- table(y_predictions, y_truth)
print(confusion_matrix)

gridAssignments = array(0,c(nrow(x1_grid),ncol(x1_grid),K))

#since the assignments are done, we do not need to call for a softmax function again, highest regression value will yield 
#highest softmax value, since the denominator will be the same for all three classes.
gridSoftmax <- function(x1,x2,c) {(w[1,c] * x1 + w[2,c] * x2 + w0[c])}
for (c in 1:K){
gridAssignments[,,c]  <- matrix(mapply(gridSoftmax, x1_grid, x2_grid,c), nrow(x2_grid), ncol(x2_grid))
}
assignmentScores <- matrix(c(gridAssignments),c(nrow(x2_grid)*ncol(x2_grid),K)) #decomposition from grid structure, in order to apply which.max
class_assignments <- apply(X = assignmentScores, MARGIN = 1, FUN = which.max)
class_assignments_grid = array(c(class_assignments),c(length(x1_interval),length(x2_interval))) #again converting the output to a grid structure


#plot points
plot(X[y_truth == 1, 1], X[y_truth == 1, 2], type = "p", pch = 19, col = "red",
     xlim = c(-6, +6),
     ylim = c(-6, +6),
     xlab = "x1", ylab = "x2", las = 1)
points(X[y_truth == 2, 1], X[y_truth == 2, 2], type = "p", pch = 19, col = "green4")
points(X[y_truth == 3, 1], X[y_truth == 3, 2], type = "p", pch = 19, col = "blue")

#indicate wrong classifications
points(X[y_predictions != y_truth, 1], X[y_predictions != y_truth, 2], cex = 1.5, lwd = 2)

#sketch grid according to assignments
points(x1_grid[class_assignments == 1],x2_grid[class_assignments==1], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.015), pch = 16)
points(x1_grid[class_assignments == 2],x2_grid[class_assignments==2], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.015), pch = 16)
points(x1_grid[class_assignments == 3],x2_grid[class_assignments==3], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.015), pch = 16)

#divide the plot into three distinct areas
contour(x1_interval, x2_interval,class_assignments_grid, nlevels =3 , add = TRUE, lwd = 2.5, drawlabels = FALSE)


