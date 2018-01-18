
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
colnames(X) <- c("x1", "x2")

# generate corresponding labels
y_truth <- c(rep(1, class_sizes[1]), rep(2, class_sizes[2]),rep(3, class_sizes[3]))
K <- max(y_truth)

## Exporting Data

# write data to a file
write.csv(x = cbind(X, y_truth), file = "hw1_data_set.csv", row.names = FALSE)
# read random variables
x1 <- X[,1]
x2 <- X[,2]

# plotting generated points
plot(points1[,1], points1[,2], type = "p", pch = 19, col = "red", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")
points(points2[,1], points2[,2], type = "p", pch = 19, col = "green")
points(points3[,1], points3[,2], type = "p", pch = 19, col = "blue")

# calculate sample means (x1,x2 separately)
sample_meansx1 <- sapply(1:K, FUN = function(c) mean(x1[y_truth == c]))
sample_meansx2 <- sapply(1:K, FUN = function(c) mean(x2[y_truth == c]))
sample_means <- t(matrix(c(sample_meansx1, sample_meansx2),3,2))

# calculate the covariance matrix
sample_cov_matrix <- array (c(cov(points1),cov(points2), cov(points3)), c(2,2,3))



# calculate covariance determinants
cov_determinants <- sapply(1:K, FUN = function(c) det(sample_cov_matrix[,,c]))

# calculate prior probabilities
class_priors <- sapply(X = 1:K, FUN = function(c) mean(y_truth == c))

# define grid
x1_interval <- seq(from = -6, to = +6, by = 0.06)
x2_interval <- seq(from = -6, to = +6, by = 0.06)
x1_grid <- matrix(x1_interval, nrow = length(x1_interval), ncol = length(x1_interval), byrow = FALSE)
x2_grid <- matrix(x2_interval, nrow = length(x2_interval), ncol = length(x2_interval), byrow = TRUE)

# define score function
f <- function(x1,x2,c) {(- 0.5 * log(cov_determinants[c]) 
                        - 0.5 * (t(c(x1,x2) - sample_means[,c]) %*% inv(sample_cov_matrix[,,c]) %*% (c(x1,x2) - sample_means[,c]))
                        + log(class_priors[c]))}

# apply score function over the whole grid
score_values = array(0,c(nrow(x1_grid),ncol(x1_grid),K))

for (c in 1:K){
score_values[,,c] <- matrix(mapply(f, x1_grid, x2_grid,c), nrow(x2_grid), ncol(x2_grid))
}

# use the score values to assign classes
assignmentScores <- matrix(c(score_values),c(nrow(x2_grid)*ncol(x2_grid),K))
class_assignments <- apply(X = assignmentScores, MARGIN = 1, FUN = which.max)

# calculate score distribution for generated points
dataScores <- array(0,c(length(x1),K))
for (c in 1:K){
  dataScores[,c] <- matrix(c(mapply(f,x1,x2,c)),c(nrow(x1),K))
}

# calculate confusion matrix
y_predicted <- apply(X = dataScores, MARGIN = 1, FUN = which.max)
confusion_matrix <- table(y_predicted, y_truth)
print(confusion_matrix)


#plotting the classification
plot(X[y_truth == 1, 1], X[y_truth == 1, 2], type = "p", pch = 19, col = "red",
     xlim = c(-6, +6),
     ylim = c(-6, +6),
     xlab = "x1", ylab = "x2", las = 1)
points(X[y_truth == 2, 1], X[y_truth == 2, 2], type = "p", pch = 19, col = "green4")
points(X[y_truth == 3, 1], X[y_truth == 3, 2], type = "p", pch = 19, col = "blue")

points(X[y_predicted != y_truth, 1], X[y_predicted != y_truth, 2], cex = 1.5, lwd = 2)

points(x1_grid[class_assignments == 1],x2_grid[class_assignments==1], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.01), pch = 16)
points(x1_grid[class_assignments == 2],x2_grid[class_assignments==2], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.01), pch = 16)
points(x1_grid[class_assignments == 3],x2_grid[class_assignments==3], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.01), pch = 16)

class_assignments_grid = array(c(class_assignments),c(length(x1_interval),length(x2_interval)))
contour(x1_interval, x2_interval,class_assignments_grid, nlevels =3 , add = TRUE, lwd = 2.5, drawlabels = FALSE)


