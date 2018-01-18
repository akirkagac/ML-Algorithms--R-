rm(list=ls(all=TRUE))
# read data into memory
training_digits <- read.csv("hw06_mnist_training_digits.csv", header = FALSE)
training_labels <- read.csv("hw06_mnist_training_labels.csv", header = FALSE)
test_digits <- read.csv("hw06_mnist_test_digits.csv", header = FALSE)
test_labels <- read.csv("hw06_mnist_test_labels.csv", header = FALSE)

# get X and y values
X_train <- as.matrix(training_digits)/255
y_train <- training_labels[,1]
X_test <- as.matrix(test_digits)/255
y_test <- test_labels[,1]

# get number of samples, number of features, and number of classes
N <- length(y_train)
D <- ncol(X_train)
U <- length(unique(y_train))

Mode <- function(x) {
  if (is.numeric(x)) {
    x_table <- table(x)
    return(as.numeric(names(x_table)[which.max(x_table)]))
  }
}

submatrix <- matrix(0,N/10,D)
withinClass = array(0,dim = c(D,D,10))
betweenClass = array(0,dim = c(D,D,10))
digitMeans = array(0,dim = c(10,D))

totalMean <- colMeans (X_train)

for (i in 1:U){

#extracting a matrix for each class  
submatrix = X_train[y_train==i,]
digitMeans[i,] = colMeans (submatrix)

withinClass[,,i] = t(as.matrix(submatrix - matrix(digitMeans[i,], N/10, D, byrow = TRUE))) %*% 
                  as.matrix(submatrix - matrix(digitMeans[i,], N/10, D, byrow = TRUE))

betweenClass[,,i] = as.matrix(digitMeans[i,] - totalMean) %*% t(as.matrix(digitMeans[i,] - totalMean))
}

# adding 3rd dimensions together to reach final values
withinMatrix = rowSums(withinClass, dims = 2)
betweenMatrix = 50*rowSums(betweenClass, dims = 2)

# avoid singularity
diag(withinMatrix) <- diag(withinMatrix) + 1e-10

# find the matrix which will yield eigenvalues and eigenvectors
eigenMatrix <- chol2inv(chol(withinMatrix)) %*% betweenMatrix

# calculate the eigenvalues and eigenvectors
decomposition <- eigen(eigenMatrix, symmetric = TRUE)


# plot scree graph
plot(1:D, decomposition$values, 
     type = "l", las = 1, lwd = 2,
     xlab = "Eigenvalue index", ylab = "Eigenvalue")

# calculate two-dimensional training projections
Z <- (X_train - matrix(totalMean, N, D, byrow = TRUE)) %*% decomposition$vectors[,1:2]

# plot two-dimensional training projections
point_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")
plot(Z[,1], Z[,2], type = "p", pch = 19, col = point_colors[y_train], cex = 0,
     main = "Training Points",
     xlab = "Dimension 1", ylab = "Dimension 2", las = 1)
text(Z[,1], Z[,2], labels = y_train %% 10, col = point_colors[y_train])

# calculate two-dimensional test projections
Z_test <- (X_test - matrix(totalMean, N, D, byrow = TRUE)) %*% decomposition$vectors[,1:2]

# plot two-dimensional test projections
point_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")
plot(Z_test[,1], Z_test[,2], type = "p", pch = 19, col = point_colors[y_test], cex = 0,
     main = "Test Points",
     xlab = "Dimension 1", ylab = "Dimension 2", las = 1)
text(Z_test[,1], Z_test[,2], labels = y_test %% 10, col = point_colors[y_test])




#low-dimensional projections from 1 to 9
testdistance = matrix(0,N,N)
classValue = matrix(0,N,1)
accuracy = matrix (0,9,1)

for (R in 1:9){
  
  Z_lowTrain <- (X_train - matrix(totalMean, N, D, byrow = TRUE)) %*% decomposition$vectors[,1:R]
  Z_lowTest <- (X_test - matrix(totalMean, N, D, byrow = TRUE)) %*% decomposition$vectors[,1:R]
  
  for (ntest in 1:N){
    for(ntrain in 1:N){
      testdistance[ntest,ntrain] = sqrt(sum((Z_lowTest[ntest,]-Z_lowTrain[ntrain,])^2))
    }
    
  #find 5-nearest neighbors and their classes to classify test points
   indexMatrix =  order(testdistance[ntest,])
   knnMatrix = indexMatrix[1:5]
   classValue[ntest] = Mode(y_test[knnMatrix])  
  }
  
  accuracy[R] = (1/N)*sum(y_test == classValue)
  
}

# plot accuracy
plot(1:9,accuracy, type="b",ylab = "Classification Accuracy (%)", xlab = "R", pch=1,lwd=2)

