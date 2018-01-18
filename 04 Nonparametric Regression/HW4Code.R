# read data into memory
rm(list=ls(all=TRUE))
data_set <- read.csv("hw04_data_set.csv")
set.seed(521)

#get x and y values
x <- data_set$x
y <- data_set$y

#separate train and test data
train_indices <-  c(sample(which(x==x),100)) 
x_train <- x[train_indices]
y_train <- y[train_indices]
x_test <- x[-train_indices]
y_test <- y[-train_indices]

point_colors <- c("red", "green", "blue")
minimum_value <- -0
maximum_value <- +60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

#Regressogram
bin_width <- 3
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)
g_head <- sapply(1:length(left_borders), function(b) (
                  sum(y_train[left_borders[b] < x_train & x_train <= right_borders[b]])
                 /sum(left_borders[b] < x_train & x_train <= right_borders[b]))) 

#plot regressogram
plot(x_train, y_train, type = "p", pch = 19, col = "blue",
     ylim = c(min(y_train),max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 19, col = "red")
legend("topright" ,c("training","test"), col=c("blue","red"), pch=19)
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(g_head[b], g_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(g_head[b], g_head[b + 1]), lwd = 2, col = "black") 
  }
}

#calculating RMSE for regressogram
RMSE_bins <- sapply(1:length(left_borders), function(b) {
  y_test[left_borders[b] < x_test & x_test <= right_borders[b]] - g_head[b]}^2) 

RMSE<- sqrt(sum(array(unlist(RMSE_bins)) / length(y_test)))
sprintf("Regressogram => RMSE is %f when h is %i", RMSE, bin_width)


#Running Mean Smoother estimation
bin_width2 <- 3
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width2, by = bin_width2)
right_borders <- seq(from = minimum_value + bin_width2, to = maximum_value, by = bin_width2)
g_head <- sapply(data_interval, function(X) {sum(y_train[(X - 0.5 * bin_width2) < x_train & x_train <= (X + 0.5 * bin_width2)])}
                 /sum((X - 0.5 * bin_width2) < x_train & x_train <= (X + 0.5 * bin_width2))) 

#plot running mean smoother
plot(x_train, y_train, type = "p", pch = 19, col = "blue",
     ylim = c(min(y_train),max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width2))
points(x_test, y_test, type = "p", pch = 19, col = "red")
legend("topright" ,c("training","test"), col=c("blue","red"), pch=19)
lines(data_interval, g_head, type = "l", lwd = 2, col = "black")

#RMSE for Running Mean Smoother
RMSE2raw <- sapply(x_test, function(X) {sum(y_train[(X - 0.5 * bin_width2) < x_train & x_train <= (X + 0.5 * bin_width2)])}
                   /sum((X - 0.5 * bin_width2) < x_train & x_train <= (X + 0.5 * bin_width2))) 

RMSE2 <- ((RMSE2raw-y_test)^2)

RMSE_value2 <- sqrt(sum(RMSE2/length(y_test)))
sprintf("Running Mean Smoother => RMSE is %f when h is %i", RMSE_value2, bin_width2)


#kernel estimation
bin_width3 <- 1

#kernel function
kernel <- function(u) {
  return (1 / sqrt(2 * pi) * exp(-0.5 * (u^2)))
}

g_head <- sapply(data_interval, function(X) (
                  sum(y_train*kernel((X-x_train)/bin_width3))
                 /sum(kernel((X-x_train)/bin_width3))
    )) 

#plot kernel smoother
plot(x_train, y_train, type = "p", pch = 19, col = "blue",
     ylim = c(min(y_train),max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 19, col = "red")
legend("topright" ,c("training","test"), col=c("blue","red"), pch=19)
lines(data_interval, g_head, type = "l", lwd = 2, col = "black")

#RMSE for Kernel Smoother
RMSE3raw <- sapply(x_test, function(X) (
  sum(y_train*kernel((X-x_train)/bin_width3))
  /sum(kernel((X-x_train)/bin_width3)))) 
  
RMSE3 <- ((RMSE3raw - y_test)^2)

RMSE_value3 <- sqrt(sum(RMSE3)/length(y_test))

#print RMSE values for all classifiers
sprintf("Regressogram => RMSE is %f when h is %i", RMSE, bin_width)
sprintf("Running Mean Smoother => RMSE is %f when h is %i", RMSE_value2, bin_width2)
sprintf("Kernel Smoother => RMSE is %f when h is %i", RMSE_value3, bin_width3)

