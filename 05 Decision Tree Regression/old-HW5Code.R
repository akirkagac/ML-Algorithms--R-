# read data into memory
rm(list=ls(all=TRUE))
data_set <- read.csv("hw05_data_set.csv")
set.seed(521)

#get x and y values
X <- data_set$x
y <- data_set$y

#since we have only one column for x, D is equal to 1.

#separate train and test data
train_indices <-  c(sample(which(X==X),100)) 
X_train <- X[train_indices]
y_train <- y[train_indices]
X_test <- X[-train_indices]
y_test <- y[-train_indices]

minimum_value <- -0
maximum_value <- +60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)


# get numbers of train and test samples
N_train <- length(y_train)
N_test <- length(y_test)



##########################################################################################################
# create necessary data structures
P<-10
node_indices <- list()
is_terminal <- c()
need_split <- c()
currentMean <- c()
node_splits <- c() 

# put all training instances into the root node
node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)

# learning algorithm
while (1) {
  # find nodes that need splitting
  split_nodes <- which(need_split)
  # check whether we reach all terminal nodes
  if (length(split_nodes) == 0) {
    break
  }
  # find best split positions for all nodes
  for (split_node in split_nodes) {
    data_indices <- node_indices[[split_node]]
    need_split[split_node] <- FALSE
    best_score <- rep(0, 1)  #arbitrary choice to define or not since they are scalars.
    best_split <- rep(0, 1)
    
    unique_values <- sort(unique(X_train[data_indices]))
    if(length(unique_values)==1){
      is_terminal[split_node] <- TRUE
      need_split[split_node] <- FALSE
      node_splits[split_node]<- unique_values[1]
      currentMean[split_node] <- mean(y_train[data_indices])
    }
    else{
      split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
      split_scores <- rep(0, length(split_positions))
      
      for (s in 1:length(split_positions)) {
        left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
        right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
        
        impurityLeft <- (1/length(data_indices)) * sum((y_train[left_indices] - mean(y_train[left_indices]))^2)
        impurityRight <- (1/length(data_indices)) * sum((y_train[right_indices] - mean(y_train[right_indices]))^2)
        split_scores[s] <- impurityLeft + impurityRight
      }
      
      best_score <- min(split_scores)
      best_split <- split_positions[which.min(split_scores)]  
      node_splits[split_node] <- best_split
      
      # create left node using the selected split
      left_indices <- data_indices[which(X_train[data_indices] < best_split)]
      node_indices[[2 * split_node]] <- left_indices
      
      if(length(left_indices) <= P){
        is_terminal[2 * split_node] <- TRUE
        need_split[2 * split_node] <- FALSE
        currentMean[2 * split_node] <- mean(y_train[left_indices])
        node_splits[2 * split_node]<- mean(left_indices)
      }
      else{
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
      }
      
      #create right node
      right_indices <- data_indices[which(X_train[data_indices] >= best_split)]
      node_indices[[2 * split_node + 1]] <- right_indices
      
      if(length(right_indices) <= P){
        is_terminal[2 * split_node+1] <- TRUE
        need_split[2 * split_node +1] <- FALSE
        currentMean[2 * split_node+1] <- mean(y_train[right_indices])
        node_splits[2 * split_node+1]<- mean(right_indices)
      }
      else{
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      } 
    }
  }
}

# traverse tree for test data points
y_predicted <- rep(0, N_test)
for (i in 1:N_test) {
  index <- 1
  while (1) {
    if (is_terminal[index] == TRUE) {
      y_predicted[i] <- currentMean[index]
      break
    } else {
      if (X_test[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}

#plot
y_interval <- rep(0, length(data_interval))
for (i in 1:length(data_interval)) {
  index <- 1
  while (1) {
    if (is_terminal[index] == TRUE) {
      y_interval[i] <- currentMean[index]
      break
    } else {
      if (data_interval[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}

plot(X_train,y_train, ylab = "y", xlab = "x", pch=19, col="blue")
points(X_test,y_test, ylab = "y", xlab = "x", pch=19, col="red")
lines(data_interval,y_interval,type="l",lwd=2.5, col="black")
legend("topright" ,c("training","test"), col=c("blue","red"), pch=19)
title("P=10")

RMSE <- sqrt(sum((y_predicted-y_test)^2)/N_test)
print(paste0("RMSE is ", RMSE," when P is ", P))
##########################################################################################################
RMSEs <- c()
for (P in 1:20){
  # create necessary data structures
  
  node_indices <- list()
  is_terminal <- c()
  need_split <- c()
  currentMean <- c()
  node_splits <- c() 
  
  # put all training instances into the root node
  node_indices <- list(1:N_train)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  while (1) {
    # find nodes that need splitting
    split_nodes <- which(need_split)
    # check whether we reach all terminal nodes
    if (length(split_nodes) == 0) {
      break
    }
    # find best split positions for all nodes
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      best_score <- rep(0, 1)  #arbitrary choice to define or not since they are scalars.
      best_split <- rep(0, 1)
      
      unique_values <- sort(unique(X_train[data_indices]))
      if(length(unique_values)==1){
        is_terminal[split_node] <- TRUE
        need_split[split_node] <- FALSE
        node_splits[split_node]<- unique_values[1]
        currentMean[split_node] <- mean(y_train[data_indices])
      }
      else{
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
          
          impurityLeft <- (1/length(data_indices)) * sum((y_train[left_indices] - mean(y_train[left_indices]))^2)
          impurityRight <- (1/length(data_indices)) * sum((y_train[right_indices] - mean(y_train[right_indices]))^2)
          split_scores[s] <- impurityLeft + impurityRight
        }
        
        best_score <- min(split_scores)
        best_split <- split_positions[which.min(split_scores)]  
        node_splits[split_node] <- best_split
        
        # create left node using the selected split
        left_indices <- data_indices[which(X_train[data_indices] < best_split)]
        node_indices[[2 * split_node]] <- left_indices
        
        if(length(left_indices) <= P){
          is_terminal[2 * split_node] <- TRUE
          need_split[2 * split_node] <- FALSE
          currentMean[2 * split_node] <- mean(y_train[left_indices])
          node_splits[2 * split_node]<- mean(left_indices)
        }
        else{
          is_terminal[2 * split_node] <- FALSE
          need_split[2 * split_node] <- TRUE
        }
        
        #create right node
        right_indices <- data_indices[which(X_train[data_indices] >= best_split)]
        node_indices[[2 * split_node + 1]] <- right_indices
        
        if(length(right_indices) <= P){
          is_terminal[2 * split_node+1] <- TRUE
          need_split[2 * split_node +1] <- FALSE
          currentMean[2 * split_node+1] <- mean(y_train[right_indices])
          node_splits[2 * split_node+1]<- mean(right_indices)
        }
        else{
          is_terminal[2 * split_node + 1] <- FALSE
          need_split[2 * split_node + 1] <- TRUE
        } 
      }
    }
  }
  
  # traverse tree for test data points
  y_predicted <- rep(0, N_test)
  for (i in 1:N_test) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE) {
        y_predicted[i] <- currentMean[index]
        break
      } else {
        if (X_test[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
  }
  
  RMSEs[P] <- sqrt(sum((y_predicted-y_test)^2)/N_test)
  print(paste0("RMSE is ", RMSEs[P]," when P is ", P))
  
}

plot(1:20,RMSEs, type="b",ylab = "RMSE", xlab = "P", pch=1,lwd=2)
title("RMSE vs P")

