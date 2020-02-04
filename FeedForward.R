#import the german data
german_data <- read.table(file.choose(),header=FALSE)

#read the data
head(german_data)

#generate random weights
rand_weights_vector <- runif(ncol(german_data) * nrow(german_data))

#convert vector to matrix
rand_weights_matrix <- matrix(rand_weights_vector,nrow = ncol(german_data), ncol=nrow(german_data),byrow=TRUE)

#store the state of neural network as it is trained
data_neural_network <-  list( 
  input = german_data, 
  #weights for layer 1
  W1 <-  rand_weights_matrix,
  #weights for layer 2
  W2 <- matrix(runif(4), ncol=1),
  #actual observed data
  y = german_data[3],
  #store of predicted information
  output = matrix(
    rep(0,times = 5),
    ncol = 1
  ))

#the activation function
sigmoid <- function(x){
  Y = 1.0/(1.0 + exp(-x))
  return(Y)
}

#the derivative of the sigmoid function
sigmoid_derivative  <- function(x){
  x * (1.0 -x)
}

#generate the loss function
loss_function <- function(nn){
  sum(nn$y - nn$output) ^ 2
}

feedforward <- function(nn) {
  
  nn$layer1 <- sigmoid(nn$input %*% nn$W1)
  nn$output <- sigmoid(nn$layer1 %*% nn$W2)
  
  nn
}

n <- 1000

backprop <- function(nn) {
  
  # application of the chain rule to find derivative of the loss function with 
  # respect to weights2 and weights1
  d_weights2 <- (
    t(nn$layer1) %*%
      # `2 * (nn$y - nn$output)` is the derivative of the sigmoid loss function
      (2 * (nn$y - nn$output) *
         sigmoid_derivative(nn$output))
  )
  
  d_weights1 <- ( 2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)) %*% 
    t(nn$weights2)
  d_weights1 <- d_weights1 * sigmoid_derivative(nn$layer1)
  d_weights1 <- t(nn$input) %*% d_weights1
  
  # update the weights using the derivative (slope) of the loss function
  nn$W1 <- nn$W1 + d_weights1
  nn$W2 <- nn$W2 + d_weights2
  
  nn
}

loss_df <- data.frame(
  iteration = 1:n,
  loss = vector("numeric", length = n)
)

for (i in seq_len(1500)) {
  my_nn <- feedforward(data_neural_network)
  my_nn <- backprop(data_neural_network)
  
  # store the result of the loss function.  We will plot this later
  loss_df$loss[i] <- loss_function(data_neural_network)
}

# print the predicted outcome next to the actual outcome
data.frame(
  "Predicted" = round(data_neural_network$output, 3),
  "Actual" = Y
)