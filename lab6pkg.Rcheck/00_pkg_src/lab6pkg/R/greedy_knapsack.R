#'greedy function for knapsack problem
#'@name greedy_knapsack
#'@param x A data.frame. with weight and value
#'@param W A number. the overall weight
#'
#'@description implement the function using greedy method.
#'
#'@return list with maximium value and picked elements
#'@usage  greedy_knapsack(x,W)
#'
#'@examples
#' set.seed(42)
#'   n <- 2000
#'   knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
#'                              v=runif(n = n, 0, 10000))
#'  l1 <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'@export

greedy_knapsack <- function(x,W){
  x$id <- 1:length(x$w)     #give the id to each line
  df1 <- x[which(x$w < W),]  #find the items that can be taken into the knapsack
  df1$ratio <- df1$v/df1$w   #calculate the ratio v/w
  decr_ord <- order(df1$ratio,decreasing = TRUE) #order the items

  df1 <- df1[decr_ord,]  #pick up the those items
  # print(df1)
  weight <- 0
  value <- 0
  elements <- NULL
  i <- 1
  while(weight+df1$w[i]<W && i<length(df1$w)){
    value <- value + df1$v[i]   #add the value
    weight <- weight + df1$w[i]  #add the weight of items
    elements <- c(elements,df1$id[i])  #put it in the elements
    i <- i+1
  }

  # print(elements)
  l <- list(value=value,elements=elements)
}

####just for test
# set.seed(42)
# n <- 2000
# knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# 
# l1 <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
# l1





