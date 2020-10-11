#' Dynamic programming Solution for Kanpsack Problem
#' @name Knapsack_dynamic 
#' @param x is a data frame with two columns w(weight) and v(value)
#' @param W is maximum weight(capacity) of kanpsack 
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving weight of 
#' selected elements from data frame x 
#' @usage knapsack_dynamic(x,W)
#'
#' @examples
#'   RNGversion(min(as.character(getRversion()),"3.6.1"))
#'   set.seed(42,kind="Mersenne-Twister",normal.kind = "Inversion")
#'   n <- 2000
#'   knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
#'                              v=runif(n = n, 0, 10000))
#'   l<-knapsack_dynamic(knapsack_objects[1:12,],3500)
#'   
#' @description The knapsack problem is a problem in combinatorial optimization:
#' Given a set of items, each with a weight and a value,
#' determine the number of each item to include in a collection 
#' so that the total weight is less than or equal to a given limit and the total value is as large as possible.
#' This function should return the same results as the brute force algorithm,
#' but unlike the brute force it should scale much better since the algorithm will run in O<Wn>.
#' @seealso  \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @export



knapsack_dynamic <- function(x,W){
  
  stopifnot(is.data.frame(x), is.numeric(x$v), is.numeric(x$w), is.numeric(W), W!=1)
  #A is value matrix
  #A columns are maximum weight of knapsack(0:W)
  #A rows are items(x$w)(0:weight of the last item)
  
  lw<- length(x$w)
  A <- matrix(0, nrow = lw+1, ncol = W+1)
  
  # defining knapsack_object
  
  # RNGversion(min(as.character(getRversion()),"3.6.1"))
  # set.seed(42,kind="Mersenne-Twister",normal.kind = "Inversion")
  # n <- 2000
  # knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
  #                              v=runif(n = n, 0, 10000))
  
  
  for (i in 2:(lw+1)) {
    for (j in 2:(W+1)) {
      if ((x$w[i-1])>j) {
        A[i,j]<- A[i-1,j]
      }
      else{
        A[i,j]<- max(A[i-1,j],(x$v[i-1]+A[i-1,j-x$w[i-1]]))
      }
    }
  }
  #Now finding included elements in knapsack, maximum value and current weight of knapsack
  
  i <- lw+1
  j <- W+1
  k <- 1
  knapsack_Elements <- c()
  TotalW <- 0
  MaxValue <- 0
  
  repeat{
    if(A[i,j] == A[i-1,j]) {
      i <- i-1
    }
    else {
      knapsack_Elements[k] <- i-1
      k<- k+1
      j <- j-x$w[i-1]
      i <- i-1
    }
    
    if((i<= 1) ||(j<=1)) {
      return(list( 'value' = round(A[lw+1, (W+1)]), 'elements' = sort(knapsack_Elements)))
      break}
  }
}
#system.time(knapsack_dynamic(knapsack_objects[1:12,],3500))

# Profiling
# install library --> devtools::install_github("hadley/lineprof")
# library(lineprof)
# source("")
# l <- lineprof(knapsack_dynamic())
# l