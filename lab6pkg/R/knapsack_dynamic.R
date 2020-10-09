#' Dynamic programming Solution for Kanpsack Problem
#' @title Knapsack_dynamic 
#' @param x is a dataframe constising 
#' @param W is maximum weight(capacity) of kanpsack 
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving weight of 
#' selected elements from dataframe x 
#' @examples ?
#' @description The knapsack problem is a problem in combinatorial optimization:
#' Given a set of items, each with a weight and a value,
#' determine the number of each item to include in a collection 
#' so that the total weight is less than or equal to a given limit and the total value is as large as possible.
#' This function should return the same results as the brute force algorithm,
#' but unlike the brute force it should scale much better since the algorithm will run in O<Wn>.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @export knapsack_dynamic



knapsack_dynamic <- function(x,W){
  
  stopifnot(is.data.frame(x), is.numeric(x$v), is.numeric(x$w), is.numeric(W), W!=1)
  #A is value matrix
  #A columns are maximum weight of knapsack(0:W)
  #A rows are items(x$w)(0:heaviest item)
  A <- matrix(0, nrow = length(x$w)+1, ncol = W+1)
  A[,1]<-0
  A[1,]<-0
  
  for (i in 2:length(x$w)+1) {
    for (j in 2:(W+1)) {
      if (x$w[i]>j) {
        A[i,j]<- A[i-1,j]
      }
      else{
        A[i,j]<- max(A[i-1,j],x$v[i]+A[i-1,j-x$w[i]])
      }
    }
  }
  
  #Now finding included elements in knapsack, maximum value and current weight of knapsack
  
  i <- length(x$w)+1
  j <- W+1
  k <- 1
  knapsack_Elements <- c()
  TotalW <- 0
  MaxValue <- 0
  
  while (i>1 & j>1) 
  {
    if(A[i,j] == A[i-1,j])
    {
      i <- i-1
    }
    else
    {
      knapsack_Elements[k] <- i
      k<- k+1
      MaxValue <- MaxValue + x[i,2]
      j <- j-x[i,1]
      i <- i-1
    }
  }
  return(list( 'Max Value' = round(A[length(x$w)+1, (W+1)]), 'Included Elements' = sort(knapsack_Elements)))
  }
