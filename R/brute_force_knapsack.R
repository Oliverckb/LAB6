#' Brute force Solution for Knapsack Problem
#' @name brute_force_knapsack 
#' @param x is a data frame with two columns w(weight) and v(value)
#' @param W is maximum weight(capacity) of knapsack 
#' @param parallel is to tell R whether or not to use parallel computation
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving weight of 
#' selected elements from data frame x 
#' @usage brute_force_knapsack(x,W,parallel)
#'
#' @examples
#'   RNGversion(min(as.character(getRversion()),"3.6.1"))
#'   set.seed(42,kind="Mersenne-Twister",normal.kind = "Inversion")
#'   n <- 2000
#'   knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
#'                              v=runif(n = n, 0, 10000))
#'   l<-brute_force_knapsack(knapsack_objects[1:16,] , 3500,FALSE)
#'   
#' @description The knapsack problem is a combinatorial optimization problem.
#' A dataframe is given having two parameters, weight and value. 
#' Each value has its own weight and we have to pack items with maximum value and within weight capacity.
#' This function will run in O<2^n>.
#' @seealso  \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @import parallel
#' @importFrom utils combn
#' @export
 

#  set.seed(42)
#  RNGversion(min(as.character(getRversion()),"4.0.2"))
#  set.seed(42,kind="Mersenne-Twister",normal.kind = "Inversion")
#  n <- 2000
# knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
#                               v=runif(n = n, 0, 10000))

brute_force_knapsack <- function(x, W, parallel=FALSE)
{
  stopifnot(is.data.frame(x),W >= 0)
  
  if(parallel == FALSE)
  {
    object <- NULL
    weight <- NULL
    value <- NULL
    elements <- NULL
    
    for(i in 1:length(x$w))
    {
      object <- c(object,combn(1:length(x$w), i,paste,collapse = ","))
      weight <- c(weight,combn(x$w,i,sum))
      value <- c(value,combn(x$v,i,sum))
      total <- data.frame(object=object, weight=weight, value=value)
    }
  }
  
  else 
  {
    library(parallel)
    object <- NULL
    weight <- NULL
    value <- NULL
    elements <- NULL
   
    numofCores <- detectCores()-2
    cl <- makeCluster(numofCores)
    clusterExport(cl, c('x'), envir = environment())
    clusterEvalQ(cl , {require(parallel)})
   
    object <- parLapply(cl, 1:length(x$w), function(t) {
      
      combn(1:length(x$w),t,paste,collapse = ",")
      
      })
    weight <- parLapply(cl, 1:length(x$w), function(t) {
      combn(x$w,t,sum)
      })
    value  <- parLapply(cl, 1:length(x$v), function(t) {
      combn(x$v,t,sum)
      })
    total  <- data.frame(object=unlist(object), weight=unlist(weight), value=unlist(value))
    stopCluster(cl)
  }
  
  optimal_weights <- which(total$weight<=W)
  max_value <- max(total$value[optimal_weights])
  
  index <- which(total$value == max_value)
  y <- total$object[index]
  print(class(y))
  
  elements <- as.numeric(unlist(strsplit(y,",")))
  
  knapsack <- list(value = round(max_value),elements= elements)
  return(knapsack)
}

#Question How much time does it takes to run the algorithm for n = 16 objects?
# system.time(brute_force_knapsack(knapsack_objects[1:12,] , 3500))
# user  system elapsed 
# 0.90    0.00    0.91

# Profiling
# install library --> devtools::install_github("hadley/lineprof")
# library(lineprof)
# source("")
# l <- lineprof(brute_force_knapsack())
# l
# shine(l)