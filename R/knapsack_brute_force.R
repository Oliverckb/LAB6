#' brute force
#' @name Knapsack_brute_force 
#' @param x is a data frame with two columns w(weight) and v(value)
#' @param W is maximum weight(capacity) of kanpsack 
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving weight of 
#' selected elements from data frame x 
#' @usage knapsack_brute_force(x,W)
#'
#' @examples
#'   RNGversion(min(as.character(getRversion()),"3.6.1"))
#'   set.seed(42,kind="Mersenne-Twister",normal.kind = "Inversion")
#'   n <- 2000
#'   knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
#'                              v=runif(n = n, 0, 10000))
#'   l<-knapsack_brute_force(knapsack_objects[1:12,],3500)
#'   
#' @description The knapsack problem is a problem in combinatorial optimization:
#' Given a set of items, each with a weight and a value,
#' determine the number of each item to include in a collection 
#' so that the total weight is less than or equal to a given limit and the total value is as large as possible.
#' This function will run in O<2^n>.
#' @seealso  \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @export
 
 
# set.seed(42)
# n <- 20
# knapsack_objects <- data.frame(w=sample(1:40, size = n, replace = TRUE), v=runif(n = n, 0, 10))


knapsack_brute_force <- function(x,W){
  
  item <- NULL
  value <- NULL
  weight <- NULL
  for(i in 1:nrow(x)){
    item <- c(item,combn(1:nrow(x), i,paste,collapse = ","))
    value <- c(value,combn(x$v,i,sum))
    weight <- c(weight,combn(x$w,i,sum))
    total <- data.frame(item=item,value=value,weight=weight)
  }
  # print(total)
  # values_col <- colSums(value)
  # weight_col <- colSums(weight)
  
  weight_ava <- which(total$weight < W)
  max_val <- max(total$value[weight_ava])
  val_id <- total$item[which(total$value == max_val)]
  make_vector <- as.numeric(unlist(strsplit(val_id,",",fixed=TRUE)))
  # for(i in 1:length(w_list)){
  #   for(j in 1:length(w_list[[i]][1,])){
  #     w_list[[i]][1,]
  #   }
  # }
  #
  l <- list("value"=max_val,"elements"=make_vector)
  return(l)
}
