
# set.seed(42)
# n <- 20
# knapsack_objects <- data.frame(w=sample(1:40, size = n, replace = TRUE), v=runif(n = n, 0, 10))

knapsack_BruteForce <- function(x , W, parallel = TRUE)
{
  stopifnot(is.data.frame(x) || is.numeric(n) || W >= 0 )
  knapsack <- 0
  weight <- 0
  value <- 0
  
  library('parallel')
  a <- detectCores()-4
  cl <- makeCluster(a)
  
  # clusterExport(cl, c('x'), envir = environment())
  # clusterEvalQ(cl , {require(parallel)})
               
  w_list <- parLapply(cl, 1:nrow(x), function(i,f) {comb <- combn(x[[f]], i)}, f = 'w')
  v_list <- parLapply(cl, 1:nrow(x), function(i,f) {comb <- combn(x[[f]], i)}, f = 'v')
               
  stopCluster(cl)
               
  for (i in 1:nrow(x))
    {
     comb_w <- as.data.frame(w_list[[i]])
     comb_v <- as.data.frame(v_list[[i]])
    }
               
    sum_w <- colSums(comb_w)
    sum_v <- colSums(comb_v)
               
   components <- which(sum_w <= W)
               
   if (length(components) != 0)
               {
                 value_max = max(sum_v[components])
                 if (value_max > value)
                 {
                   value <- value_max
                   index =  which(sum_v == value)
                   weight = comb_w[,index]
                   elements = match(weight, x$w)
                 }
               }
               
    knapsack = list("value" = round(value), "elements" = elements)
   return(knapsack)
}


# If weight of the nth item is more than Knapsack of capacity W, then this item cannot be included in the optimal solution 


# return the maximum of two cases: (1) nth item included (2) not included 

# knapsack_BruteForce(x=knapsack_objects, W=50)
