set.seed(42)
RNGversion(min(as.character(getRversion()),"4.0.2"))
set.seed(42,kind="Mersenne-Twister",normal.kind = "Inversion")
n <- 2000
knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),
                              v=runif(n = n, 0, 10000)
)

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


