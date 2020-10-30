
#' Algorithm for the 0-1 knapsack problem
#'
#' @param x a dataframe with w (weights) as first column and v (values) as second column.
#' @param W a scalar representing the limiting capacity of the knapsack
#'
#' @return a list of the maximum value and the elements used to obtail this value
#' @export
#'
#' @examples
#' set.seed (42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample (1: 4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
brute_force_knapsack <- function(x, W){
  if (!is.data.frame(x))
    stop("x is not a dataframe!")
  weights = as.matrix(x[, 1])
  values  = as.matrix(x[, 2])
  if(any(weights <= 0 | values <= 0))
    stop("input must be positive")

  n_items = nrow(x)
  all_combinations = 0:(2^n_items - 1)
  all_combinations_matrix =
    sapply(all_combinations, function(x){
      result = intToBits(x)
      result = as.numeric(result)
      return(result[1:n_items])
    })

  matrix1 = all_combinations_matrix
  for (i in 1:ncol(matrix1)){
    matrix1[,i] = weights * matrix1[,i]
  }
  sum_w = as.matrix(colSums(matrix1))
  matrix_weights = as.matrix(which(sum_w < W))
  matrix_weights_update = all_combinations_matrix[, c(matrix_weights)]

  matrix2 = all_combinations_matrix[, c(matrix_weights)]
  for (i in 1:ncol(matrix2)){
    matrix2[,i] = values * matrix2[,i]
  }
  sum_v = as.matrix(colSums(matrix2))
  opt_value = which.max(sum_v)
  value = sum_v[opt_value, ]

  opt_weights = as.matrix(matrix_weights_update[, opt_value])
  #convert to row index:
  elements = which(!opt_weights[,1] == 0) #row elements that doesn't start with 0

  list1 = list(value = value, elements = elements)
  return(list1)
}


#' Greedy aproximation algorithm for the knapsack problem
#'
#' @param x a dataframe with w (weights) as first column and v (values) as second column.
#' @param W a scalar representing the limiting capacity of the knapsack
#'
#' @return a list of the maximum value and the elements used to obtail this value
#' @export
#'
#' @examples
#' set.seed (42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample (1: 4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack <- function(x, W){
  x$ratio = x[, 2] / x[, 1]
  x = x[order(x$ratio, decreasing = TRUE), ]
  cum_sum = as.matrix(cumsum(x$w))
  limit = tail(which(cum_sum <= W), 1)
  elements = as.matrix(row.names(x))
  elements = as.numeric(elements[c(1:limit),])
  optimal_size = x[c(1:limit), ]
  optimal_value = sum(optimal_size$v)
  greedy_list = list(value = optimal_value,
                     elements = elements)
  return(greedy_list)
}


