#' Split data into training and test sets
#'
#' @param data A data frame.
#' @param prop Proportion of rows used for training.
#' @param seed Random seed for reproducibility.
#' @return A list with elements `train` and `test`.
#' @export
split_data <- function(data, prop = 0.7, seed = 123) {
  set.seed(seed)
  idx <- sample(nrow(data), size = floor(prop * nrow(data)))
  list(train = data[idx, ], test = data[-idx, ])
}
