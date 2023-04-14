#' @title A simulated binary data
#' @docType data
#' @description A simulated binary data
#' @format A list with columns:
#' \describe{
#'  \item{x1}{A matrix of size 100x20, each column has mean 1 and sd 1}
#'  \item{x2}{A matrix of size 100x20, each column has mean 2 and sd 1}
#'  \item{x3}{A matrix of size 100x20, each column has mean 3 and sd 1}
#'  \item{y1}{A factor vector of 0's and 1's, created by beta and x1}
#'  \item{y2}{A factor vector of 0's and 1's, created by beta and x2}
#'  \item{y3}{A factor vector of 0's and 1's, created by beta and x3}
#'  \item{beta}{A random vector with first 10 entries drawn from random unif(-1, 1), otherwise 0's.}
#' }
#' @examples
#' data(TOP_data_binary)
#' ## Loading simulated matrices and vectors
#' x1 <- TOP_data_binary$x1
#' x2 <- TOP_data_binary$x2
#' x3 <- TOP_data_binary$x3
#' y1 <- TOP_data_binary$y1
#' y2 <- TOP_data_binary$y2
#' y3 <- TOP_data_binary$y3
#' 
#' set.seed(13)
#' n = 100
#' p = 20
#' x1 <- matrix(rnorm(n * p, mean = 1, sd = 1), nrow = n, ncol = p)
#' x2 <- matrix(rnorm(n * p, mean = 2, sd = 1), nrow = n, ncol = p)
#' x3 <- matrix(rnorm(n * p, mean = 3, sd = 1), nrow = n, ncol = p)
#' colnames(x1) <- colnames(x2) <- colnames(x3) <- sprintf("X%02d", 1:p)
#' z1 <- pairwise_col_diff(x1)
#' z2 <- pairwise_col_diff(x2)
#' z3 <- pairwise_col_diff(x3)
#' k <- 10
#' q <- choose(p, 2)
#' beta <- c(runif(k, -1, 1), rep(0, q - k))
#' names(beta) <- colnames(z1)
#' y1 <- factor(rbinom(n, 1, prob = expit(z1 %*% beta)), levels = c("0", "1"))
#' y2 <- factor(rbinom(n, 1, prob = expit(z2 %*% beta)), levels = c("0", "1"))
#' y3 <- factor(rbinom(n, 1, prob = expit(z3 %*% beta)), levels = c("0", "1"))
#' TOP_data_binary <- tibble::lst(x1, x2, x3, y1, y2, y3, beta)
#' usethis::use_data(TOP_data_binary)
#' 
#' @return The example data.
"TOP_data_binary"
