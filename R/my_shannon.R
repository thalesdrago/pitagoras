#' Shannon diversity
#'
#'This function calculates the Shannon diversity index of community ecology
#'
#' @param x A numeric vector with values of abundance in the community
#'
#' @return A numeric vector of length one with the value of the Shannon diversity
#' @export
#'
#' @examples
#' com <- c(1, 2, 3)
#' my_shannon(com)
#'
my_shannon <- function(x) {
  x <- x[x > 0]
  sum_all <- sum(x)
  pi <- x/sum_all
  -sum(pi*log(pi))
}
