#' Recursive Sequence Plot
#'
#' @param x Numeric vector
#' @param n Integer
#'
#' @return
#' @export
#'
#' @examples
myseq_n_df <- function(x, n) {
  if (length(x) == 3 && is.numeric(x) && n > 0) {
    nvec <- vector(mode = "double", length = n)
    for (i in seq_along(nvec)) {
      if (i <= 3) {
        nvec[[i]] = x[[i]]
      } else if (i > 3 && i <= n) {
        nvec[[i]] = nvec[[i - 1]] + ((nvec[[i - 3]] - nvec[[i - 2]]) / i)
      } else if (i > n){
        stop()
      }
    }
  }
  nvector <- nvec[3:n]
  n <- 3:n
  df <- data.frame(nvector, n)
  df %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = n, y = nvector)) +
    ggplot2::geom_line()

}
