myseq_n <- function(x, n) {
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
  return(nvec[n])

}

