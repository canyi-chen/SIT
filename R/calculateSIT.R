#' @export
calculateSIT <- function(x, y, c) {
  n <- length(x)

  # h <- floor(n / c)
  # s <- sort(x, index.return = T)
  # ys <- y[s$ix]
  # r1 <- rank(ys)

  h <- floor(n / c)
  xr <- rank(x, ties.method = "random")
  ix <- order(xr)
  ys <- y[ix]
  r1 <- rank(ys)

  s <- blocksum(r1, c)
  a <- s / n ^ 2 / (c - 1)
  b <- sum(r1) / n / (n - 1) - sum(r1 ^ 2) / n ^ 2 / (n - 1)

  sit <- 1 - a / b
  return(sit)
}
