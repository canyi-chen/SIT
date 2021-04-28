#' @references Zhang, K. (2019). Bet on independence. Journal of the American Statistical Association, 114(528), 1620–1637. https://doi.org/10.1080/01621459.2018.1537921
#' @references Kong, E., Xia, Y., & Zhong, W. (2019). Composite coefficient of determination and its application in ultrahigh dimensional variable screening. Journal of the American Statistical Association, 114(528), 1740–1751. https://doi.org/10.1080/01621459.2018.1514305
#' @export
gen_data <- function(n, lambda, case) {
  X <- runif(n, -1, 1)
  noise <- rnorm(n)
  if(case == 1) {
    x0 <- rbinom(n, 1, 1/2)*2 - 1
    y0 <- rbinom(n, 1, 1/2)*2 - 1
    U <- 10*lambda*rnorm(n)
    V <- 10*lambda*rnorm(n)
    X <- x0 + U
    Y <- y0 + V
    return(list(X = X,
                Y = Y))
  }
  if(case == 2) {
    # linear
    Y <- 0.5*X + 10*lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 3) {
    # Step function
    Y <- 3*(-1<=X & X< -0.5) + 2*(-0.5<=X & X<0) -
      4*(0<=X & X<0.5) - 3*(0.5<=X & X <=1) + 50*lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 4) {
    # W-shaped
    Y <- abs(X + 0.5)*(X<0) + abs(X-0.5)*(X>=0) +
      lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 5) {
    # Sinusoid
    Y <- cos(8*pi*X) + 5*lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 6) {
    Z <- rbinom(n, 1, 1/2)*2-1
    Y <- Z*(1-X^2)^(1/2) + lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 7) {
    # Heteroskedastic
    sigmaX <- (abs(X)<=0.5)
    Y <- 3*(sigmaX*(1-lambda) + lambda)*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 8) {
    # Diamond
    U <- runif(n, -1, 1)
    V <- runif(n, -1, 1)
    Up <- U*cos(pi/4) + V*sin(pi/4)
    Vp <- -U*sin(pi/4) + V*cos(pi/4)
    Upp <- runif(n, -1, 1)
    Vpp <- runif(n, -1, 1)
    R <- runif(n)
    X <- (R>lambda)*Up + (R<lambda)*Upp
    Y <- (R>lambda)*Vp + (R<lambda)*Vpp
    return(list(X = X,
                Y = Y))
  }
  if(case == 9) {
    # Parabola
    Y <-  0.25*X^2 + lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 10) {
    # Two-parabola
    V <- rbinom(n, 1, 1/2)*2 - 1
    Y <-  (X^2 + lambda*noise)*V
    return(list(X = X,
                Y = Y))
  }
  if(case == 11) {
    # Variance
    Y = 3*(sqrt(X^2 + 1)*(1-lambda) + lambda)*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 12) {
    # Log
    Y = 0.05*log(X^2) + lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 13) {
    # Half-Circular
    Y <- (1-X^2)^(1/2) + 3*lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 14) {
    # Checkerboard
    W <- sample(c(1,2,3), n, replace = T)
    X <- W + lambda*rnorm(n)/4
    V1 <- sample(c(2,4), n, replace = T)
    V2 <- sample(c(1,3,5), n, replace = T)
    Y <- (V1 + lambda*rnorm(n))*(W==2) + (V2 + lambda*rnorm(n))*(W!=2)
    return(list(X = X,
                Y = Y))
  }
  if(case == 15) {
    # Local
    G1 <- rnorm(n, 0, 1/2)
    G2 <- rnorm(n, 0, 1/2)
    X <- G1
    Y <- (X + lambda*rnorm(n))*((0<=G1) & (G1<=1) & (0<=G2) & (G2<=1)) + G2
    return(list(X = X,
                Y = Y))
  }
  if(case == 16) {
    # Independent-uniform
    Y <- runif(n, -1, 1)
    return(list(X = X,
                Y = Y))
  }
  if(case == 17) {
    # Independent-normal
    X <- rnorm(n)
    Y <- rnorm(n)
    return(list(X = X,
                Y = Y))
  }
  if(case == 18) {
    # Independent-cauchy
    X <- rcauchy(n)
    Y <- rcauchy(n)
    return(list(X = X,
                Y = Y))
  }
  if(case == 19) {
    # Doppler
    Y <- sqrt(X^2*(1 - X^2))*sin(pi*1.05/X^2 + .05) + 1.5*lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 20) {
    # HeaviSine
    Y <- 4*sin(4*pi*X^2) - sign(X^2 - .3) -
      sign(.72 - X^2) + 24*lambda*noise
    return(list(X = X,
                Y = Y))
  }
  if(case == 21) {
    # Spiral
    u <- runif(n, -1, 1)
    X <- u^2*cos(u^2*4*pi) + lambda*rnorm(n)
    Y <- u^2*sin(u^2*4*pi) + lambda*rnorm(n)
    return(list(X = X,
                Y = Y))
  }
  # default: independent
  Y <- runif(n, -1, 1)
  return(list(X = X,
              Y = Y))
}
