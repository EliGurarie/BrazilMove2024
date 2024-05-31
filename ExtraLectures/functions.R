variogram <- function(z, t, lag.max){

  dt <- median(diff(t))
  lag.time <- seq(0, lag.max, dt)
  lags <- 1:(length(lag.time)-1)

  n <- length(z)

  x.diff <- outer(Re(z), Re(z), function(a,b) abs(a-b))
  x.var <- sapply(lags,
                  function(l) var(x.diff[cbind(1:n,
                                               ifelse((1:n)-l >= 1, (1:n)-l, NA))],
                                  na.rm = TRUE))
  y.diff <- outer(Im(z), Im(z), function(a,b) abs(a-b))
  y.var <- sapply(lags,
                  function(l) var(x.diff[cbind(1:n,
                                               ifelse((1:n)-l >= 1, (1:n)-l, NA))],
                                  na.rm = TRUE))

  data.frame(lag = lag.time, vgram = c(0,(x.var + y.var)/2))
}

plotVariogram <- function(vg, ...){
  plot(vg, type = "l", lwd = 3, xlab = "time", ...)
  legend("topleft", col = 1:2, legend = c("observed", "theory"), lwd = 2)
}

OU <- function(tau=1, alpha=1, mu=0, dt=.01, Tmax=100, X0 = rnorm(1, mu, sqrt(alpha^2*tau/2))){
  T <- seq(0,Tmax,dt)
  n <- length(T)
  W <- rnorm(n, 0, 1/sqrt(dt))
  X <- c(X0,rep(0,n-1))
  for(i in 2:n)
    X[i] <- X[i-1] - (1/tau)*(X[i-1] - mu)*dt + alpha*W[i]*dt
  return(data.frame(T,X))
}

OUP <- function(mux=0, muy=0, A = 100, tau = 1, ...){
  zp <- sqrt(-2*log(1-.95))
  alpha <- sqrt(2*A/ (pi*zp^2*tau))
  X <- OU(mu=mux, alpha=alpha, tau = tau, ...)$X
  Y <- OU(mu=muy, alpha=alpha, tau = tau, ...)$X
  T <- OU(...)$T
  return(data.frame(T, Z = X + 1i*Y))
}

CVM <- function(Tmax, tau, beta=1, v0, dt){
  T <- seq(0,Tmax,dt)
  n <- length(T)
  V <- T*0
  dW <- (rnorm(n) + 1i*rnorm(n))*sqrt(dt)
  V[1] <- v0
  for(i in 2:n)
    V[i] <-  V[i-1] - V[i-1] * dt/tau + beta * dW[i]
  Z <- cumsum(V)*dt
  return(Z)
}
