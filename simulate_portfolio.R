# portfolio simulation and analysis
simulate.portfolio <- function(R, rp, simulations = 10, rebalance_on = "quarters", ...){
  rebal.idx <- index(R[endpoints(R, on = rebalance_on, k = 1)])
  # list of weights randomly sampled with replacement from a set of random portfolios
  out <- vector("list", simulations)
  for(i in 1:simulations){
    # sample with replacement
    out[[i]] <- xts(rp[sample.int(n = nrow(rp), size = length(rebal.idx), replace = TRUE),], rebal.idx)
  }
  R.out <- lapply(out, function(x) Return.portfolio(R, weights = x))
  R.out <- do.call(cbind, R.out)
  R.out
}

# portfolio simulations
simulate.portfolio2 <- function(R, rp, simulations = 10, dynamic = TRUE, rebalance_on = "quarters", ...){
  rebal.idx <- index(R[endpoints(R, on = rebalance_on, k = 1)])
  out <- foreach(i = 1:simulations, .combine = cbind) %dopar% {
    # randomly sample random portfolios and build a time series of weights at
    # the specified rebalance frequency
    if(dynamic){
      w <- xts(rp[sample.int(n = nrow(rp), size = length(rebal.idx), replace = TRUE),], rebal.idx)
      r <- Return.portfolio(R, weights = w)
    } else {
      w <- as.vector(rp[sample.int(n = nrow(rp), size = 1, replace = TRUE),])
      r <- Return.portfolio(R[-1,], weights = w, rebalance_on = rebalance_on)
    }
    r
  }
  out
}

simulate.portfolio.fixed <- function(R, rp, simulations = 10, rebalance_on = "quarters", ...){
  rebal.idx <- index(R[endpoints(R, on = rebalance_on, k = 1)])
  out <- foreach(i = 1:simulations, .combine = cbind) %dopar% {
      # randomly sample random portfolios and build a time series of weights at
      # the specified rebalance frequency
      w <- as.vector(rp[sample.int(n = nrow(rp), size = 1, replace = TRUE),])
      r <- Return.portfolio(R, weights = w, rebalance_on = rebalance_on)
      r
  }
  out
}

information.ratio.component <- function(x, Rb, scale = 12){
  x.ap <- vector('numeric', ncol(x))
  x.te <- vector('numeric', ncol(x))
  x.ir <- vector('numeric', ncol(x))
  for(i in 1:ncol(x)){
    ra <- na.omit(x[,i])
    rb <- Rb[index(ra)]
    ap <- Return.annualized(ra) - Return.annualized(rb)
    te <- sd(ra - rb) * sqrt(scale)
    x.ap[i] <- ap
    x.te[i] <- te
    x.ir[i] <- ap / te
  }
  out <- list()
  out$ap <- x.ap
  out$te <- x.te
  out$ir <- x.ir
  out
}

sharpe.ratio.component <- function(x, scale = 12){
  x.ret <- vector('numeric', ncol(x))
  x.sd <- vector('numeric', ncol(x))
  x.sr <- vector('numeric', ncol(x))
  for(i in 1:ncol(x)){
    rr <- na.omit(x[,i])
    n <- nrow(rr)
    a.ret <- prod(1 + rr)^(scale / n) - 1
    a.sd <- sd(rr) * sqrt(scale)
    a.sr <- a.ret / a.sd
    x.ret[i] <- a.ret
    x.sd[i] <- a.sd
    x.sr[i] <- a.sr
  }
  out <- list()
  out$ar <- x.ret
  out$sd <- x.sd
  out$sr <- x.sr
  out
}

