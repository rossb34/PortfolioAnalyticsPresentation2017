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
