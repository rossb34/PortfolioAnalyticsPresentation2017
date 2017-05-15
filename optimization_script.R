library(PortfolioAnalytics)
library(methods)
library(foreach)
library(iterators)
require(doParallel, quietly=TRUE)
registerDoParallel(cores=detectCores()-4)

source('data_prep.R')
source('simulate_portfolio.R')

# setup for output
create.fig.path <- function(x, fig.dir = 'figures/', type = 'png'){
    paste0(fig.dir, x, '.', type)
}

# common resolution for projectors is 1024 x 768
w.px <- 1024 * 0.8
h.px <- 768 * 0.6

# random portfolios number of permutations
rp.n <- 10000

normalize.weights <- function(weights){
  for(r in 1:nrow(weights)){
    weights[r,] <- weights[r,] / sum(weights[r,])
  }
  weights
}

# data
# sector ETFs
# monthly returns
# 1999 through May 2016

R.eq <- cbind(R.mkt, R.sector)
R <- R.sector
png(create.fig.path('risk_return_all'), width = w.px, height = h.px, units = 'px')
chart.RiskReturnScatter(R.eq, main = 'Risk and Return Scatter Plot', add.sharpe = NA)
dev.off()
# table.AnnualizedReturns(R.eq)

# start with a simple portfolio optimization problem
p <- portfolio.spec(assets = colnames(R), weight_seq = generatesequence(min = 0, max = 1, by = 0.002))
p <- add.constraint(portfolio = p, type = 'weight_sum', min_sum = 0.99, max_sum = 1.01)
p <- add.constraint(portfolio = p, type = 'box', min = 0, max = 1)
p <- add.objective(portfolio = p, type = 'return', name = 'mean', multiplier = 0)
p <- add.objective(portfolio = p, type = 'risk', name = 'StdDev')
print(p)

# run the optimization using the last 36 observations (i.e. 3 years)
# generate random portfolios
rp <- random_portfolios(portfolio = p, permutations = rp.n, method = 'sample')
rp <- normalize.weights(rp)
opt.base <- optimize.portfolio(R = tail(R, 36), portfolio = p, optimize_method = 'random', rp = rp, trace = TRUE)
opt.base

png(create.fig.path('opt_base_weights'), width = w.px, height = h.px, units = 'px')
chart.Weights(opt.base)
dev.off()


png(create.fig.path('opt_base_risk_return'), width = w.px, height = h.px, units = 'px')
chart.RiskReward(opt.base, risk.col = 'StdDev', neighbors = 50)
dev.off()

# box constraints
p.box <- p
p.box <- add.constraint(portfolio = p.box, type = 'box', min = 0.05, max = 0.20, indexnum = 2)

rp.box <- random_portfolios(portfolio = p.box, permutations = rp.n, method = 'sample')
rp.box <- normalize.weights(rp.box)
opt.box <- optimize.portfolio(R = tail(R, 36), portfolio = p.box, optimize_method = 'random', rp = rp.box, trace = TRUE)
opt.box

png(create.fig.path('opt_box_weights'), width = w.px, height = h.px, units = 'px')
chart.Weights(opt.box)
dev.off()

png(create.fig.path('opt_box_risk_return'), width = w.px, height = h.px, units = 'px')
chart.RiskReward(opt.box, risk.col = 'StdDev', neighbors = 50)
dev.off()

# risk budget
# p.rb <- p
# p.rb <- add.objective(portfolio = p.rb, type = 'risk_budget', name = 'StdDev', min_prisk = 0.10, max_prisk = 0.12)
# p.rb <- add.objective(portfolio = p.rb, type = 'risk_budget', name = 'StdDev', min_concentration = TRUE)
# opt.rb <- optimize.portfolio(R = tail(R, 36), portfolio = p.rb, optimize_method = 'random', rp = rp, trace = TRUE)
# opt.rb
# chart.Weights(opt.rb)
# chart.RiskReward(opt.rb, risk.col = 'StdDev', neighbors = 50)
# chart.RiskBudget(opt.rb, risk.type = 'percentage')

# in sample feasible space comparison of the baseline portfolio and box
# constrained portfolio
xs.base <- extractStats(opt.base)
optimal.base <- xs.base[which.min(xs.base[,'out']),]
xs.box <- extractStats(opt.box)
optimal.box <- xs.box[which.min(xs.box[,'out']),]

png(create.fig.path('feasible_base_box'), width = w.px, height = h.px, units = 'px')
plot(x = xs.base[, 'StdDev'], y = xs.base[, 'mean'], col = 'gray', main = 'Feasible Space', ylab = 'mean', xlab = 'StdDev')
points(x = xs.box[, 'StdDev'], y = xs.box[, 'mean'], col = 'salmon', pch = 5)
points(x = optimal.base['StdDev'], y = optimal.base['mean'], col = 'black', pch = 19)
points(x = optimal.box['StdDev'], y = optimal.box['mean'], col = 'blue', pch = 15)
legend(x = 'topright', legend = c('base', 'box', 'optimal base', 'optimal box'), 
       col = c('gray', 'salmon', 'black', 'blue'), pch = c(1, 5, 19, 15))
dev.off()

# evaluate out of sample performance
rebal.period <- 'quarters'
n.train <- 36
n.roll <- 36

opt.base.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p,
                                                 optimize_method = 'random',
                                                 rp = rp, trace = TRUE,
                                                 rebalance_on = rebal.period,
                                                 training_period = n.train,
                                                 rolling_window = n.roll)
saveRDS(opt.base.rebal, file = 'out/opt_base_rebal.rds')
opt.base.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.base.rebal))
colnames(opt.base.rebal.r) <- 'base'

w <- extractWeights(opt.base.rebal)
all(abs(1 - rowSums(w)) <= 1e-8)

png(create.fig.path('opt_base_rebal_weights'), width = w.px, height = h.px, units = 'px')
chart.Weights(opt.base.rebal, main = 'Baseline Portfolio Optimal Weights')
dev.off()

opt.box.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p.box,
                                                optimize_method = 'random',
                                                rp = rp.box, trace = TRUE,
                                                rebalance_on = rebal.period,
                                                training_period = n.train,
                                                rolling_window = n.roll)
saveRDS(opt.box.rebal, file = 'out/opt_box_rebal.rds')
opt.box.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.box.rebal))
colnames(opt.box.rebal.r) <- 'box'

w <- extractWeights(opt.box.rebal)
all(abs(1 - rowSums(w)) <= 1e-8)

png(create.fig.path('opt_box_rebal_weights'), width = w.px, height = h.px, units = 'px')
chart.Weights(opt.box.rebal, main = 'Box Constrained Portfolio Optimal Weights')
dev.off()

opt.r <- na.omit(cbind(opt.base.rebal.r, opt.box.rebal.r, R.mkt))
png(create.fig.path('base_box_perf'), width = w.px, height = h.px, units = 'px')
charts.PerformanceSummary(opt.r, main = "Performance Summary")
dev.off()

table.AnnualizedReturns(opt.r)
table.InformationRatio(opt.r[,1:2], opt.r[,'SPY'])

# roll.te <- rollapply(opt.r[,c('box', 'SPY')], width = 12, by.column = FALSE, 
#                      FUN = function(x) as.numeric(TrackingError(Ra = x[,1], Rb = x[,2])))
# plot(roll.te, main = "12 Month Rolling Tracking Error", 
#      grid.ticks.on = "years", major.ticks = "years")

# custom moment function
# not sure what to do for a good example

# custom objective function
# stick with objective to minimize standard deviation, add penalty term for
# tracking error
te.target <- function(R, weights, Rb, min.te = 0.02, max.te = 0.05, scale = 12){
    # calculate the portfolio return
    r <- Return.portfolio(R = R, weights = weights)
    # align the indexes
    Rb <- Rb[index(r)]
    te <- sd(r - Rb) * sqrt(scale)
    # penalize tracking error outside of [min.te, max.te] range
    out <- 0
    if(te > max.te)
        out <- (te - max.te) * 10000
    if(te < min.te)
        out <- (min.te - te) * 10000
    out
}

p.te <- p
p.te <- add.objective(portfolio = p, type = 'risk', name = 'te.target',
                      arguments = list(Rb = R.mkt, scale = 12,
                                       min.te = 0.03, max.te = 0.05))

opt.te.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p.te,
                                               optimize_method = 'random',
                                               rp = rp, trace = TRUE,
                                               rebalance_on = rebal.period,
                                               training_period = n.train,
                                               rolling_window = n.roll)
saveRDS(opt.te.rebal, file = 'out/opt_te_rebal.rds')
opt.te.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.te.rebal))
colnames(opt.te.rebal.r) <- 'te.target'

w <- extractWeights(opt.te.rebal)
all(abs(1 - rowSums(w)) <= 1e-8)

png(create.fig.path('opt_te_rebal_weights'), width = w.px, height = h.px, units = 'px')
chart.Weights(opt.te.rebal, main = 'Tracking Error Target Portfolio Optimal Weights')
dev.off()


opt.r <- na.omit(cbind(opt.base.rebal.r, opt.box.rebal.r, opt.te.rebal.r, R.mkt))
png(create.fig.path('base_box_te_perf'), width = w.px, height = h.px, units = 'px')
charts.PerformanceSummary(opt.r, main = "Performance Summary")
dev.off()

xs.base.rebal <- extractStats(opt.base.rebal)
.extract.neighbors <- function(x, n = 10){
  x.order <- order(x[, "out"])
  head(x[x.order, ], n = n)
}
xn <- lapply(xs.base.rebal, .extract.neighbors, n = 5)


# table.AnnualizedReturns(opt.r)
# table.InformationRatio(opt.r[,1:3], opt.r[,'SPY'])

# roll.te <- rollapply(opt.r[,c('te.target', 'SPY')], width = 36, by.column = FALSE, 
#                      FUN = function(x) as.numeric(TrackingError(Ra = x[,1], Rb = x[,2])))
# plot(roll.te, main = "36 Month Rolling Tracking Error", 
#      grid.ticks.on = "years", major.ticks = "years")

# equal weight portfolio
# utility function to generate equal weights for a specified rebalance frequency
equal.weights <- function(R, rebalance_on = "months"){
  nc <- ncol(R)
  rebal.idx <- index(R[endpoints(R, on = rebalance_on, k = 1)])
  weight.mat <- matrix(1 / nc, nrow = length(rebal.idx), ncol = nc)
  out <- xts(weight.mat, rebal.idx)
  colnames(out) <- colnames(R)
  out
}

ew <- equal.weights(R, 'quarters')
ew.r <- Return.portfolio(R, ew)
colnames(ew.r) <- 'equal.weight'

# opt.r <- na.omit(cbind(opt.base.rebal.r,
#                        opt.box.rebal.r,
#                        opt.te.rebal.r,
#                        ew.r,
#                        R.mkt))
# charts.PerformanceSummary(opt.r)


# roll_te <- rollapply(opt.r[,c('box', 'SPY')], width = 36, by.column = FALSE, 
#                      FUN = function(x) as.numeric(TrackingError(Ra = x[,1], Rb = x[,2])))
# colnames(roll_te) <- paste(colnames(opt.r)[1], " to ", colnames(opt.r)[2], sep = "")
# plot(roll_te, main = "12 Month Rolling Tracking Error", grid.ticks.on = "years", major.ticks = "years", legend.loc = "topright")

#sim.base <- simulate.portfolio(R, rp, simulations = 1000, rebalance_on = rebal.period)
sim.base <- simulate.portfolio2(R['2002-03-31/'], rp, simulations = 5000, rebalance_on = rebal.period)

# sim.box <- simulate.portfolio(R, rp.box, simulations = 1000, rebalance_on = rebal.period)
sim.box <- simulate.portfolio2(R['2002-03-31/'], rp.box, simulations = 5000, rebalance_on = rebal.period)

# subset to the same index as the optimization rebalancing returns
# sim.base <- sim.base[index(opt.r)]
# sim.box <- sim.box[index(opt.r)]

sim <- cbind(sim.box, sim.base)
png(create.fig.path('sim_perf'), width = w.px, height = h.px, units = 'px')
charts.PerformanceSummary(sim, colorset = c(rep('salmon', ncol(sim.box)), 
                                            rep('gray', ncol(sim.base))), 
                          legend.loc = NULL, main = "Simulated Performance Summary")
legend('topleft', legend = c('constrained', 'unconstrained'), 
       fill = c('salmon', 'gray'), bty = 'n')
dev.off()


# performance statistics of the simulated portfolios
sim.base.sr <- sharpe.ratio.component(x = sim.base, scale = 12)
sim.base.ir <- information.ratio.component(x = sim.base, Rb = R.mkt, scale = 12)

sim.box.sr <- sharpe.ratio.component(x = sim.box, scale = 12)
sim.box.ir <- information.ratio.component(x = sim.box, Rb = R.mkt, scale = 12)

opt.r.stats <- table.AnnualizedReturns(opt.r)
opt.r.ir <- table.InformationRatio(opt.r[,1:3], Rb = opt.r[,'SPY'])

# Simulation: Sharpe Ratio
png(create.fig.path('sim_base_sr'), width = w.px, height = h.px, units = 'px')
chart.Histogram(sim.base.sr$sr, methods = c("add.density", "add.normal"),
                xlab = 'Annualized Sharpe Ratio',
                main = "Distribution of Annualized Sharpe Ratio: \nBase Portfolio Specification")
abline(v = opt.r.stats[3,c('base', 'te.target')], lty = 1:2)
text(x = opt.r.stats[3,c('base', 'te.target')], y = c(4, 3), 
     labels = colnames(opt.r.stats[,c('base', 'te.target')]), pos = 4)
dev.off()

png(create.fig.path('sim_box_sr'), width = w.px, height = h.px, units = 'px')
chart.Histogram(sim.box.sr$sr, methods = c("add.density", "add.normal"),
                xlab = 'Annualized Sharpe Ratio',
                main = "Distribution of Annualized Sharpe Ratio: \nBox Constrained Portfolio Specification")
abline(v = opt.r.stats[3,'box'], lty = 1)
text(x = opt.r.stats[3,'box'], y = 10, 
     labels = 'box', pos = 4)
dev.off()


# Simulation: Information Ratio
png(create.fig.path('sim_base_ir'), width = w.px, height = h.px, units = 'px')
chart.Histogram(sim.base.ir$ir, methods = c("add.density", "add.normal"),
                xlab = 'Information Ratio',
                main = "Distribution of Information Ratio: \nBase Portfolio Specification")
abline(v = opt.r.ir[3,c('base', 'te.target')], lty = 1:2)
text(x = opt.r.ir[3,c('base', 'te.target')], y = c(2, 1.5), 
     labels = colnames(opt.r.ir[,c('base', 'te.target')]), pos = 4)
dev.off()

png(create.fig.path('sim_box_ir'), width = w.px, height = h.px, units = 'px')
chart.Histogram(sim.box.ir$ir, methods = c("add.density", "add.normal"),
                xlab = 'Information Ratio',
                main = "Distribution of Information Ratio: \nBox Constrained Portfolio Specification")
abline(v = opt.r.ir[3,'box'], lty = 1)
text(x = opt.r.ir[3,'box'], y = 2, 
     labels = 'box', pos = 4)
dev.off()


# Simulation: Tracking Error
png(create.fig.path('sim_base_te'), width = w.px, height = h.px, units = 'px')
chart.Histogram(sim.base.ir$te, methods = c("add.density", "add.normal"),
                xlab = 'Annualised Tracking Error',
                main = "Distribution of Annualised Tracking Error: \nBase Portfolio Specification")
abline(v = opt.r.ir[2,c('base', 'te.target')], lty = 1:2)
text(x = opt.r.ir[2,c('base', 'te.target')], y = c(60, 60), 
     labels = colnames(opt.r.ir[,c('base', 'te.target')]), pos = 4)
dev.off()

png(create.fig.path('sim_box_te'), width = w.px, height = h.px, units = 'px')
chart.Histogram(sim.box.ir$te, methods = c("add.density", "add.normal"),
                xlab = 'Annualised Tracking Error',
                main = "Distribution of Annualised Tracking Error: \nBox Constrained Portfolio Specification")
abline(v = opt.r.ir[2,'box'], lty = 1)
text(x = opt.r.ir[2,'box'], y = 2, 
     labels = 'box', pos = 4)
dev.off()


#extractCovariance(statistical.factor.model(R = R, k = 4))
#cov(R)

# improve estimates
# library(rmgarch)
# univariate normal GARCH(1, 1)
#x.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
#                     variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),
#                     distribution.model = 'norm')
#m.spec <- multispec(replicate(ncol(R), x.spec))
# GARCH(1,1) DCC specification
#dcc.garch11.spec <- dccspec(uspec = m.spec, dccOrder = c(1, 1), distribution = 'mvnorm')
#dcc.garch11.fit <- dccfit(dcc.garch11.spec, data = R)
#dcc.forecast <- dccforecast(dcc.garch11.fit, n.ahead = 1)
#dcc.forecast
# 1-step ahead covariance matrix forecast
#rcov(dcc.forecast)[[1]]
# 1-step ahead correlation matrix forecast
#rcor(dcc.forecast)
