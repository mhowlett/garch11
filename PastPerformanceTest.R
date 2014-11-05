
sp500 <- read.csv("rshare/spy.txt")

library("bayesGARCH")
# provides GARCH(1,1) parameter estimation with t-distribution innovations. 
# reference manual provides model definition.
# http://cran.r-project.org/web/packages/bayesGARCH/index.html

# this scale factor is required to avoid convergence problems in bayesGARCH
scaleFactor <- 100
f <- diff(log(sp500[["values"]])) * scaleFactor
# r <- bayesGARCH(g, control=list(l.chain = 30000))
# alpha0 <- median(r[[1]][,1][10000:30000])
# alpha1 <- median(r[[1]][,2][10000:30000])
# beta <- median(r[[1]][,3][10000:30000])
# nu <- median(r[[1]][,4][10000:30000])

# parameters generated using the above for S&P 500 Total Return
alpha0 <- 0.01576851
alpha1 <- 0.09751042
beta <- 0.8965093
nu <- 6.912451

# sigma <- sd(f/scaleFactor)
sigma <- 0.01283941

nextGARCH <- function(prev) {
  h_tmp <- alpha0 + alpha1*prev[["y"]]^2 + beta*prev[["h"]]
  y_tmp <- rt(1,nu,0) * sqrt(((nu-2)/nu)*h_tmp)
  c(y = y_tmp, h = h_tmp)
}

genGARCH <- function(n, start = c(h=0, y=0)) {
  result <- double(n)
  prev <- start
  for (i in 1:n) {
    cur <- nextGARCH(prev)
    # sigma thing is a hack - todo work out correct factor here
    result[i] = cur[["y"]]/scaleFactor - sigma*sigma/2
    prev <- cur
  }
  result
}

genNormal <- function(n) {
  rnorm(n, mean=-sigma*sigma/2, sd = sigma)

  # the fact that without the -sigma^2/2 offset it doesn't work is obvious with 
  # a little bit of thought, but the exact factor wasn't to me. The belew from:
  # http://www.epixanalytics.com/modelassist/AtRisk/Model_Assist.htm#Extra_example_models/Finance/Modelling_lognormal_properties_of_stock_prices.htm
  
  # ASIDE: If you attempted to guess the above formula from Central Limit Theorem 
  # principles you will be surprised to see the inclusion of a s2/2 component in 
  # Equation 2 for the mean return. It is there because the return in each short 
  # period is a function of the stock price at that moment, and therefore cannot 
  # be simply added up in log space. The proof of the formula is outside the scope
  # of this guide: it is a result of applying Ito’s Lemma (see, for example Hull
  # (1993, page 208), a mathematical result crucial in Black and Scholes work on
  # valuing of options.
}

timeseriesFromReturns <- function(rets){
  nn <- length(rets);
  result <- double(length(rets) + 1)
  result[1] = 0;
  for (i in 1:nn) {
    result[i+1] <- result[i] + rets[i]
  }
  exp(result)
}

getDaysInverseSpacing <- function(forecastPeriodDays, everyDayPeriod)
{
  days <- numeric()
  day <- 1
  while (day < forecastPeriodDays)
  {
    days <- append(days, day)
    day <- day + 1 + round(day/everyDayPeriod)
  }
  days <- append(days, forecastPeriodDays)
  as.integer(days)
}

days <- getDaysInverseSpacing(3650, 4.1)

runSimulation <- function(nsim, days, gen) {
  result = matrix(nrow=nsim, ncol=length(days))
  for (i in 1:nsim) {
    sim_rets <- gen(5000)
    sim_ts <- timeseriesFromReturns(sim_rets)
    for (j in 1:length(days))
    {
      result[i, j] = sim_ts[days[j]]
    }
  }
  result
}

plotSimResult <- function(result) {
  expected <- numeric(length(days))
  lower <- numeric(length(days))
  upper <- numeric(length(days))
  for (i in 1:length(days)){
    expected[i] <- mean(result[, i])
    lower[i] <- quantile(result[, i], 0.05)
    upper[i] <- quantile(result[, i], 0.95)
  }
  plot(days, expected, type='l', col='green', ylim=c(0, 5))
  lines(days, lower, col='red')
  lines(days, upper, col='red')
}

