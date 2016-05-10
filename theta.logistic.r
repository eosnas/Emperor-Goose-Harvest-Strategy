require(R2jags)
## Fit theta logistic model to summer survey data including year 2015
## 5/9/16 modified priors in response to Josh Dooley's comments
################################
# Data
require(R2jags)
dat <- read.csv(file="EMGO_Logistic_v2.csv")

# Specify Model
sink("theta.logistic.emgo.2.jags")
cat("
    model{
    # Priors and constraints
    q ~ dunif(0.01, 1) #harvest/population adjustment factor, #dunif(0.01, 1) worked
    #dbeta(1, 5) or dunif(0, 1) or dbeta(2, 5)--did not converge in 1mil.   
    c ~ dbeta(50, 150)    # c <- 0.25 for constant, distribution approx. from Dooley
    r.max ~  dlnorm(log(0.09), 0.3)T(0, 0.2) #Prior for r max, approximated from Dooley report & pers. comm. on 5/9/16
    # allows some very high values to r.max > 0.3, try truncation
    #also tried: dunif(0.05, 0.25), dunif(0, 0.2), and dunif(0, 0.3)
    theta ~ dlnorm(log(1.76), 0.25)T(1, 3)  #theta-logistic parameter from Dooley report & pers. comm. on 5/9/16
    # allow much too high theta values > 100 in posterior, try truncation 
    #also used dunif(1,3)
    CC ~ dunif(50000, 400000) #<-- converged but gives very high CC and upper tail on N.tot
    #dunif(100000,250000) works but dlnorm(log(200000), 0.2)T(0, 400000) does not 
    sigma.proc ~ dunif(0, 0.3)                    # Prior for sd of state process
    tau.proc <- pow(sigma.proc, -2)
    sigma.proc1 ~ dunif(0, 0.1)                    # Prior for sd of state process
    tau.proc1 <- pow(sigma.proc, -2)
    #model of missing harvest data
    m.har ~ dnorm(4081, 0.00001)
    sigma.har ~ dunif(0, 3000)
    tau.har <- pow(sigma.har, -2)
    for(t in 1:T){
      har.obs[t] ~ dnorm(m.har, tau.har)   #dbeta(214, 786)
      har[t] <- har.obs[t]/(1 - c)
    }
    # Likelihood
    # State process
    P[1] ~ dunif(0, 0.5)                      # Prior for initial population size
    P.true[1] ~ dlnorm(log(P[1]), tau.proc1)
    for (t in 1:(T-1)){
    P[t+1] <- max(P.true[t] + r.max*P.true[t]*(1 - P.true[t]^theta) - har[t]/CC, 1e-5)
    P.true[t+1] ~ dlnorm(log(P[t+1]), tau.proc)
    }
    # Observation process
    for (t in 1:T) {
    logN.est[t] <- log(q) + log(P.true[t]) + log(CC)
    mu[t] <- logN.est[t] 
    tau.obs[t] <- pow(sigma.obs[t],-2)
    y[t] ~ dnorm(exp(mu[t]), tau.obs[t])
    }
    
    # Population sizes on real scale
    for (t in 1:T) {
    N.est[t] <- exp(logN.est[t])
    N.tot[t] <- N.est[t]/q
    #y.new[t] ~ dnorm(exp(mu[t]), tau.obs[t])
    #y.tot[t] <- y.new[t]/q
    }
    }
    ",fill = TRUE)
sink()
# Set up data list
jags.data <- list(y = dat$SUM_TOT,  
                  har.obs = dat$EMGO_HARV,
                  sigma.obs = dat$SUM_TOT_SE,
                  T = length(dat$SUM_TOT) )
# Initial values
inits <- function(){list(
  q = runif(1, 0.1, 0.12),
  c = runif(1, 0.20, 0.25),
  r.max = runif(1, 0.15, 0.16),
  theta = runif(1, 1.01, 1.02), 
  CC = runif(1, 200000, 200100),
  sigma.proc = runif(1, 0.01, 0.02),
  sigma.proc1 = runif(1, 0.01, 0.02),
  m.har = runif(1, 4000, 4100),
  sigma.har = runif(1, 101, 200)
)}
# Parameters monitored
parameters <- c("r.max", "sigma.proc", "N.est", "CC", "theta", "q", "N.tot", "c", "har", "m.har", "sigma.har")
# Call JAGS from R
out.1 <- jags.parallel(jags.data, inits, parameters, "theta.logistic.emgo.2.jags", 
                     n.chains = 3, n.thin = 100, n.iter = 1100000, n.burnin = 1000000, working.directory = getwd())
out = out.1
plot(out)
dput(out.1$BUGSoutput$sims.list, file = "summer_theta_logistic_output.2.txt")
#plot population time series and estimate
x = rep(min(dat$YR):max(dat$YR), each=dim(out$BUGSoutput$sims.list$CC)[1])
y=rep(out$BUGSoutput$sims.list$CC, times=length(dat$YR))
smoothScatter(cbind(x,y), nrpoints = 0, ylim=c(0,400000), ylab="Summer Total", xlab="Year", bandwidth = c(1,1))
ci = quantile(out$BUGSoutput$sims.list$CC, probs=c(0.025, 0.5, 0.975))
abline(h = ci[1], lwd=2, lty=1, col="white")
abline(h = ci[3], lwd=2, lty=1, col="white")
abline(h = ci[2], lwd=2, lty=2, col="white")
ci = apply(out$BUGSoutput$sims.list$N.tot, 2, quantile, probs=c(0.025, 0.5, 0.975))
arrows(x0=dat$YR, x1=dat$YR, y0=ci[1,],y1=ci[3,], length=0)
points(dat$YR, ci[2,], pch=22, bg=1)

plot(dat$YR, dat$SUM_TOT, pch=16, ylim=c(0, 50000),  ylab="Summer Total", xlab="Year")
arrows(x0=dat$YR, x1=dat$YR, y0=dat$SUM_TOT - 2*dat$SUM_TOT_SE,y1=dat$SUM_TOT + 2*dat$SUM_TOT_SE, length=0)
points(dat$YR, dat$SUM_TOT, pch=16)
arrows(x0=dat$YR+0.1, x1=dat$YR+0.1, y0=out$BUGSoutput$mean$N.est - 2*out$BUGSoutput$sd$N.est,
       y1=out$BUGSoutput$mean$N.est + 2*out$BUGSoutput$sd$N.est, length=0)
points(dat$YR+0.1, out$BUGSoutput$mean$N.est, pch=21, bg="gray50" )
points(dat$YR, jags.data$har.obs, col=2, pch=16)
ci = apply(out$BUGSoutput$sims.list$har, 2, quantile, probs=c(0.025, 0.5, 0.975))
arrows(x0=dat$YR, x1=dat$YR, y0=ci[1,],y1=ci[3,], length=0)
points(dat$YR, ci[2,], pch=22, bg=3)
