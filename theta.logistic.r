## Code by Erik Osnas, originally written May 2016, modified March 2021
## Fit theta logistic model to summer survey data including years up to 2019
## 20160509 modified priors in response to Josh Dooley's comments
## 20210322 (1) converted to jagsUI, 
##          (2) updated source of survey estimates from latest AKaerial results,
##          (3) added harvest data up through 2019 from AGFG harvest survey
##          (4) change uncertainty model for harvest data from 2016 on to use estimated SE
##          (5) found typo in sigma.proc and sigma.proc1, they were the same in earlier version, 
##              I removed sigma.proc1. This should not make any result change, just simplify code.
################################
# Data
library(jagsUI)
library(AKaerial)
library(dplyr)
ykd <- filter(YKGHistoric$combined, Species == "EMGO") %>% 
  select(Year, itotal, itotal.se)
dat <- read.csv(file="EMGO_data.csv") #harvest data sourced from ADFG harvest surveys after 2015
                                      #before 2015 from Dooley report (?)

# Specify Model
sink("theta.logistic.emgo.jags")
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
    #model of missing harvest data
    m.har ~ dnorm(4081, 0.00001)
    sigma.har ~ dunif(0, 3000)
    tau.har <- pow(sigma.har, -2)
    for(t in 1:T){ #T is number of data years before start of AMBCC season
      har.obs[t] ~ dnorm(m.har, tau.har)   #dbeta(214, 786)
      har[t] <- har.obs[t]/(1 - c)
    }
    mu.green ~ dlnorm(log(15000), 1/(0.2)^2) #prior for harvest under green policy
    for(h in 1:H){ #H is the number of harvest years, currently 3
      tau.sur[h] <- pow(sigma.sur[h], -2)
      har.sur[h] ~ dnorm(mu.green, tau.sur[h]) #likelihood
      har.obs[T+h] ~ dnorm(har.sur[h], tau.sur[h])T(0,)
      har[T+h] <- har.obs[T+h]/(1 - c)
    }
    # Likelihood
    # State process
    P[1] ~ dunif(0, 0.5)                      # Prior for initial population size
    P.true[1] ~ dlnorm(log(P[1]), tau.proc)
    for (t in 1:(T+H-1)){
    P[t+1] <- max(P.true[t] + r.max*P.true[t]*(1 - P.true[t]^theta) - har[t]/CC, 1e-5)
    P.true[t+1] ~ dlnorm(log(P[t+1]), tau.proc)
    }
    # Observation process
    for (t in 1:(T+H)) {
    logN.est[t] <- log(q) + log(P.true[t]) + log(CC)
    mu[t] <- logN.est[t] 
    tau.obs[t] <- pow(sigma.obs[t],-2)
    y[t] ~ dnorm(exp(mu[t]), tau.obs[t])
    }
    
    # Population sizes on real scale
    for (t in 1:(T+H)) {
    N.est[t] <- exp(logN.est[t])
    N.tot[t] <- N.est[t]/q
    }
    }
    ",fill = TRUE)
sink()
# Set up data list
jags.data <- list(y = ykd$itotal,  
                  sigma.obs = ykd$itotal.se,
                  har.obs = c(dat$EMGO_HARV[-c(33:35)], rep(NA, 3)),
                  har.sur = dat$EMGO_HARV[33:35],
                  sigma.sur = dat$EMGO_HARV[33:35]*dat$CIP[33:35]/200, #data is in CIP = 2*cv, App P, eq. 6a from ADFG report
                  T = 32, #T is the number of years in data before start of current AMBCC season
                  H = 3)  #H is the number of harvest years
# Initial values
inits <- function(){list(
  q = runif(1, 0.1, 0.12),
  c = runif(1, 0.20, 0.25),
  r.max = runif(1, 0.15, 0.16),
  theta = runif(1, 1.01, 1.02), 
  CC = runif(1, 200000, 200100),
  sigma.proc = runif(1, 0.01, 0.02),
  m.har = runif(1, 4000, 4100),
  sigma.har = runif(1, 101, 200)
)}
# Parameters monitored
parameters <- c("r.max", "sigma.proc", "N.est", "CC", "theta", "q", "N.tot", "mu.green")
# Call JAGS from R
out <- jags(jags.data, inits, parameters, "theta.logistic.emgo.jags", 
            n.chains = 3, n.thin = 100, n.iter = 1100000, n.burnin = 1000000, n.adapt=2000, 
            parallel=TRUE)
# out <- jags(jags.data, inits, parameters, "theta.logistic.emgo.jags", 
#                      n.chains = 3, n.thin = 100, n.iter = 1100000, n.burnin = 1000000, working.directory = getwd())
plot(out)
saveRDS(out, file = "summer_theta_logistic_2021.RDS")
out <- readRDS("summer_theta_logistic_2021.RDS")
#plot population time series and estimate
x = rep(min(ykd$Year):max(ykd$Year), each=length(out$sims.list$CC))
y=rep(out$sims.list$CC, times=length(ykd$Year))
smoothScatter(cbind(x,y), nrpoints = 0, ylim=c(0,400000), ylab="Summer Total", xlab="Year", 
              bandwidth = c(1,1))
ci = quantile(out$sims.list$CC, probs=c(0.025, 0.5, 0.975))
abline(h = ci[1], lwd=2, lty=1, col="white")
abline(h = ci[3], lwd=2, lty=1, col="white")
abline(h = ci[2], lwd=2, lty=2, col="white")
ci = apply(out$sims.list$N.tot, 2, quantile, probs=c(0.025, 0.5, 0.975))
arrows(x0=ykd$Year, x1=ykd$Year, y0=ci[1,],y1=ci[3,], length=0)
points(ykd$Year, ci[2,], pch=22, bg=1)


plot(ykd$Year, ykd$itotal, pch=16, ylim=c(0, 50000),  ylab="YKD indicated breeding total birds", 
     xlab="Year", main="Emperor goose population, harvest, and state-space model")
arrows(x0=ykd$Year, x1=ykd$Year, y0=ykd$itotal - 2*ykd$itotal.se,y1=ykd$itotal + 2*ykd$itotal.se, 
       length=0)
arrows(x0=ykd$Year+0.1, x1=ykd$Year+0.1, y0=out$mean$N.est - 2*out$sd$N.est,
       y1=out$mean$N.est + 2*out$sd$N.est, length=0)
points(ykd$Year+0.1, out$mean$N.est, pch=21, bg="gray50" )
points(1985:2016, dat$EMGO_HARV[-c(33:35)], col=2, pch=16)
points(2017:2019, dat$EMGO_HARV[33:35], pch=16, col=3)
segments(x0=2017, x1=2019, y0=out$mean$mu.green, col=3)
segments(x0=2017, x1=2019, y0=out$q2.5$mu.green, col=3, lty=2)
segments(x0=2017, x1=2019, y0=out$q97.5$mu.green, col=3, lty=2)
arrows(x0=2017:2019, x1=2017:2019, y0=(dat$EMGO_HARV[33:35] - (dat$CIP*dat$EMGO_HARV/100)[33:35]),
       y1=(dat$EMGO_HARV[33:35] + (dat$CIP*dat$EMGO_HARV/100)[33:35]), 
       length=0, col="lightgreen")
arrows(x0=2016, x1=2016, y0=(dat$EMGO_HARV[32] - (dat$CIP*dat$EMGO_HARV/100)[32]), 
       y1=(dat$EMGO_HARV[32] + (dat$CIP*dat$EMGO_HARV/100)[32]), 
       length=0, col="red")
legend("topleft", legend=c("Survey Estimate", "State-space model", "Historical harvest data", 
                           "AMBCC harvest survey-C&T season"), 
       pch=16, col=c(1, "gray50", 2, 3))

