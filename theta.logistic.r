## Code by Erik Osnas, originally written May 2016, modified March 2021, November 2022 for NAAG conference
## Fit theta logistic model to summer survey data including years up to 2019
## 20160509 modified priors in response to Josh Dooley's comments
## 20210322 (1) converted to jagsUI, 
##          (2) updated source of survey estimates from latest AKaerial results,
##          (3) added harvest data up through 2019 from AGFG harvest survey
##          (4) change uncertainty model for harvest data from 2016 on to use estimated SE
##          (5) found typo in sigma.proc and sigma.proc1, they were the same in earlier version, 
##              I removed sigma.proc1. This should not make any result change, just simplify code.
## 20210630 (1) changed harvest data source to include standard errors from ADFG harvest surveys, 
##              sourced data from https://www.adfg.alaska.gov/index.cfm?adfg=subsistence.b_annual;
##              from 1985 - 2003 from YKD Goose MP Surveys; 2004-2013 from electronic download at site above;
##              from 2014 - 2019 from annual reports
## 20210707 (1) modified model to incorporate standard error of harvest data for all years
##          (2) modified model to allow missing population survey data for 2020, see https://github.com/USFWS/State-Space-Prediction-2020
## 20221104 (1) modified model and data to include 2022 data
##          (2) changed mean of m.har to 2093 to reflect mean of observed harvest, used 4081 in 2016, not sure why. 
##          (3) added different prior for mu.green with lower mean (5693) than used in 2016. It can be commented out.
## 20221115 (1) changed model to accept parameter for mu.green prior so that different priors can be used, 
##              one with original prior and one with lower mean. 
## 20221116 (1) increased iters for convergence; need more for out2 than for out
## 20221118 (1) added posterior for original data/model; used to see what was learned; higher harvest prior
## 20221122 (1) original model was in terms of kill for prior of red, yellow, 
##              and green seasons; revised these priors to be in term of harvest and used crippling prior as in Dooley.   
## 20221123 (1) Added back in harvest data used in 2016 and 2021. Harvest number pre-2017 are much larger; need to resolve. 
##              Modified prior back for mean harvest pre-2017 to 4081.
##          (2) Used SE for har below as uncertainty measure for original data. 
################################
# Data
library(jagsUI)
#library(AKaerial)
library(tidyverse)
ykd <- read_csv(file="data/YKG1985to2022Combined.csv") %>%
  filter(Species == "EMGO") %>% 
  select(Year, itotal, itotal.se)
harOriginal <- read_csv(file = "data/EMGO_data.csv")
#plot
ggplot(data=ykd) + 
  geom_pointrange(aes(x=Year, y=itotal, ymin=itotal-2*itotal.se, ymax=itotal+2*itotal.se)) + 
  geom_hline(yintercept = 23000)
################################################################################
## Harvest data, wrangle 
harv1 <- read_csv(file="data/emgo_harvest_1985_2003.csv")  #harvest data sourced from YKD Goose Man. Survey, ADFG
harv2 <- read_csv(file="data/emgo_harvest_export_2004_2013.csv") #harvest data downloaded from ADFG on 20210630
harv3 <- read_csv(file="data/emgo_harvest_2014_2019.csv") #typed in from report tables, not available for download 

#reformat harv1
harv1 <- harv1 %>% rename(RegionalHarvest = Harvest) %>%
  group_by(Year) %>%
  summarise(Harvest = sum(RegionalHarvest, na.rm=TRUE), SE = sqrt(sum(SE^2, na.rm = TRUE)))
#estimate is zero in 1988 and 2003, YKD had no reported harvest, remove
harv1 <- filter(harv1, !Year %in% c(1988, 2003:2005)) %>% #2005 is low but non-zero estimates for YKD and BB, 
  # leave in? Josh deleted in 2015; here delete for consistency; 2004 is in data download (harv2)
  select(Year, Harvest, SE) %>%
  mutate(Type="Goose Management Plan", `Spring Season`=ifelse(Year<=1986, "Open", "Closed"))
#reformat harv2
  #some rows contain summary by region ("All Regions")--all but 2012 and 2013:
  #appears that in 2012 the only data is for NW Arctic and St Lawrence, remove 2012; 
  #for 2013 there is no 'All subregion' summary, need to summarize that year
  #for Bering Strait, only 'All subregions' is given
harv2013 <- harv2 %>% filter(Year==2013) %>%
  filter(Year != 2012, `Sub-region` != "All subregions") %>%
  select(Year, Region, `Sub-region`, contains("Annual")) %>%
  rename(RegionalHarvest = "Annual Harvest", CIP = "CIP (Annual)") %>%
  mutate(CIP = as.numeric(substr(CIP, 1, 4)), Var = (RegionalHarvest*CIP/200)^2) %>% #remove percent sign and convert to numeric
  group_by(Year) %>%
  summarise(Harvest = sum(RegionalHarvest, na.rm=TRUE), SE = sqrt(sum(Var, na.rm = TRUE))) %>% #CIP = 2*CV = 2*SE/mean
  ungroup() %>% select(Year, Harvest, SE)  %>%
  mutate(Type="ADFG 2004-2015", `Spring Season`="Closed")
 #do the rest
harv2 <- harv2 %>% 
  filter( !Year %in%c(2012, 2013), `Sub-region` == "All subregions") %>%
  select(Year, Region, `Sub-region`, contains("Annual")) %>%
  rename(RegionalHarvest = "Annual Harvest", CIP = "CIP (Annual)") %>%
  mutate(CIP = as.numeric(substr(CIP, 1, 4)), Var = (RegionalHarvest*CIP/200)^2) %>% #remove percent sign and convert to numeric
  group_by(Year) %>%
  summarise(Harvest = sum(RegionalHarvest, na.rm=TRUE), SE = sqrt(sum(Var, na.rm = TRUE))) %>% #CIP = 2*CV = 2*SE/mean
  ungroup() %>% select(Year, Harvest, SE)  %>%
  mutate(Type="ADFG 2004-2015", `Spring Season`="Closed") %>% 
  bind_rows(harv2013)
#reformat harv3
harv3 <- harv3 %>%  rename(RegionalHarvest = Harvest) %>% #BBR and BSNS was not sampled in 2015
  mutate(Var = (RegionalHarvest*CIP/200)^2) %>%
  group_by(Year) %>%
  summarise(Harvest = sum(RegionalHarvest), SE = sqrt(sum(Var))) %>%
  ungroup() %>% drop_na() %>%
  bind_cols(data.frame(Type=c("ADFG 2004-2015",rep("5 Region Design",4)))) %>%
  mutate(`Spring Season` = ifelse(Year <= 2016, "Closed", "Open"))
har <- bind_rows(harv1, harv2, harv3) %>%
  mutate(lower=Harvest-2*SE, upper=Harvest+2*SE)
# above might be improved by imputation for missing data/regions
##plot harvest data
ggplot(har, aes(x=Year, Y=Harvest, shape=Type, color=`Spring Season`)) +
  geom_pointrange(aes(x=Year, y=Harvest, ymin=lower, ymax=upper))
#plot harvest and survey data together
ggplot(data=ykd) + 
  geom_pointrange(aes(x=Year, y=itotal, ymin=itotal-2*itotal.se, ymax=itotal+2*itotal.se)) + 
  geom_hline(yintercept = 23000) + 
  geom_pointrange(data = har, aes(x=Year, y=Harvest, ymin=lower, ymax=upper, color=`Spring Season`))
################################################################################
#plot air survey data
Combined <- ykd  %>%
  mutate(X="Combined", Observer = NA)
Observers <- read_csv("data/YKG1985to2022Expanded.csv") %>% #read from file until AKaerial is updated
  filter(Species=="EMGO") %>% 
  select(Year, strata, Observer, itotal.est, var.Ni) %>% 
  mutate(fYear = factor(Year), strata=factor(strata)) %>% 
  group_by(Year, Observer) %>% summarise(itotal=sum(itotal.est), itotal.se=sqrt(sum(var.Ni))) %>%
  mutate(X="By Observer")

df <- bind_rows(Observers, Combined)
gplot <- ggplot() + 
  geom_pointrange(data=df, aes(x=Year, y=itotal, ymin=itotal-2*itotal.se, ymax=itotal+2*itotal.se,
                                color=Observer), position=position_dodge(width=0.25)) + 
  geom_hline(yintercept = 23000) + 
  #geom_segment(aes(x=2017, xend=2021, y=23000, yend=23000)) +
  scale_color_discrete(breaks=levels(df$Observer)) +
  facet_wrap(~X, ncol = 1) + 
  labs(x="Year", y="Indicated Total Birds")
print(gplot)
################################################################################
#fit linear model for missing data population data
#estimate average CV of index and mse around cv expectation
fit <- summary(lm(ykd$itotal.se~ykd$itotal-1))
################################################################################
# Specify JAGS Model
sink("theta.logistic.emgo.jags")
cat("
    model{
    # Priors and constraints
    q ~ dunif(0.01, 1) #harvest/population adjustment factor, #dunif(0.01, 1) worked
    #dbeta(1, 5) or dunif(0, 1) or dbeta(2, 5)--did not converge in 1mil.   
    c ~ dbeta(50, 150)    # c <- 0.25 for constant, distribution approx. from Dooley
    r.max ~  dlnorm(log(0.09), 0.3)T(0, 0.2) #Prior for r max, approximated from Dooley report & 
    #  pers. comm. on 5/9/16; allows some very high values to r.max > 0.3, try truncation;
    #  also tried: dunif(0.05, 0.25), dunif(0, 0.2), and dunif(0, 0.3)
    theta ~ dlnorm(log(1.76), 0.25)T(1, 3)  #theta-logistic parameter from Dooley report & pers. comm. on 5/9/16
    # allow much too high theta values > 100 in posterior, try truncation 
    #also used dunif(1,3)
    CC ~ dunif(50000, 400000) #<-- converged but gives very high CC and upper tail on N.tot
    #dunif(100000,250000) works but dlnorm(log(200000), 0.2)T(0, 400000) does not 
    sigma.proc ~ dunif(0, 0.3)                    # Prior for sd of state process
    tau.proc <- pow(sigma.proc, -2)
    #hierarchical model of missing harvest data: 1988, 2003, 2012, 2014, 2020, 2021, 2022
    m.har ~ dnorm(4081, 0.00001) #mean harvest before AMBCC season; parameterized as kill in 2016
    sigma.har ~ dunif(0, 3000) #process SD in harvest across years, shared between all years
    for(t in 1:3){ 
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    har[4] ~ dnorm(m.har, 1/sigma.har^2) #year 1988, latent state process harvest
    kill[4] <- har[4]/(1 - c)
    for(t in 5:18){ 
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    har[19] ~ dnorm(m.har, 1/sigma.har^2) #year 2003
    kill[19] <- har[19]/(1 - c)
    for(t in 20:27){ 
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    
    har[28] ~ dnorm(m.har, 1/sigma.har^2) #year 2012 missing,
    kill[28] <- har[28]/(1 - c)
    tau.sur[29] <- pow(sigma.sur[29], -2)
    har[29] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest year 2013 in data,
    har.sur[29] ~ dnorm(har[29], tau.sur[29]) #likelihood
    kill[29] <- har[29]/(1 - c)
    har[30] ~ dnorm(m.har, 1/sigma.har^2) #year 2014 missing,
    kill[30] <- har[30]/(1 - c)
    
    for(t in 31:T){ #T is last data year before start of AMBCC season
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    mu.green ~ dlnorm(pmu.mean, pmu.tau) #prior for harvest under green policy

    for(h in 1:3){ #h is the number of harvest years with data, currently 3 are observed
      tau.sur[T+h] <- pow(sigma.sur[T+h], -2)
      har[T+h] ~ dnorm(mu.green, 1/sigma.har^2) #latent state process harvest
      har.sur[T+h] ~ dnorm(har[T+h], tau.sur[T+h]) #likelihood
      kill[T+h] <- har[T+h]/(1 - c) #translate harvest to kill
    }
    for( h in 4:H){
      har[T+h] ~ dnorm(mu.green, 1/sigma.har^2) #years 2020 to 2022
      kill[T+h] <- har[T+h]/(1 - c)
    }
    # Likelihood
    # State process
    P[1] ~ dunif(0, 0.5)                      # Prior for initial population size
    P.true[1] ~ dlnorm(log(P[1]), tau.proc)
    for (t in 1:(T+H-1)){
    fk[t] <- (1 - exp(-0.0001*P.true[t]*CC))*kill[t]  #functional response of hunters
    P[t+1] <- max(P.true[t] + r.max*P.true[t]*(1 - P.true[t]^theta) - fk[t]/CC, 1e-5)
    P.true[t+1] ~ dlnorm(log(P[t+1]), tau.proc)
    }
    # Observation process
    for (t in 1:(T+3)) {
    logN.est[t] <- log(q) + log(P.true[t]) + log(CC)
    mu[t] <- logN.est[t] 
    tau.obs[t] <- pow(sigma.obs[t],-2)
    y[t] ~ dnorm(exp(mu[t]), tau.obs[t])
    }
    #year 2020, missing
    logN.est[T+4] <- log(q) + log(P.true[T+4]) + log(CC)
    mu[T+4] <- logN.est[T+4] 
    # predict new observation, see https://github.com/USFWS/State-Space-Prediction-2020
    beta.se ~ dnorm(BETA, 1/SE^2)           #from linear model
    s ~ dchisq(DF) 
    sigma.obs.missing ~ dnorm(beta.se*exp(mu[T+4]), s/((SIGMA^2)*DF) ) #from linear model
    tau.obs[T+4] <- pow(sigma.obs.missing,-2)
    y[T+4] ~ dnorm(exp(mu[T+4]), tau.obs[T+4])
    #year 2021 and 2022
    for(t in 5:H){
    logN.est[T+t] <- log(q) + log(P.true[T+t]) + log(CC)
    mu[T+t] <- logN.est[T+t] 
    tau.obs[T+t] <- pow(sigma.obs[T+t],-2)
    y[T+t] ~ dnorm(exp(mu[T+t]), tau.obs[T+t])
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
#need to augment harvest data with NAs for missing years
har.na <- har %>% select(Year, Harvest, SE) %>% 
  full_join(data.frame(Year=1985:2022)) %>% 
  arrange(Year)
#swap out harvest data with original until I can figure this out
har.na$Harvest[1:32] <- harOriginal$EMGO_HARV[1:32]
ykd <- ykd %>% full_join(data.frame(Year=1985:2022)) %>% 
  arrange(Year)
jags.data <- list(y = ykd$itotal,  
                  sigma.obs = ykd$itotal.se,
                  har.sur = har.na$Harvest, 
                  sigma.sur = har.na$SE,
                  pmu.mean = log(11250), pmu.tau = 1/(0.2)^2, #original (2016) mean and se for mu.green prior
                  T = 32, #T is the number of years in data before start of current AMBCC season
                  H = 6,  #H is the number of harvest years
                  DF = fit$df[2], SIGMA = fit$sigma, #linear model for missing data
                  BETA=fit$coefficients[1], SE=fit$coefficients[2])
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
parameters <- c("r.max", "sigma.proc", "N.est", "CC", "theta", "q", "N.tot", 
                "mu.green", "har", "m.har", "sigma.har", "c")
# Call JAGS from R
#out is with original prior on mu.green
out1 <- jags(jags.data, inits, parameters, "theta.logistic.emgo.jags", 
            n.chains = 4, n.thin = 2, n.iter = 1000000, n.burnin = 900000, n.adapt=10000,
            parallel=TRUE)
saveRDS(out1, file = "out1.harOriginal.RDS")
## could not get convergence in 500K iters for CC and q parameters, try more
# took 900K for convergence
# in hindsight (in 2022), above prior seems way too high, what if this is lowered?
jags.data2 <- jags.data
jags.data2$pmu.mean = log(5693) #lowered to mean of harvest in 2017 to 2019
out2 <- jags(jags.data2, inits, parameters, "theta.logistic.emgo.jags", 
            n.chains = 4, n.thin = 4, n.iter = 1200000, n.burnin = 1000000, n.adapt=10000, 
            parallel=TRUE)
# needed above iters to converge
saveRDS(out2, file = "out2.harOriginal.RDS")

# #plot population time series and estimate
# out <- readRDS("out.RDS")
# x = rep(min(ykd$Year):max(ykd$Year), each=length(out$sims.list$CC))
# y=rep(out$sims.list$CC, times=length(ykd$Year))
# smoothScatter(cbind(x,y), nrpoints = 0, ylim=c(0,400000), ylab="Summer Total", xlab="Year", 
#               bandwidth = c(1,1))
# ci = quantile(out$sims.list$CC, probs=c(0.025, 0.5, 0.975))
# abline(h = ci[1], lwd=2, lty=1, col="white")
# abline(h = ci[3], lwd=2, lty=1, col="white")
# abline(h = ci[2], lwd=2, lty=2, col="white")
# ci = apply(out$sims.list$N.tot, 2, quantile, probs=c(0.025, 0.5, 0.975))
# arrows(x0=ykd$Year, x1=ykd$Year, y0=ci[1,],y1=ci[3,], length=0)
# points(ykd$Year, ci[2,], pch=22, bg=1)
# 
# 
# plot(ykd$Year, ykd$itotal, pch=16, ylim=c(0, 50000),  ylab="YKD indicated breeding total birds", 
#      xlab="Year", main="Emperor goose population, harvest, and state-space model")
# arrows(x0=ykd$Year, x1=ykd$Year, y0=ykd$itotal - 2*ykd$itotal.se,y1=ykd$itotal + 2*ykd$itotal.se, 
#        length=0)
# arrows(x0=ykd$Year+0.1, x1=ykd$Year+0.1, y0=out$mean$N.est - 2*out$sd$N.est,
#        y1=out$mean$N.est + 2*out$sd$N.est, length=0)
# points(ykd$Year+0.1, out$mean$N.est, pch=21, bg="gray50" )
# points(1985:2016, har.na$Harvest[-c(33:38)], pch=1, col="black")
# points(2017:2022, har.na$Harvest[33:38], pch=22, col=1, bg=1)
# segments(x0=2017, x1=2022, y0=out$mean$mu.green, col=3)
# segments(x0=2017, x1=2022, y0=out$q2.5$mu.green, col=3, lty=2)
# segments(x0=2017, x1=2022, y0=out$q97.5$mu.green, col=3, lty=2)
# arrows(x0=2017:2022, x1=2017:2022, y0=(har.na$Harvest[33:38] - 2*(har.na$SE)[33:38]),
#        y1=(har.na$Harvest[33:38] + 2*(har.na$SE)[33:38]), 
#        length=0, col="darkgray")
# arrows(x0=2017:2022+0.1, x1=2017:2022+0.1, y0=out$q2.5$har[33:38], y1=out$q97.5$har[33:38], 
#        length=0, col="darkgray")
# arrows(x0=har.na$Year[-c(33:38)], x1=har.na$Year[-c(33:38)], y0=(har.na$Harvest[-c(33:38)] - 2*har.na$SE[-c(33:38)]),
#        y1=(har.na$Harvest[-c(33:38)] + 2*har.na$SE[-c(33:38)]), 
#        length=0, col="black")
# points(2017:2022+0.1, out$mean$har[33:38], pch=16, col="darkgray")
# legend("topleft", legend=c("Survey Estimate", "State-space model", "Historical harvest data", 
#                            "AMBCC harvest survey-C&T season"), 
#        pch=c(16, 16, 1, 22), col=c(1, "gray50", 1, 1), pt.bg=c(NA, NA, NA, 1))
################################################################################
##
## Now fit the model with only data up to 2016 and with 2016 prior.
## This will give us the state of knowledge in 2016, given the current model (with small updates)
# Specify JAGS Model
sink("theta.logistic.emgo.2016.jags")
cat("
    model{
    # Priors and constraints
    q ~ dunif(0.01, 1) #harvest/population adjustment factor, #dunif(0.01, 1) worked
    #dbeta(1, 5) or dunif(0, 1) or dbeta(2, 5)--did not converge in 1mil.   
    c ~ dbeta(50, 150)    # c <- 0.25 for constant, distribution approx. from Dooley
    r.max ~  dlnorm(log(0.09), 0.3)T(0, 0.2) #Prior for r max, approximated from Dooley report & 
    #  pers. comm. on 5/9/16; allows some very high values to r.max > 0.3, try truncation;
    #  also tried: dunif(0.05, 0.25), dunif(0, 0.2), and dunif(0, 0.3)
    theta ~ dlnorm(log(1.76), 0.25)T(1, 3)  #theta-logistic parameter from Dooley report & pers. comm. on 5/9/16
    # allow much too high theta values > 100 in posterior, try truncation 
    #also used dunif(1,3)
    CC ~ dunif(50000, 400000) #<-- converged but gives very high CC and upper tail on N.tot
    #dunif(100000,250000) works but dlnorm(log(200000), 0.2)T(0, 400000) does not 
    sigma.proc ~ dunif(0, 0.3)                    # Prior for sd of state process
    tau.proc <- pow(sigma.proc, -2)
    #hierarchical model of missing harvest data: 1988, 2003, 2012, 2014, 2020, 2021, 2022
    m.har ~ dnorm(4081, 0.00001) #mean harvest before AMBCC season; parameterized as kill in 2016
    sigma.har ~ dunif(0, 3000) #process SD in harvest across years, shared between all years
    for(t in 1:3){ 
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    har[4] ~ dnorm(m.har, 1/sigma.har^2) #year 1988, latent state process harvest
    kill[4] <- har[4]/(1 - c)
    for(t in 5:18){ 
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    har[19] ~ dnorm(m.har, 1/sigma.har^2) #year 2003
    kill[19] <- har[19]/(1 - c)
    for(t in 20:27){ 
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    
    har[28] ~ dnorm(m.har, 1/sigma.har^2) #year 2012 missing,
    kill[28] <- har[28]/(1 - c)
    tau.sur[29] <- pow(sigma.sur[29], -2)
    har[29] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest year 2013 in data,
    har.sur[29] ~ dnorm(har[29], tau.sur[29]) #likelihood
    kill[29] <- har[29]/(1 - c)
    har[30] ~ dnorm(m.har, 1/sigma.har^2) #year 2014 missing,
    kill[30] <- har[30]/(1 - c)
    
    for(t in 31:T){ #T is last data year before start of AMBCC season
      tau.sur[t] <- pow(sigma.sur[t], -2)
      har[t] ~ dnorm(m.har, 1/sigma.har^2) #latent state process harvest
      har.sur[t] ~ dnorm(har[t], tau.sur[t]) #likelihood
      kill[t] <- har[t]/(1 - c) #translate harvest to kill
    }
    mu.green ~ dlnorm(pmu.mean, pmu.tau) #prior for harvest under green policy

    # Likelihood
    # State process
    P[1] ~ dunif(0, 0.5)                      # Prior for initial population size
    P.true[1] ~ dlnorm(log(P[1]), tau.proc)
    for (t in 1:(T-1)){
    fk[t] <- (1 - exp(-0.0001*P.true[t]*CC))*kill[t]  #functional response of hunters
    P[t+1] <- max(P.true[t] + r.max*P.true[t]*(1 - P.true[t]^theta) - fk[t]/CC, 1e-5)
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
    }
    }
    ",fill = TRUE)
sink()

## modify data for pre-2017
jags.data0 <- jags.data
jags.data0$y <- jags.data$y[1:32]
jags.data0$sigma.obs <- jags.data$sigma.obs[1:32]
jags.data0$har.sur <- jags.data$har.sur[1:32]
jags.data0$sigma.sur <- jags.data$sigma.sur[1:32]

# Call JAGS from R
#out0 is with original prior on mu.green and data from 1985 to 2016
#note that this is not the exact model or data fit in 2016
# minor improvement made to model and minor data quality issue fixed in harvest and YKD count data
#model now includes SE for harvest data
out0 <- jags(jags.data, inits, parameters, "theta.logistic.emgo.2016.jags", 
            n.chains = 4, n.thin = 4, n.iter = 2400000, n.burnin = 2000000, n.adapt=10000,
            parallel=TRUE)
saveRDS(out0, file = "out0.harOriginal.RDS")
