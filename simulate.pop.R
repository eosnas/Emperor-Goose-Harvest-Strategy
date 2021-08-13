# simulate theta-logistic to find equilibrium at posterior mean
# Notes: (1) should population model be in terms of harvest rate? Functional response of hunters?
#        (2) add a revised observation model to reduce process variance, 
#           average observer effects and annual detection effects.
library(ggplot2)
#load posterior
out <- readRDS("summer_theta_logistic_2021.RDS")
project.pop <- function(
  #population projection at constant parameters
  Tmax = 100,
  n1 = out$mean$N.tot[37], 
  r = out$mean$r.max, 
  theta = out$mean$theta,
  K = out$mean$CC, 
  H = out$mean$mu.green,
  q = out$mean$q,
  c = 0.25, 
  total = TRUE){
  
  pop <- numeric(Tmax)
  pop[1] <- n1
  for(t in 2:Tmax){
    pop[t] <- pop[t-1] + pop[t-1]*r*(1-(pop[t-1]/K)^theta) - H/(1-c)
    pop[t] <- ifelse(pop[t]<0,0,pop[t])
  }
  if(total == FALSE) pop <- q*pop
  return(pop)
}

post2.5 <- project.pop(H = out$q2.5$mu.green)
postmean <- project.pop()
post97.5 <- project.pop(H = out$q97.5$mu.green)
Tmax=100
df <- data.frame(Time = 1:Tmax, mean=postmean, lower=post97.5, upper=post2.5)

gplot <- ggplot(df) + 
  geom_ribbon(aes(x=Time, ymin=lower, ymax=upper), fill="lightblue") +
  geom_line(aes(x=Time, y=postmean)) + 
  labs(x="Year", y="Total Birds")
print(gplot)

#now sample the full posterior
Nsamples <- 200
Tmax=100
pick <- sample(1:length(out$sims.list$r.max), 100)
results <- matrix(NA, Tmax, Nsamples)
for(i in 1:Nsamples){
  results[,i] <- project.pop(Tmax=Tmax, 
                             n1 = out$sims.list$N.tot[i,37], 
                             r = out$sims.list$r.max[i], 
                             theta = out$sims.list$theta[i],
                             K = out$sims.list$CC[i], 
                             H = out$sims.list$mu.green[i],
                             q = out$sims.list$q)
}
df <- data.frame(Time = 1:Tmax, results) 
gplot <- ggplot(df) + 
  geom_line(aes(x=Time, y=X1)) + 
  labs(x="Year", y="Total Birds")
for(i in 1:Nsamples){
  df2 <- data.frame(Time=df$Time, Pop=df[,i])
  gplot <- gplot + geom_line(data=df2, aes(x=Time, y=Pop))
}
print(gplot)

#What proportion of the posterior is harvest rate > r.max?
h <- out$sims.list$mu.green/out$sims.list$N.tot[,37]
sum(out$sims.list$r.max - h/(1 - 0.25) < 0)/length(out$sims.list$r.max)

#add stochastic population and harvest dynamics
project.pop <- function(
  #population projection at constant parameters
  Tmax = 100,
  n1 = out$mean$N.tot[37], 
  r = out$mean$r.max, 
  theta = out$mean$theta,
  K = out$mean$CC, 
  H = out$mean$mu.green,
  sdH = NA,
  sdpop = NA,
  q = out$mean$q, 
  total = TRUE, 
  stochastic = TRUE){
  
  pop <- numeric(Tmax)
  pop[1] <- n1
  crip <- rbeta(1, 50, 150) #mean for rbeta =0.25, beta distribution parameters, could sample from posterior
  for(t in 2:Tmax){
    if(stochastic == FALSE){
      pop[t] <- pop[t-1] + pop[t-1]*r*(1-(pop[t-1]/K)^theta) - H
      pop[t] <- ifelse(pop[t]<0,0,pop[t])
    }
    if(stochastic == TRUE){
      H_t <- rnorm(1, H, sdH)
      pop[t] <- pop[t-1] + pop[t-1]*r*(1-(pop[t-1]/K)^theta) - H_t/(1-crip)
      pop[t] <- max(pop[t], 1e-5)
      pop[t] <- rlnorm(1, log(pop[t]), sdpop)
      
    }
  }
  if(total == FALSE) pop <- q*pop
  return(pop)
}

Nsamples <- 200
Tmax <- 100
pick <- sample(1:length(out$sims.list$r.max), 100)
results <- matrix(NA, 100, Nsamples)
for(i in 1:Nsamples){
  results[,i] <- project.pop(n1 = out$sims.list$N.tot[i,37], 
                             r = out$sims.list$r.max[i], 
                             theta = out$sims.list$theta[i],
                             K = out$sims.list$CC[i], 
                             H = out$sims.list$mu.green[i],
                             sdH = out$sims.list$sigma.har[i],
                             sdpop = out$sims.list$sigma.proc[i],
                             q = out$sims.list$q)
}
df <- data.frame(Time = 1:Tmax, results) 
gplot <- ggplot(df) + 
  geom_line(aes(x=Time, y=X1)) + 
  labs(x="Year", y="Total Birds")
for(i in 1:Nsamples){
  df2 <- data.frame(Time=df$Time, Pop=df[,i])
  gplot <- gplot + geom_line(data=df2, aes(x=Time, y=Pop))
}
print(gplot)

#Now add harvest regulation rule
# if observed pop < 23000, then harvest = historical harvest
# if observed pop >= 23000, then harvest as above rnorm(mu.green, sigma.har)
project.pop <- function(
  #population projection at constant parameters
  Tmax = 100,
  n1 = out$mean$N.tot[37], 
  r = out$mean$r.max, 
  theta = out$mean$theta,
  K = out$mean$CC, 
  Hgreen = out$mean$mu.green,
  Hred = out$mean$m.har, 
  sdH = NA,
  sdpop = NA,
  q = out$mean$q, 
  total = TRUE, 
  stochastic = TRUE){
  
  pop <- numeric(Tmax)
  pop[1] <- n1
  crip <- rbeta(1, 50, 150) #mean for rbeta =0.25, beta distribution parameters, could sample from posterior
  s <- rchisq(1, 35) #linear model for observation error
  for(t in 2:Tmax){
    if(stochastic == FALSE){
      pop[t] <- pop[t-1] + pop[t-1]*r*(1-(pop[t-1]/K)^theta) - H
      pop[t] <- ifelse(pop[t]<0,0,pop[t])
    }
    if(stochastic == TRUE){
      mu <- q*pop[t-1]
      sigma.obs <- dnorm(0.056*mu, sqrt(((307.9^2)*35)/s) )
      obs <- rnorm(1, mu, sigma.obs) 
      Har <- ifelse(obs < 23000, rnorm(Hred, sdH), rnorm(1, Hgreen, sdH)) 
      pop[t] <- pop[t-1] + pop[t-1]*r*(1-(pop[t-1]/K)^theta) - Har/(1-crip)
      pop[t] <- max(pop[t], 1e-5)
      pop[t] <- rlnorm(1, log(pop[t]), sdpop)
    }
  }
  if(total == FALSE) pop <- q*pop
  return(pop)
}

Nsamples <- 200
Tmax <- 100
pick <- sample(1:length(out$sims.list$r.max), 100)
results <- matrix(NA, 100, Nsamples)
for(i in 1:Nsamples){
  results[,i] <- project.pop(n1 = out$sims.list$N.tot[i,37], 
                             r = out$sims.list$r.max[i], 
                             theta = out$sims.list$theta[i],
                             K = out$sims.list$CC[i], 
                             Hgreen = out$sims.list$mu.green[i],
                             Hred = out$sims.list$m.har[i],
                             sdH = out$sims.list$sigma.har[i],
                             sdpop = out$sims.list$sigma.proc[i],
                             q = out$sims.list$q[i],
                             total = FALSE)
}
df <- data.frame(Time = 1:Tmax, results) 
gplot <- ggplot()
for(i in 1:Nsamples){
  df2 <- data.frame(Time=df$Time, Pop=df[,i+1])
  gplot <- gplot + geom_line(data=df2, aes(x=Time, y=Pop))
}
gplot <- gplot +
  labs(x="Year", y="Total Birds") + 
  geom_hline(yintercept = 23000, color="red")
print(gplot)

#How frequent is season closure
sum(results < 23000)/(Tmax*Nsamples)
#How frequency is quasi-extinction
sum(apply(results,2,min) < 1)/Nsamples

gplot <- ggplot()
for(i in 1:10){
  df2 <- data.frame(Time=df$Time, Pop=df[,i+1])
  gplot <- gplot + geom_line(data=df2, aes(x=Time, y=Pop))
}
gplot <- gplot +
  labs(x="Year", y="Total Birds") + 
  geom_hline(yintercept = 23000, color="red")
print(gplot)
