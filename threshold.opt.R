# function to interate through closure thresholds and find cummulative harvest and population size or utility
#load data
out <- readRDS("out2.RDS")
#define threshold to search across; this is observed YKD index values to search over
t_red <- seq(0, 50000, by= 250)
Nsamples <- 10000 #number of samples of the posterior
pick <- sample(1:length(out$sims.list$r.max), Nsamples)
TimeHor <- 200 #time over which to calculate return/yield; time horizon
discount <- 1 #time discount rate
df <- data.frame(Closure=t_red,
                 cumHar=rep(NA, length(t_red)),
                 pHunt=rep(NA, length(t_red)),
                 mPop = rep(NA, length(t_red)))

#set up loops
temp <- list(cumhar = numeric(), phunt = numeric(), mPop = numeric())
for(t in 1:length(t_red)){
  for(i in 1:Nsamples){
      reward <- project.pop(Tmax = TimeHor+1,
                            n1 = out$sims.list$N.tot[pick[i],], 
                            r = out$sims.list$r.max[pick[i]], 
                            theta = out$sims.list$theta[pick[i]],
                            K = out$sims.list$CC[pick[i]], 
                            Hgreen = out$sims.list$mu.green[pick[i]],
                            Hred = out$sims.list$m.har[pick[i]],
                            sdpop = out$sims.list$sigma.proc[pick[i]],
                            q = out$sims.list$q[pick[i]],
                            t_close = t_red[t],
                            total = FALSE)
      temp$cumhar[i] <- sum(reward$har, na.rm = TRUE)
      temp$phunt[i] <- sum(reward$hunt, na.rm = TRUE)/length(reward$hunt[!is.na(reward$hunt)])
      temp$mPop[i] <- mean(reward$pop, na.rm = TRUE)
  }
  df$cumHar[t] <- mean(temp$cumhar)
  df$pHunt[t] <- mean(temp$phunt)
  df$mPop[t] <- mean(temp$mPop)
}
saveRDS(df, file = "optim.RDS")
ggplot(data = df, aes(x=Closure, y=Objective)) + 
  geom_point()+
  geom_smooth(method="gam", se=TRUE)
