#Predict EMGO observation from two state space models
#devtools::install_github("USFWS/AKaerial")
library(AKaerial)
library(jagsUI)
library(dplyr)
library(ggplot2)

load("../EMGO_predict/.RData")
out1 <- dget("summer_theta_logistic_2016.txt")
out2 <- readRDS("summer_theta_logistic_2021.RDS")

## compare CC
png("compare-K-posteriors.png")
plot(density(out1$CC), xlim=c(0, 4e+05), ylim=c(0, 4e-06), 
     xlab="Carrying Capacity", main="Posterior carrying capacity")
lines(density(out2$sims.list$CC), col=2)
segments(x0=50000, x1=400000, y0=dunif(100000, 50000, 400000), col="darkgray")
segments(x0=50000, y1=0, y0=dunif(100000, 50000, 400000), col="darkgray")
segments(x0=400000, y1=0, y0=dunif(100000, 50000, 400000), col="darkgray")
legend("topleft", legend=c("Prior (2016)", "Posterior (2016)", "Posterior (2019)"), 
       lty=1, col=c("darkgray",1:2))
dev.off()

## compare r.max strange result compared to prior?
plot(density(rlnorm(100000, log(0.09), 0.3)), col="darkgray", xlim=c(0, 0.3),
     xlab="r_max", main="Posterior maximum growth rate")
lines(density(out1$r.max))
lines(density(out2$sims.list$r.max), col=2)
legend("topright", legend=c("Prior (2016)", "Posterior (2016)", "Posterior (2019)"),
       lty=1, col=c("darkgray",1:2))
#remove prior
png("compare-r_max-posteriors.png")
plot(density(out1$r.max), xlim=c(0, 0.3),
     xlab="r_max", main="Posterior maximum growth rate")
lines(density(out2$sims.list$r.max), col=2)
legend("topright", legend=c("Posterior (2016)", "Posterior (2019)"), 
       lty=1, col=c(1:2))
dev.off()
## compare harvest under green
png("compare-harvest-prior-posterior.png")
plot(density(out2$sims.list$mu.green), col=2, xlim=c(0, 3e+04), 
#plot(density(out2$sims.list$mu.green/(1-0.25)), col=2, xlim=c(0, 3e+04), 
#posterior adjusted to kill rate, prior was used as kill in optimization model
     main="Mean harvest under C&T Season (Green)", 
     xlab="Mean harvest")
lines(density(rlnorm(100000, log(15000), 0.2)*(1-0.25)))
legend("topright", legend=c("Prior", "posterior"), lty=1, col=1:2)
dev.off()
#compare theta-logist state-space model to simple SS model used for 2020/21 prediction
## State-space model for 2020 and 2021 predictions
data <-  tibble::as_tibble(YKGHistoric$combined) %>% 
  mutate(Species = as.character(Species)) %>% 
  filter(Species == "EMGO") %>% 
  select(years=Year, N = itotal, SE = itotal.se)

#Plot index and prediction
plotData <- data.frame(Year=data$years, Index=data$N, 
                      lower=data$N - 1.96*data$SE, 
                      upper = data$N + 1.96*data$SE, 
                      Observed = "Yes")
newData <- data.frame(Year=c(2020), Index=out$mean$y.new, 
                      lower=out$q2.5$y.new, 
                      upper = out$q97.5$y.new, 
                      Observed ="No")
plotData <- rbind(plotData, newData)
plotData <- data.frame(plotData, N=c(apply(exp(out$sims.list$logN.est), 2, mean)[-36],NA), 
                       Model="State Space Simple")
outData <- data.frame(Year=data$years, Index=NA, N=out2$mean$N.est, 
                      lower=out2$q2.5$N.est, 
                      upper = out2$q97.5$N.est, 
                      Observed ="Yes", Model="Theta-logistic")
plotData <- rbind(plotData, outData)

gplot <- ggplot() +
  geom_pointrange(data=plotData, aes(x=Year, y=Index, ymin=lower, ymax=upper, 
                                     color=Observed, group = Observed), size=1) +
  geom_point(data=plotData, aes(x=Year, y=N, shape=Model)) +
  geom_hline(yintercept=23000) + 
  annotate(geom="text", x=1989, y=24000, label="Closure Threshold") + 
  labs(title = "Emperor goose index estimate and state space model prediction for 2020", 
       x="Year", y = "Index") + 
  theme(legend.position=c(0.2, 0.75)) + 
  guides(color=guide_legend("Index estimate")) 
print(gplot)
ggsave("compare-thelta-logistic-simple.png")
