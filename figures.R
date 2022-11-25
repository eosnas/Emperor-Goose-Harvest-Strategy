# code for NAAG figures
# Must run theta.logistic.r code to format data

har.all <- har.na %>% 
  mutate(lower=Harvest-2*SE, upper=Harvest+2*SE) %>%
  mutate(`Spring Season` = ifelse(Year <= 2016 & Year >= 1987, "Closed", "Open"))

ggplot(data=filter(ykd, Year < 2017)) + 
  geom_pointrange(aes(x=Year, y=itotal, ymin=itotal-2*itotal.se, ymax=itotal+2*itotal.se)) + 
  #geom_hline(yintercept = 23000) + 
  geom_pointrange(data = filter(har.all, Year < 2017), 
                  aes(x=Year, y=Harvest, ymin=lower, ymax=upper, color=`Spring Season`)) + 
  labs(x="Year", y="Number") + 
  theme(axis.title = element_text(face="bold", size = 22), 
        axis.text=element_text(size=15))
ggsave("figures/2016-har-index.png")


## 2022 posterior results

out <- readRDS("out2.harOriginal.RDS")

plot(ykd$Year, ykd$itotal, pch=16, ylim=c(0, 50000),  ylab="YKD indicated breeding total birds",
     xlab="Year", main="Emperor goose population, harvest, and state-space model")
arrows(x0=ykd$Year, x1=ykd$Year, y0=ykd$itotal - 2*ykd$itotal.se,y1=ykd$itotal + 2*ykd$itotal.se,
       length=0)
arrows(x0=ykd$Year+0.1, x1=ykd$Year+0.1, y0=out$mean$N.est - 2*out$sd$N.est,
       y1=out$mean$N.est + 2*out$sd$N.est, length=0)
points(ykd$Year+0.1, out$mean$N.est, pch=21, bg="gray50" )
points(1985:2016, har.na$Harvest[-c(33:38)], pch=1, col="black")
points(2017:2022, har.na$Harvest[33:38], pch=22, col=1, bg=1)
segments(x0=2017, x1=2022, y0=out$mean$mu.green, col=3)
segments(x0=2017, x1=2022, y0=out$q2.5$mu.green, col=3, lty=2)
segments(x0=2017, x1=2022, y0=out$q97.5$mu.green, col=3, lty=2)
arrows(x0=2017:2022, x1=2017:2022, y0=(har.na$Harvest[33:38] - 2*(har.na$SE)[33:38]),
       y1=(har.na$Harvest[33:38] + 2*(har.na$SE)[33:38]),
       length=0, col="darkgray")
arrows(x0=2017:2022+0.1, x1=2017:2022+0.1, y0=out$q2.5$har[33:38], y1=out$q97.5$har[33:38],
       length=0, col="darkgray")
arrows(x0=har.na$Year[-c(33:38)], x1=har.na$Year[-c(33:38)], y0=(har.na$Harvest[-c(33:38)] - 2*har.na$SE[-c(33:38)]),
       y1=(har.na$Harvest[-c(33:38)] + 2*har.na$SE[-c(33:38)]),
       length=0, col="black")
points(2017:2022+0.1, out$mean$har[33:38], pch=16, col="darkgray")
legend("topleft", legend=c("Survey Estimate", "State-space model", "Historical harvest data",
                           "AMBCC harvest survey-C&T season"),
        pch=c(16, 16, 1, 22), col=c(1, "gray50", 1, 1), pt.bg=c(NA, NA, NA, 1))
