library(AKaerial)
library(tidyverse)

Combined <- filter(YKGHistoric$combined, Species=="EMGO") %>% 
  select(Year, itotal, itotal.se) %>%
  mutate(X="Combined", Observer = NA)
Observers <- filter(YKGHistoric$expanded.table, Species=="EMGO") %>% 
  select(Year, strata, Observer, itotal.est, var.Ni) %>% 
  mutate(fYear = factor(Year), strata=factor(strata)) %>% 
  group_by(Year, Observer) %>% summarise(itotal=sum(itotal.est), itotal.se=sqrt(sum(var.Ni))) %>%
  mutate(X="By Observer")

dat <- bind_rows(Observers, Combined)

gplot <- ggplot() + 
  geom_pointrange(data=dat, aes(x=Year, y=itotal, ymin=itotal-2*itotal.se, ymax=itotal+2*itotal.se,
                                color=Observer), position=position_dodge(width=0.25)) + 
  scale_color_discrete(breaks=levels(dat$Observer)) +
  facet_wrap(~X, ncol = 1) + 
  labs(x="Year", y="Indicated Total Birds")
print(gplot)
