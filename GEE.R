library(lava)
library(corrr)
library(geepack)
library(tidyverse)
data(calcium)
head(calcium)
calcium.spread=calcium%>%select(bmd,person,visit)%>%spread(visit,bmd)
calcium.corr=calcium.spread%>%select(-person)%>%
             correlate()%>%shave(upper=F)%>%fashion(decimals=3)
calcium.gather=calcium.spread%>%gather(key=visit,value=bmd,'1':'5')
calcium%>%ggplot(aes(x=visit,y=bmd,group=person))+
          geom_line(aes(group=person))+
          theme_bw()

library(lme4)
lmer=lmer(bmd~visit+(visit|person),data=calcium)
summary(lmer)
calcium.predict=calcium%>%mutate(bmd.predict=predict(lmer))
calcium.predict%>%ggplot(aes(x=visit,y=bmd.predict))+
                  geom_line(aes(group=person))+
                  theme_bw()

calcium_agg <- calcium %>%
  mutate(pred_values = predict(lmer)) %>%
  group_by(visit, group) %>%
  summarize(predicted_bmd = mean(pred_values))
ggplot(calcium_agg, aes(x = visit, color = group)) +
  geom_point(aes(y = bmd), data = calcium) +
  geom_line(aes(y = predicted_bmd), size = 1.25)