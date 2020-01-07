---
title: "A Retrospective Analysis"
date: "2019/12/25"
output: html_document
---

```{r setup, include=FALSE}
library(knitr)
library(moonBook)
library(tidyverse)
dataset=read.csv('dataset.csv')
dataset=dataset%>%select(-X)
dataset=dataset%>%mutate_at('ID',as.character)
dataset2=dataset%>%filter(year==2)
```

## Introduction
Osteoporosis is a disease caused by a loss of bone mineral density(BMD) that is associated with fragility fracturs. Pralia is used to reduce the risk of fractures in people who did not respond to other medication treatment in osteoporosis. The ratio of people who respond to Pralia and people who do not is different considering indivisual history. In reference to previous researches, the definition of responder of Pralia is people who had increased more than 3% in LBMD or HBMD after 24 months. And the definition of non-responder of Pralia is people who had not. The purpose of this research is to explore the results of examination in responders and non-responders, and inspect the background factors.

## Dataset Description
The dataset used in this research is extracted by excluding every missing data in LBMD and HBMD from a database.
The descriptive statistics is below.(P-value is induced by Welch t-test)
```{r, echo=F}
mytable(year~.,data=dataset,digit=2)
```

## Mean and SD
Mean and SD of LBMD increase in responder
```{r, echo=F}
dataset2.LBMD.y=dataset2%>%filter(LBMD.yn=='y')
dataset2.LBMD.n=dataset2%>%filter(LBMD.yn=='n')
dataset2.HBMD.y=dataset2%>%filter(HBMD.yn=='y')
dataset2.HBMD.n=dataset2%>%filter(HBMD.yn=='n')
dataset2.LBMD.y%>%ungroup()%>%
                  summarize(LBMD.increase.mean=mean(LBMD.increase),
                            LBMD.increase.sd=sd(LBMD.increase))
```
Mean and SD of LBMD increase in non-responder
```{r, echo=F}
dataset2.LBMD.n%>%ungroup()%>%
                  summarize(LBMD.increase.mean=mean(LBMD.increase),
                            LBMD.increase.sd=sd(LBMD.increase))
```
Mean and SD of HBMD increase in responder
```{r, echo=F}
dataset2.HBMD.y%>%ungroup()%>%
                  summarize(HBMD.increase.mean=mean(HBMD.increase),
                            HBMD.increase.sd=sd(HBMD.increase))
```
Mean and SD of HBMD increase in non-responder
```{r, echo=F}
dataset2.HBMD.n%>%ungroup()%>%
                  summarize(HBMD.increase.mean=mean(HBMD.increase),
                            HBMD.increase.sd=sd(HBMD.increase))
```