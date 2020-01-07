# Stem Cell
library(infer)
library(openintro)
data(stem.cell)
stem.cell=stem.cell%>%mutate(change=after-before)
change.md=stem.cell%>%group_by(trmt)%>%
                      summarize(change.mean=mean(change))%>%
                      pull()%>%diff()
change.md
n=1000
set.seed(1000)
boot.change.md=stem.cell%>%specify(change~trmt)%>%
                           hypothesize(null="independence")%>%
                           generate(reps=n,type="permute")%>%
                           calculate(stat="diff in means",order=c("esc","ctrl"))
boot.change.md
boot.change.md%>%filter(stat>=change.md)%>%
                 summarize(p.value=n()/n)
# N.C Births
ncbirths=ncbirths%>%filter(!is.na(habit))
weight.md=ncbirths%>%group_by(habit)%>%
                     summarize(weight.mean=mean(weight))%>%
                     pull()%>%diff()
weight.md
n=1000
boot.weight.md=ncbirths%>%specify(weight~habit)%>%
                          hypothesize(null="independence")%>%
                          generate(reps=n,type="permute")%>%
                          calculate(stat="diff in means",order=c("nonsmoker","smoker"))
boot.weight.md%>%summarize(lower=quantile(stat,0.025),
                           upper=quantile(stat,0.975))
boot.weight.md%>%filter(stat<=weight.md)%>%
                 summarize(one.sided.p.value=n()/n,
                           two.sided.p.value=2*one.sided.p.value)