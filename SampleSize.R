power.t.test(delta=3, sd=10, power=0.8, 
             type = "two.sample", alternative = "two.sided")
power.t.test(delta=3, sd=10, power=0.8, 
             type = "two.sample", alternative = "one.sided")

library(samplesize)
n.ttest(power = 0.8, alpha = 0.05, mean.diff = 3, sd1 = 10, sd2 = 10,
            k = 0.5, design = "unpaired", fraction = "unbalanced")
n.ttest(power = 0.8, alpha = 0.05, mean.diff = 3, sd1 = 10, sd2 = 10, 
            k = 1, design = "unpaired", fraction = "balanced")
origin.n=power.t.test(delta=3, sd=10, power=0.8, 
             type = "two.sample", alternative = "one.sided")$n
origin.n
needed.n=ceiling(orig.n/(1-0.1))
needed.n

library(pwr)
#sample size in each group ? effect.size 0.25 sig.level 0.05 power 0.8
pwr.t.test(n = NULL,
           d = 0.25, 
           sig.level = 0.05, 
           type = "two.sample", 
           alternative = "greater", 
           power = 0.8)