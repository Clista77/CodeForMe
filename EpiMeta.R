tab1.3.array <- array(c( 2, 90, 2, 660,
                        16,360,13,1250,
                        41,557, 7, 774,
                        35,449, 5, 173),
                      dim = c(2,2,4),
                      list(diabetes2 = c("1 Diabetic", "2 Not diabetic"),
                           death2    = c("1 Yes", "2 No"),
                           agecat    = c(1:4)))
addmargins(tab1.3.array)

ai <- tab1.3.array[1,1,]
bi <- tab1.3.array[1,2,]
ci <- tab1.3.array[2,1,]
di <- tab1.3.array[2,2,]
data.frame(ai, bi, ci, di)

library(epiR)
epi.2by2(tab1.3.array, units = 1)
res.strata=epi.2by2(tab1.3.array, units = 1)

library(meta)
meta.res=metabin(event.e = ai, n.e = ai + bi,
                      event.c = ci, n.c = ci + di,
                      sm = "OR", studlab = paste("agecat", c(1:4), sep = ""),
                      method="MH",
                      label.e="Exposed", label.c="Non-Exposed", outclab = "Death",
                      comb.fixed = TRUE, comb.random = FALSE)
meta.res
forest.meta(meta.res)