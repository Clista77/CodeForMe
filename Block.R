set.seed(888)
treatment <- c("A","B")
simple <- sample(treatment, 20, replace=TRUE)
cat(simple,sep="\n")
table(simple)

library(blockrand)
set.seed(888)
block1 <- blockrand(n=20, num.levels = 2,block.sizes = c(2,2))
block1
block2 <- blockrand(n=20, num.levels = 2,block.sizes = c(1,2))
block2
