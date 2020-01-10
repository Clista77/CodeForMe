library(Matching)
data(lalonde)

#------傾向スコアの計算
Logit <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75 + u74 + u75, 
	family = binomial, data = lalonde)
PS    <- Logit$fitted

#---restriction（制限、調整）
Restriction <- lm(lalonde$re78 ~ lalonde$treat + PS)
summary(Restriction)

#------IPW（「その治療群に割り付けられる確率」の逆数で重み付けする）
Weight   <- lalonde$treat/PS + (1 - lalonde$treat)/(1 - PS)
IPweight <- lm(lalonde$re78 ~ lalonde$treat, weights = Weight)
summary(IPweight)

Y    <- lalonde$re78
IPW1 <- sum( (Y/PS)[lalonde$treat == 1])/sum(1/PS[lalonde$treat == 1])
IPW0 <- sum( (Y/(1-PS))[lalonde$treat == 0])/sum(1/(1-PS)[lalonde$treat == 0])
IPW1-IPW0

#------DR（ダブリーロバスト推定量）
n1      <- nrow(lalonde[lalonde$treat == 1, ])
n0      <- nrow(lalonde[lalonde$treat == 0, ])
Linear1 <- lm(re78 ~ age + educ + black + hisp + married + nodegr + re74 + re75 + u74 + u75, data = lalonde[lalonde$treat == 1, ])
Linear0 <- lm(re78 ~ age + educ + black + hisp + married + nodegr + re74 + re75 + u74 + u75, data = lalonde[lalonde$treat == 0, ])
Y1      <- lalonde$re78[lalonde$treat == 1]
Y0      <- lalonde$re78[lalonde$treat == 0]
PS1     <- PS[lalonde$treat == 1]
PS0     <- PS[lalonde$treat == 0]

DR1 <- 1/n1 * sum(Y1 + ( (1 - PS1)/PS1) * (Y1 - Linear1$fitted))
DR0 <- 1/n0 * sum(Y0/(1 - PS0) + (1 - 1/(1 - PS0))*Linear0$fitted)
DR1-DR0