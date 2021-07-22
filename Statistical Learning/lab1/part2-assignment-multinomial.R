library(foreign)
library(nnet)
library(stargazer)
dat <- read.csv("C:/Users/Firoozeh/ANES2016.csv") # we read the file

table(dat$PartyID)
dat.mydata$PartyID <- as.factor(dat.mydata$PartyID)

#develop multinomial logistic regression
dat.mydata$out <- relevel(dat.mydata$PartyID, ref='2')
mult.model <- multinom(out ~ Education2+Birthplace+Income+Age+Employment+Dependent+Marital+Media+FamSize+Partner+Education+SpouseEdu+GBirth+Housing , data = dat.mydata)


summary(mult.model)
library(stargazer)
stargazer(mult.model, type="text", out="mult.model.htm")
multi1.rrr = exp(coef(mult.model))
multi1.rrr
library(stargazer)
stargazer(mult.model, type="text", coef=list(multi1.rrr), p.auto=FALSE, out="multi1rrr.htm")

head(pp <- fitted(mult.model))

#predict
prob <- predict(mult.model, dat.mydata, type = "probs")
prob

#Missclassification error
library(caret)
tab <- table(pred, dat.mydata$PartyID)

print(tab)

1-sum(diag(tab))/sum(tab)

# 2-tailed z test
z <- summary(mult.model)$coefficients/summary(mult.model)$standard.errors
p <- (1-pnorm(abs(z), 0, 1)) *2
p
# drop out oll other vaiables that have not significant rol(confidence level <0.95)
mymodel1 <- mult.model <- multinom(out ~ Birthplace+Income+Age+Marital+Media+Housing , data = dat.mydata)
summary(mymodel1)
z <- summary(mymodel1)$coefficients/summary(mymodel1)$standard.errors
p <- (1-pnorm(abs(z), 0, 1)) *2
p
z
