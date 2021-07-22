#####problem1 part a    ######
library(MASS)
set.seed(100)
x<- rnorm(100)
err <- rnorm(100)
B0 <- -1
B1<- 2
#y <- B0+(B1*x)+err
y <- -1+2*x+rnorm(100)
#mean(x)
#mean(y)
l1 <- lm(y~x)
summary(l1)
plot(x)
plot(y)
mean(x)
mean(y)
u <- x-mean(x)
v <-  y-mean(y)
mean(u)
mean(v)
lm_mean <- lm(v~u)
summary(lm_mean)
par(mfrow = c(1,2))
plot(x,y,ylab="Y",,xlab="X")
abline(l1,col="red",lwd=2)
abline(v=mean(x),col="gray")
abline(h=mean(y),col="blue")
plot(u,v,ylab="Mean centered Y",xlab="Mean centered X")
abline(b=0, a=0, col="black")
abline(lm_mean,col="red",lwd=2)
abline(v=mean(u),col="yellow")
abline(h=mean(v),col="blue")


#####problem1 part b  #####
lm_mean <- lm(v~u+0)
summary(lm_mean)

lm_reverse <- lm(u~v+0)
summary(lm_reverse)
