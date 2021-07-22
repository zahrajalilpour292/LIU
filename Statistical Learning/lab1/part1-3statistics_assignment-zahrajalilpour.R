#Analysing the role of All variables on considering Trump as Liberal or Conservative by Logistic Regression
library(haven)
library(foreign) 
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(mitools)
library(foreign)
library(pscl)
library(DAMisc)
#Reading the file
dat <- read.csv("C:/Users/Firoozeh/ANES2016.csv") # we read the file
#Outcome:
dat$Trump <- car::recode(dat$Trump,"4:7=1;1:3=0;-9:-8=NA; NA=NA")  # recode rows related to Trump
dat$f_trump <- factor(dat$Trump, levels =c(0,1), labels=c("Liberal", "Conservative"))
table(dat$f_trump)
#select a subset of the data and remove missing observations:
dat.sub <- dat %>% 
  dplyr::select("f_trump", "Age", "Media","FamSize","Partner","SpouseEdu","GBirth","Housing","Education2","Marital","Education", "Income", "Employment", "Dependent","PartyID","Birthplace") %>%
  na.omit()
#Build the generalized linear model:
logit1 <- glm(f_trump ~ Age + Media+FamSize+Partner+SpouseEdu+GBirth+Housing+Education2+Marital+Education + Income + Employment+Dependent+PartyID+Birthplace, data = dat.sub,
              family = binomial(link = logit), x = TRUE)
summary(logit1)
#To convert logged odds to odds, take the exponent of the coefficients using the exp() function:
logit1 %>%
  coef() %>%
  exp()
1-exp(logit1$coef[2])
1- exp(logit1$coef[3])
1- exp(logit1$coef[4])
1- exp(logit1$coef[5])
1- exp(logit1$coef[6])
1- exp(logit1$coef[7])
1- exp(logit1$coef[8])
1- exp(logit1$coef[9])
1- exp(logit1$coef[10])
1- exp(logit1$coef[11])
1- exp(logit1$coef[12])
1- exp(logit1$coef[13])
1- exp(logit1$coef[14])
1- exp(logit1$coef[15])
1- exp(logit1$coef[16])
#we want to compare considering Trum to liberal or Conservative by increasing education while other IVs are held constant at their means
education2.data <- with(dat, data.frame(Age = mean(Age, na.rm = T),
                                       Media=mean(Media, na.rm = T),
                                       FamSize=mean(FamSize, na.rm = T),
                                       Partner=mean(Partner, na.rm = T),
                                       Education=mean(Education, na.rm = T),
                                       SpouseEdu=mean(SpouseEdu, na.rm = T),
                                       Birthplace=mean(Birthplace, na.rm = T),
                                       GBirth=mean(GBirth, na.rm = T),
                                       Housing=mean(Housing, na.rm = T),
                                       Marital=mean(Marital, na.rm = T),
                                       Employment = mean(Employment, na.rm = T),
                                       Dependent = mean(Education, na.rm = T),
                                       Income = mean(Income, na.rm = T),
                                       PartyID = mean(PartyID, na.rm = T),
                                       Education2 = 1:6))
education2.data

logit1 %>%
  augment(newdata = education2.data, predict = "response") 
logit1 %>%
  augment(newdata = education2.data, type.predict = "response") %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> log.data 
ggplot(log.data, aes(Education2, .fitted)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)



housing.data <- with(dat, data.frame(Age = mean(Age, na.rm = T),
                                        Media=mean(Media, na.rm = T),
                                        FamSize=mean(FamSize, na.rm = T),
                                        Partner=mean(Partner, na.rm = T),
                                        Education=mean(Education, na.rm = T),
                                        SpouseEdu=mean(SpouseEdu, na.rm = T),
                                        Birthplace=mean(Birthplace, na.rm = T),
                                        GBirth=mean(GBirth, na.rm = T),
                                        Education2=mean(Education2, na.rm = T),
                                        Marital=mean(Marital, na.rm = T),
                                        Employment = mean(Employment, na.rm = T),
                                        Dependent = mean(Education, na.rm = T),
                                        Income = mean(Income, na.rm = T),
                                        PartyID = mean(PartyID, na.rm = T),
                                        Housing = 1:4))
housing.data
logit1 %>%
  augment(newdata = housing.data, predict = "response") 
logit1 %>%
  augment(newdata = housing.data, type.predict = "response") %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> log.data 
ggplot(log.data, aes(Housing, .fitted)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)

income.data <- with(dat, data.frame(Age = mean(Age, na.rm = T),
                                    Media=mean(Media, na.rm = T),
                                    FamSize=mean(FamSize, na.rm = T),
                                    Partner=mean(Partner, na.rm = T),
                                    Education=mean(Education, na.rm = T),
                                    SpouseEdu=mean(SpouseEdu, na.rm = T),
                                    Birthplace=mean(Birthplace, na.rm = T),
                                    GBirth=mean(GBirth, na.rm = T),
                                    Education2=mean(Education2, na.rm = T),
                                    Marital=mean(Marital, na.rm = T),
                                    Employment = mean(Employment, na.rm = T),
                                    Dependent = mean(Education, na.rm = T),
                                    Housing = mean(Housing, na.rm = T),
                                    PartyID = mean(PartyID, na.rm = T),
                                    Income = 1:28))
income.data
logit1 %>%
  augment(newdata = income.data, predict = "response") 
logit1 %>%
  augment(newdata = income.data, type.predict = "response") %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> log.data 
ggplot(log.data, aes(Income, .fitted)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)

dependent.data <- with(dat, data.frame(Age = mean(Age, na.rm = T),
                                    Media=mean(Media, na.rm = T),
                                    FamSize=mean(FamSize, na.rm = T),
                                    Partner=mean(Partner, na.rm = T),
                                    Education=mean(Education, na.rm = T),
                                    SpouseEdu=mean(SpouseEdu, na.rm = T),
                                    Birthplace=mean(Birthplace, na.rm = T),
                                    GBirth=mean(GBirth, na.rm = T),
                                    Education2=mean(Education2, na.rm = T),
                                    Marital=mean(Marital, na.rm = T),
                                    Employment = mean(Employment, na.rm = T),
                                    Income = mean(Income, na.rm = T),
                                    Housing = mean(Housing, na.rm = T),
                                    PartyID = mean(PartyID, na.rm = T),
                                    Dependent = 1:9))
dependent.data
logit1 %>%
  augment(newdata = dependent.data, predict = "response") 
logit1 %>%
  augment(newdata = dependent.data, type.predict = "response") %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> log.data 
ggplot(log.data, aes(Dependent, .fitted)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)

birthplace.data <- with(dat, data.frame(Age = mean(Age, na.rm = T),
                                       Media=mean(Media, na.rm = T),
                                       FamSize=mean(FamSize, na.rm = T),
                                       Partner=mean(Partner, na.rm = T),
                                       Education=mean(Education, na.rm = T),
                                       SpouseEdu=mean(SpouseEdu, na.rm = T),
                                       Dependent=mean(Dependent, na.rm = T),
                                       GBirth=mean(GBirth, na.rm = T),
                                       Education2=mean(Education2, na.rm = T),
                                       Marital=mean(Marital, na.rm = T),
                                       Employment = mean(Employment, na.rm = T),
                                       Income = mean(Income, na.rm = T),
                                       Housing = mean(Housing, na.rm = T),
                                       PartyID = mean(PartyID, na.rm = T),
                                       Birthplace = 1:4))
birthplace.data
logit1 %>%
  augment(newdata = birthplace.data, predict = "response") 
logit1 %>%
  augment(newdata = birthplace.data, type.predict = "response") %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> log.data 
ggplot(log.data, aes(Birthplace, .fitted)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
