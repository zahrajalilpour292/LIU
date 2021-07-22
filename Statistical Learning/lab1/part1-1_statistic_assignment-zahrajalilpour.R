#recoding all variables and cleaning the data to see whether the relationship between IV and DV is linear or not
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages(psych)
dat <- read.csv("C:/Users/Firoozeh/ANES2016.csv") # we read the file
dim(dat)
dat$Trump <- car::recode(dat$Trump,"4:7=6;1:3=2;-9:-8=NA")  # recode rows related to Trump
dat <- na.omit(dat) # to remove NA from rows related to Trump
dat$f_Trump <- factor(dat$Trump, levels =c(2,6), labels=c("Liberal", "Conservative"))

library(data.table)
dat$Age <- car::recode(dat$Age,"-9:-8=NA")
dat$f_age <- cut(dat$Age,include.lowest = TRUE, breaks = c(18,29,44,65,Inf), labels= c("18-29","30-44","45-64","65+"))
bartable = table(dat$f_Trump, dat$f_age)
knitr::kable(bartable,digits=2)

dat$Income <- car::recode(dat$Income,"-9:=NA ; -5=NA")
dat$f_income <- cut(dat$Income,include.lowest = TRUE, breaks= c(7,13,17,22,23, Inf), labels= c("under20000","22000-40000","45000-60000","65000-90000","+100000"))
bartable1 = table(dat$f_Trump, dat$f_income)
knitr::kable(bartable1,digits=2)

dat$Education <- car::recode(dat$Education, "1:8=8;9:10=10;11:12=12;13:16=15; -9=NA; 90=10")
dat$f_education <- factor(dat$Education, levels =c(8,10,12,15), labels=c("Not garduate", "Diploma","College Edu.","Higher Edu."))
bartable2 = table(dat$f_Trump, dat$f_education)
knitr::kable(bartable2,digits=2)

dat$Media <- car::recode(dat$Media,"-9:-8=NA")
dat$f_media <- cut(dat$Media,include.lowest = TRUE, breaks = c(1,4,5,Inf), labels= c("max 1 day","max 4 days","5+ days"))
bartable3 = table(dat$f_Trump, dat$f_media)
knitr::kable(bartable3,digits=2)

dat$FamSize <- car::recode(dat$FamSize,"-9=NA")
dat$f_famsize <- cut(dat$FamSize,include.lowest = TRUE, breaks = c(0,3,5,6,Inf), labels= c("no family","up to 3","up to 5","more than 6"))
bartable4 = table(dat$f_Trump, dat$f_famsize)
knitr::kable(bartable4,digits=2)
             
dat$Partner <- car::recode(dat$Partner,"-9=NA; -1=NA")
dat$f_partner <- cut(dat$Partner,include.lowest = TRUE, breaks = c(1,2,Inf), labels= c("with partner","without partner"))
bartable5 = table(dat$f_Trump, dat$f_partner)
knitr::kable(bartable5,digits=2)

dat$SpouseEdu <- car::recode(dat$SpouseEdu, "2:8=8;9:10=10;11:12=12;13:16=15; -8:-9=NA;-1=NA;95=NA; 99=NA; 90=10")
dat$f_spouseEdu <- factor(dat$SpouseEdu, levels =c(8,10,12,15), labels=c("Not garduate", "Diploma","College Edu.","Higher Edu."))
bartable6 = table(dat$f_Trump, dat$f_spouseEdu)
knitr::kable(bartable6,digits=2)

dat$Employment <- car::recode(dat$Employment, "2=4; 6=4;7=1;-9=NA")
dat$f_employment <- factor(dat$Employment, levels =c(1,4,5,8), labels=c("Working now", "UnEmployed","Retired","Student"))
bartable7 = table(dat$f_Trump, dat$f_employment)
knitr::kable(bartable7,digits=2)

dat$Birthplace <- car::recode(dat$Birthplace, "1:3=1;7=1;-9:-8=NA")
dat$f_birthplace <- factor(dat$Birthplace, levels =c(1,4), labels=c("United State", "Other Countries"))
bartable8 = table(dat$f_Trump, dat$f_birthplace)
knitr::kable(bartable8,digits=2)

dat$GBirth <- car::recode(dat$GBirth, "-9:-8=NA")
dat$f_Gbirth <- factor(dat$GBirth, levels =c(0,1,2,3,4), labels=c("None", "One","Two","Three","All"))
bartable9 = table(dat$f_Trump, dat$f_Gbirth)
knitr::kable(bartable9,digits=2)

dat$Dependent <- car::recode(dat$Dependent, "4:7=4;9=4;-9=NA")
dat$f_dependent <- factor(dat$Dependent, levels =c(0,1,2,3,4), labels=c("No Children", "One Child","Two Children","Three Children","More than Four Children"))
bartable10 = table(dat$f_Trump, dat$f_dependent)
knitr::kable(bartable10,digits=2)

dat$Housing <- car::recode(dat$Housing, "-9:-8=NA")
dat$f_housing <- factor(dat$Housing, levels =c(1,2,3,4), labels=c("Pay Rent", "Pay Mortgage","Own Home","Other arrangement"))
bartable11 = table(dat$f_Trump, dat$f_housing)
knitr::kable(bartable11,digits=2)

dat$Education2 <- car::recode(dat$Education2, "3:4=4")
dat$f_education2 <- factor(dat$Education2, levels =c(1,2,4,5,6), labels=c("No Diploma", "High School Diploma","College","Bachelor", "Master/PhD"))
bartable12 = table(dat$f_Trump, dat$f_education2)
knitr::kable(bartable12,digits=2)

dat$f_PartyId <- factor(dat$PartyID, levels =c(1,2,3,4), labels=c("Democrat", "Republican","Independent","Other Party"))
bartable13 = table(dat$f_Trump, dat$f_PartyId)
knitr::kable(bartable13,digits=2)

dat$f_marital <- factor(dat$Marital, levels =c(1,2,3,4,5), labels=c("Married", "Widowed","Divorced","Separeted", "Never Married"))
bartable14 = table(dat$f_Trump, dat$f_marital)
knitr::kable(bartable14,digits=2)


ggplot(data=dat, mapping = aes(x = f_age, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Age_Group")+ylab("Total Amount")+labs(title="Relation between Age and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_income, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Income")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Income and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_education, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Education")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Education and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_dependent, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Dependent")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Dependent and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_education2, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Education2")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Education2 and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_housing, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Housing")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Housing and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_PartyId, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("PartyID")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between PartyID and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_Gbirth, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("GBirth")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between GBirth and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_employment, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Employment")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Employment and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_famsize, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("FamSize")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Family_Size and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_marital, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Marital")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Marital and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))

ggplot(data=dat, mapping = aes(x = f_birthplace, fill = f_Trump)) +
  geom_bar(position = "dodge")+xlab("Birth_Place")+ylab("Total Amount")+theme(axis.text.x=element_text(angle=20,size=8,vjust=0.5))+labs(title="Relation between Birth_Place and Liberal/Conservative")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))
