library("readxl")
library(tidyr)
library(ggplot2)
library(dplyr)
library(MASS)
my_data <- read_excel("C:/Users/Firoozeh/home assignment new 05-02.xlsx")

d <-table(my_data$Year, my_data$Size)
d
as.data.frame(table(my_data$Country, my_data$Size=="Large"))
as.data.frame(table(my_data$Country=="Japan"))
d1 <-table(my_data$Sector, my_data$Size)
d1
d2 <-table(my_data$Country, my_data$Size)
d2
d3 <-table(my_data$Region, my_data$Size)
d3

blue.bold.italic.16.text <- element_text(face = "italic", color = "black", size = 6)

ggplot(data = my_data, mapping = aes(x = Sector, fill = Size)) +
  geom_bar(position = "dodge")+xlab("Sector")+ylab("Total Amount")+coord_flip()+labs(title="Relation between Sector and Size")+ theme(legend.position=c(0.8,0.8))+
  theme(legend.title = element_text(colour="blue", size=8, 
                                    face="bold"))+
  theme(legend.text = element_text(colour="black", size=5, 
                                   face="italic"))+
  theme(axis.text.y = blue.bold.italic.16.text)




## axis.text.x for x axis only
#p.labs + theme(axis.text.x = blue.bold.italic.16.text)
ggplot(data = my_data, mapping = aes(x = Year, fill = Region)) +
  geom_bar(position = "dodge")+labs(title="Relation between Region and Year")+xlab("Year from 1999 to 2018")+ylab("Total amount")+theme(legend.position=c(0.2,0.6))+
  theme(legend.title = element_text(colour="blue", size=8, 
                                    face="bold"))+
  theme(legend.text = element_text(colour="black", size=5, 
                                   face="italic"))+
  theme(axis.text.x = blue.bold.italic.16.text)


ggplot(data = my_data, mapping = aes(x = Year, fill = Size)) +
  geom_bar(position = "dodge")+labs(title="Relation between Size and Year")+xlab("Year")+ylab("Number")+theme(legend.position=c(0.2,0.6))+
  theme(legend.title = element_text(colour="blue", size=8, 
                                    face="bold"))+
  theme(legend.text = element_text(colour="black", size=5, 
                                   face="italic"))+
  theme(axis.text.x = blue.bold.italic.16.text)


ggplot(data = my_data, mapping = aes(x = Country, fill = Size)) +
  geom_bar(position = "dodge")+labs(title="Relation between Country and Size")+xlab("Country")+ylab("Number")+theme(
    plot.title = element_text(color = "brown", size = 10, face = "italic"))


ggplot(my_data, aes(x=Region)) +
  geom_bar(fill="steelblue") +
  facet_wrap(~Size, ncol=1)+coord_flip()


qplot(Sector, data = my_data, fill = Size)+coord_flip()+labs(title="Relation between Sector and Size")+theme(
  plot.title = element_text(color = "brown", size = 10, face = "italic"))


#qplot(Country, data = my_data, fill = Size)+labs(title="Relation between Sector and Size")+theme(
  #plot.title = element_text(color = "brown", size = 10, face = "italic"))
#qplot(Region, data = my_data, fill = Size, geom = "density")
#table(my_data$Country,my_data$Size, exclude =  c("MNE", "SME") )%>%prop.table(2)


qplot(Size, data = my_data, fill = Type)+labs(title="Relation between Type of Report and Size")+theme(
  plot.title = element_text(color = "brown", size = 10, face = "italic"))
