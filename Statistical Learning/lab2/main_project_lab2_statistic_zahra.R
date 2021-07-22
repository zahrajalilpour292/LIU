library(readxl)
library(MASS)
library(tree)
library(ISLR)
library(randomForest)
library(GLDEX)
library(e1071)
library(VIM)
library(rpart)
library(caret)
install.packages("gbm")
library(gbm)
install.packages("caTools")
df <- read_excel("C:/Users/Firoozeh/Data_Cortex_Nuclear.xls")
View(df)
str(df)


#remove the first column, MouseId?

df <- df[, c(2:82)]


df$Genotype <- as.integer(df$Genotype)
df$Treatment <- as.integer(df$Treatment)
df$Behavior <- as.integer(df$Behavior)
aggr(df,combined=F,bars=T,numbers=T,sortVars=F,sortCombs=T,prop=T)
summary(aggr(df))
missmap(df, main = "Missing Values", col = c("pink", "snow2"))

dim(df)
table(df$Genotype)	
table(df$Treatment)	
table(df$Behavior)	
table(df$class)	
categorical_counts <- df[, 79:82]
unique(categorical_counts)
#How many NAs each rows has?
apply(df, 1, function(x)sum(is.na(x)))
#How many rows has at least 1 NAs?
sum(apply(df, 1, function(x)sum(is.na(x)))!=0)
#How much % of rows has NAs?
sum(apply(df, 1, function(x)sum(is.na(x)))!=0)/nrow(df)
#How those NAs distributed among rows
table(apply(df, 1, function(x)sum(is.na(x))))
#Replacing NA values by mean of each column
for (i in 1:77){
  a<- which.na(df[,i])
  df[a,i] <- min(df[,i],na.rm = T)/2
}
sum(apply(df, 1, function(x)sum(is.na(x)))!=0)
table(apply(df, 1, function(x)sum(is.na(x))))

######Create train and test dataset#####

m <- length(colnames(df))
df1<- df[,-((m-3):(m-1))]
dim(df1)   
View(df1)
df2 <- df[,-((m-2):m)]
View(df2)

######binary classification######

Geno=ifelse (df2$Genotype =="Control"," 0"," 1 ")
df2 =data.frame(df2 ,Geno)
View(df2)
tree.df2 =tree(Geno~.-Genotype ,df2 )
summary (tree.df2 )
plot(tree.df2 )
text(tree.df2 ,pretty =0)
require(caTools)
set.seed(123) 
train2=sample (1: nrow(df2 ), 200)
df2.test=df2[-train2 ,]
Geno.test=Geno[-train2 ]
tree.df2 =tree(Geno~.-Genotype ,df2 ,subset =train2 )
summary(tree.df2)
#Plot Decision Tree
plot(tree.df2)
text(tree.df2, pretty=0)
tree.pred <- predict(tree.df2, newdata= df2.test, type="class")
table(tree.pred ,Geno.test)
base_accuracy <- mean(tree.pred == df2.test$Geno)
base_accuracy
table(tree.pred, df2.test$Geno)
#####prune the tree#####
set.seed(123)
tree.df2_prune <- cv.tree(tree.df2, FUN=prune.misclass)
plot(tree.df2_prune$size, tree.df2_prune$dev, type="b")
prune.df2 =prune.misclass (tree.df2 ,best =9)
summary(prune.df2)
plot(prune.df2 )
text(prune.df2 ,pretty =0)
test_tree_pred <- predict(prune.df2, newdata = df2.test, type="class")
base_accuracy <- mean(test_tree_pred == df2.test$Geno)
base_accuracy
table(test_tree_pred, df2.test$Geno)



###creat train and test####
require(caTools)
set.seed(101) 
sample = sample.split(1:nrow(df), SplitRatio = .75)
train = subset(df1, sample == TRUE)
test  = subset(df1, sample == FALSE)
dim(train)
dim(test)

####Building Decision Tree Model######
#Base Model
tree_df1 <- tree(as.factor(class) ~ ., data = train)
summary(tree_df1)
#Plot Decision Tree
plot(tree_df1)
text(tree_df1, pretty=0)

# Evaluate the performance of classification by predict
test_pred <- predict(tree_df1, newdata= test, type="class")

base_accuracy <- mean(test_pred == test$class)
base_accuracy
table(test_pred, test$class)

#####prune the tree#####
set.seed(123)
tree_df1_prune <- cv.tree(tree_df1, FUN=prune.misclass)
plot(tree_df1_prune$size, tree_df1_prune$dev, type="b")
prune.df1 =prune.misclass (tree_df1 ,best =16)
summary(prune.df1)
plot(prune.df1 )
text(prune.df1 ,pretty =0)
test_tree_pred <- predict(prune.df1, newdata = test, type="class")
base_accuracy <- mean(test_tree_pred == test$class)
base_accuracy
table(test_tree_pred, test$class)

#######bagging######
set.seed(123)
bag_df1 <- randomForest(as.factor(class)~.,data=train, mtry=77, importance=TRUE)
bag_df1
test.bag <- predict(bag_df1, newdata = test, type="class")
plot(bag_df1)

table(test.bag, test$class)
base_accuracy <- mean(test.bag == test$class)
base_accuracy
importance(bag_df1)
varImpPlot(bag_df1)
#########boosting######
set.seed(123)

tc = trainControl(method = "repeatedcv", number = 5, repeats=1)
model = train(class ~., data=train, method="gbm", trControl=tc)
pred = predict(model, test)
result = data.frame(test$class, pred)
print(result)
table(pred, test$class)
base_accuracy <- mean(pred == test$class)
base_accuracy


#####Building RandomForest Model#####
set.seed(123)
forest_df1 <- randomForest(as.factor(class)~., data=train,mtry=20, importance=TRUE)
forest_df1
test.forest <- predict(forest_df1, newdata = test, type="class")
table(test.forest, test$class)
base_accuracy <- mean(test.forest == test$class)
base_accuracy

#######Building SVM model #####
set.seed(123)
svmfit <- svm(as.factor(class)~., data=train, kernel="radial", gamma=1, cost=1)
plot(svmfit,data=train)
summary(svmfit)

#######cross validation for gama and cost, kernel="radial"####
set.seed(123)
tune.out <- tune(svm, as.factor(class)~., data=train, kernel="radial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                             gamma=c(0,0.5,1,2,3,4)))

summary(tune.out)
table(true=test$class, pred=predict(tune.out$best.model, 
                                         newdata = test))


set.seed(123)
svmfit <- svm(as.factor(class)~., data=train, kernel="radial", gamma=0.5, cost=10)
summary(svmfit)
svm.accuracy(pred, class)


#######cross validation for gama and cost, kernel="polynomial"####

set.seed(123)
tune.out <- tune(svm, as.factor(class)~., data=train, kernel="polynomial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                             degree=c(0,1,2,3,4)))

summary(tune.out)
table(true=test$class, pred=predict(tune.out$best.model, 
                                         newdata = test))

########Performing Principal Component Analysis####
library(RColorBrewer)
library(scales)
install.packages("ggfortify")
library(ggfortify)
cluster_df <- df[,1:77]
dim(cluster_df)
pr.out <- prcomp(cluster_df, scale=TRUE)
autoplot(pr.out, colour = 'class', data=df,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 2)
summary(pr.out)
pr.out$sdev
pr.var =pr.out$sdev ^2
pr.var
pve=pr.var/sum(pr.var )
pve
plot(pve , xlab=" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type="b")
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type="b")
library(factoextra)
fviz_eig(pr.out, addlabels = TRUE)
scores = as.data.frame(pr.out$x) 
library(rgl)
plot3d(scores[,1:3], col= as.numeric(as.factor(df$Behavior)), size=10, type='p', 
       xlim = c(-50,50), ylim=c(-50,50), zlim=c(-50,50))

######clustering#####
set.seed(123)
df_y = scale(cluster_df, center = T, scale = T)
fviz_nbclust(df_y, kmeans, method = "wss") 

km.out <-kmeans(cluster_df, 5, nstart=20)
a <- km.out$centers
fviz_cluster(km.out, data = cluster_df)


########Hierarchical clustering#####
dist_data<-dist(cluster_df, method = 'euclidean')
hdata<-hclust(dist_data, method="complete")
plot(hdata, ylab = 'Euclidean distances')
abline(h=7.65, lty=2)
groups <- cutree(hdata, k = 8)
groups <- as.factor(groups)
fit <- as.dendrogram(hdata)
library(dendextend)
cd = color_branches(fit,k=8)
plot(cd)
table(groups, df$class)
table(groups, df$Genotype)
table(groups, df$Treatment)
table(groups, df$Behavior)


#################clustering k-means with PCA####


comp <- data.frame(pr.out$x[,1:12])
plot(comp, col=km.out$cluster, pch=".")

table(km.out$cluster, df$Treatment)
table(km.out$cluster, df$Genotype)
table(km.out$cluster, df$Behavior)


