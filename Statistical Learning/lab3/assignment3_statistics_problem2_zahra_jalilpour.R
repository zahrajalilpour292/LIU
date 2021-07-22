library(RColorBrewer)
library(scales)
library(rgl)
library(tidyverse)
######loadind the data
df_raw <- read.delim("C:/Users/Firoozeh/five-personality.txt",  stringsAsFactors = FALSE, blank.lines.skip = TRUE)

#####data description ########
summary(df_raw)
dim(df_raw)
str(df_raw)
head(df_raw, 3)

###### removing missing value######
df_raw <- na.omit(df_raw)

#### sampling traing data as subset #####
set.seed(123)
train <- sample(1:nrow(df_raw), 5000 , replace  = FALSE)
traindata <- df_raw[train,]
head(traindata, 3)
dim(traindata)
summary(traindata)


# Caluclating optimal number of clusters 
# Initialize total within sum of squares error: wss
wcc <- 0
# For 1 to 20 cluster centers
for(i in 1:20) {
  
  km.out <- kmeans(traindata, centers = i, nstart = 20, iter.max = 50 )
  # Save total within sum of squares to wss variable
  wcc[i] <- km.output$tot.withinss
}
summary(km.out)
# Plot total within sum of squares vs. number of clusters
plot(1:20, wcc, type = "b", col="blue",
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

library(factoextra)
library(NbClust)

set.seed(123)
par(mfrow = c(1, 2))
fviz_nbclust(traindata, kmeans, method = "wss")

## Determining Optimal clusters (k) Using Average Silhouette Method
fviz_nbclust(traindata, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


# compute gap statistic
set.seed(123)
gap_stat <- clusGap(traindata, kmeans, K.max = 8, nstart = 25, B = 50 )

# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

######  Creating K-means Cluster Model with k=4 by elbow method  #####
set.seed(123)
km.out <- kmeans(traindata, 4, nstart = 20)
km.out$cluster
km.out$size


##### performing PCA with k=4 ####
pca_cluster <- prcomp(traindata, center = TRUE, scale = FALSE)
names(pca_cluster)
summary(pca_cluster)
pca_cluster$x
data_cluster_pca <- data.frame(pca_cluster$x[,1:10])
x11()
plot(data_cluster_pca, col=km.out$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=km.out$cluster, pch = 16)

######  Creating K-means Cluster Model with k=2 by Silhouette method  #####
set.seed(123)
kms.out <- kmeans(traindata, 2, nstart = 20)
kms.out$cluster
kms.out$size

##### performing PCA with k=2 #### 
x11()
plot(data_cluster_pca, col=kms.out$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=kms.out$cluster, pch = 16)


######  Creating K-means Cluster Model with k=5 by elbow method  #####
set.seed(123)
km5.out <- kmeans(traindata, 5, nstart = 20)
km5.out$cluster


##### performing PCA with k=5 ####  
x11()
plot(data_cluster_pca, col=km5.out$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=km5.out$cluster, pch = 16)

######  Creating K-means Cluster Model with k=6 by elbow method  #####set.seed(1)
km6.out <- kmeans(traindata, 6, nstart = 20)
km6.out$cluster


##### performing PCA with k=6 ####  
x11()
plot(data_cluster_pca, col=km6.out$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=km6.out$cluster, pch = 16)


###### assign label######
new_data <- traindata[1:50]
View(new_data)

######create new columns and aggregate values to get a score for each personality treat, according to positive or negative keys ########
new_data$EXT <- (new_data$EXT1 + new_data$EXT3 + new_data$EXT5 +
                           new_data$EXT7 + new_data$EXT9-new_data$EXT2 - new_data$EXT4 - new_data$EXT6 - new_data$EXT8 - new_data$EXT10)


new_data$OPN <- (new_data$OPN1 - new_data$OPN2 + new_data$OPN3 - new_data$OPN4 +
                            new_data$OPN5 - new_data$OPN6 + new_data$OPN7 - new_data$OPN8 +
                   new_data$OPN9 + new_data$OPN10)

new_data$CSN <- (new_data$CSN1 - new_data$CSN2 + new_data$CSN3 -
                            new_data$CSN4+ new_data$CSN5 - new_data$CSN6 + new_data$CSN7 -
                   new_data$CSN8 + new_data$CSN9 + new_data$CSN10)


new_data$AGR<- (new_data$AGR1 + new_data$AGR2 - new_data$AGR3 +
                           new_data$AGR4 - new_data$AGR5 + new_data$AGR6 - new_data$AGR7 +
                            new_data$AGR8 + new_data$AGR9 + new_data$AGR10)


new_data$EST <- (new_data$EST1 - new_data$EST2 + new_data$EST3 -
                          new_data$EST4 + new_data$EST5+ new_data$EST6 +
                          new_data$EST7 + new_data$EST8 + new_data$EST9 + new_data$EST10) 


#include column with the classification of clusters
new_data$Cluster <- kms.out$cluster
dim(new_data)

#create subset with new columns and cluster column
subset_2clusters <- new_data %>% select(51:56)
head(subset_2clusters)

#calculate means of variables in order to label clusters
data_k2 <- data.frame(subset_2clusters %>%
                    mutate(Cluster = kms.out$cluster) %>%
                    group_by(Cluster) %>%
                    summarise_all("mean"))

view(data_k2)

#label the clusters - 4 clusters

#include column with the classification of clusters - 4 clusters
new_data$Cluster <- km.out$cluster

#create subset with new columns and cluster column
subset_4clusters <- new_data %>% select(51:56)
summary(subset_4clusters)

#calculate means of variables in order to label clusters
data_k4 <- data.frame(subset_4clusters %>%
                     mutate(Cluster = km.out$cluster) %>%
                     group_by(Cluster) %>%
                     summarise_all("mean"))

View(data_k4)


####### - Hierarchical Clustering######
dist_data <- dist(traindata, method = 'euclidean')
hc.complete =hclust(dist(traindata), method="complete")
hc.average =hclust(dist(traindata), method ="average") 
hc.single=hclust(dist(traindata), method ="single")

#complete method with Eucledian Distance
x11()
plot(hc.complete, 
     main="Complete Linkage with Eucledian Distance", xlab="", sub="", cex=.9)
abline (h=25, col =" red ")
plot(hc.average, 
     main="Average Linkage with Eucledian Distance", xlab="", sub="", cex=.9)
plot(hc.single, 
     main="Single Linkage with Eucledian Distance", xlab="", sub="", cex=.9)


#Use correlation-based distance
data1 <- scale(traindata)
ddc = as.dist(1- cor(t(data1)))
hcor.complete =hclust(ddc, method="complete")
hcor.average =hclust(ddc, method ="average") 
hcor.single=hclust(ddc, method ="single")

#plot Complete Linkage
x11()
hc_correlation <- plot(hcor.complete, main=" Complete Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=1.68, col =" red ")
hcor.complete.clusters <- cutree(hcor.complete,7)
hcor.complete.clusters

#plot Average Linkage
x11()
hc_correlation2 <- plot(hcor.average, main=" Average Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=0.997, col =" red ")

#plot Single Linkage
x11()
hc_correlation3 <- plot(hcor.single, main=" Single Linkage
with Correlation -Based Distance ", xlab="", sub ="")



#select subset of data as dataset is too large
#number selected according to what K-means code could handle
set.seed(123)
test <- sample(1:nrow(df_raw),7000)
test_data <- df_raw[test,]
View(test_data)
summary(test_data)

######create new columns and aggregate values to get a score for each personality treat, according to positive or negative keys ########
test_data$EXT <- (test_data$EXT1 +test_data$EXT3 + test_data$EXT5 +
                   test_data$EXT7 + test_data$EXT9- test_data$EXT2 - test_data$EXT4 - test_data$EXT6 - test_data$EXT8 - test_data$EXT10)


test_data$OPN <- (test_data$OPN1 - test_data$OPN2 + test_data$OPN3 - test_data$OPN4 +
                   test_data$OPN5 - test_data$OPN6 + test_data$OPN7 - test_data$OPN8 +
                   test_data$OPN9 + test_data$OPN10)

test_data$CSN <- (test_data$CSN1 - test_data$CSN2 + test_data$CSN3 -
                   test_data$CSN4+ test_data$CSN5 - test_data$CSN6 + test_data$CSN7 -
                   test_data$CSN8 + test_data$CSN9 + test_data$CSN10)


test_data$AGR<- (test_data$AGR1 + test_data$AGR2 - test_data$AGR3 +
                  test_data$AGR4 - test_data$AGR5 + test_data$AGR6 - test_data$AGR7 +
                  test_data$AGR8 + test_data$AGR9 + test_data$AGR10)


test_data$EST <- (test_data$EST1 - test_data$EST2 + test_data$EST3 -
                   test_data$EST4 + test_data$EST5+ test_data$EST6 +
                   test_data$EST7 + test_data$EST8 + test_data$EST9 + test_data$EST10) 

dim(test_data)
#create subset with new columns and cluster column
subset_testdata <- test_data %>% select(51:55)
summary(subset_testdata)

#Apply K-means with 2 clusters
set.seed(123)
kms_testdata <- kmeans(subset_testdata, 2, nstart = 20)
kms_testdata$cluster
kms_testdata

######performing PCS###### 
test_pca_cluster <- prcomp(subset_testdata, scale = FALSE)
summary(test_pca_cluster)
dim(test_pca_cluster)
test_pca_cluster$x
test_data_cluster_pca <- data.frame(test_pca_cluster$x[,1:5])

x11()
plot(test_data_cluster_pca, col=k2_testdata$cluster, pch=16)
plot(test_data_cluster_pca$PC1, test_data_cluster_pca$PC2, col=kms_testdata$cluster, pch = 16)


#Apply K-means with 4 clusters
set.seed(123)
km4_testdata <- kmeans(subset_testdata, 4, nstart = 20)
km4_testdata$cluster
km4_testdata



#include column with the classification of 2 clusters
subset_testdata$Cluster <- kms_testdata$cluster
View(subset_testdata)

#calculate means of variables in order to label clusters
label_cluster2 <- data.frame(subset_testdata %>%
                               mutate(Cluster = kms_testdata$cluster) %>%
                               group_by(Cluster) %>%
                               summarise_all("mean"))

View(label_cluster2)

#include column with the classification of 4 clusters
subset2_testdata <- subset_testdata
subset2_testdata$Cluster <- km4_testdata$cluster
View(subset2_testdata)

#calculate means of variables in order to label clusters
label_cluster4 <- data.frame(subset2_testdata %>%
                               mutate(Cluster = km4_testdata$cluster) %>%
                               group_by(Cluster) %>%
                               summarise_all("mean"))

View(label_cluster4)
