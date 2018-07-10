#This code runs PCA and kmeans on UFO data
#data available from https://www.kaggle.com/NUFORC/ufo-sightings
#Note data needs to be cleaned with UFO.r file

rm(list = ls())

library(fpc)
library(data.table)
library(ggplot2)
library(fastcluster)
library(factoextra)
library(caret)

#load data
df <- read.table(file.choose(),sep = ",",header = T,na.strings = c("?"))
df$length_of_encounter_seconds<-as.character(df$length_of_encounter_seconds)
df$length_of_encounter_seconds<-as.numeric(df$length_of_encounter_seconds)
df$latitude<-as.numeric(df$latitude)

df<-df[sample(nrow(df), 5000), ]
#scale the variables
#dfsubset <- subset(df, select = -c(X,city,state.province,country,UFO_shape,date_documented))
dfsubset <- subset(df, select = c(latitude,longitude,Year,Month,Day,Hour, Min))
scaled_df <- scale(dfsubset,scale=TRUE)
scaled_df<-cbind(scaled_df,df$UFO_shape,df$country,df$state.province)
#Hierarchical Clustering
d <- dist(scaled_df,method = "euclidean") #distance matrix

#####
#pca
pcmp <- prcomp(scaled_df, scale = TRUE)
fviz_eig(pcmp)

###
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(pcmp)
eig.val

# Results for Variables
res.var <- get_pca_var(pcmp)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 





###########
# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(scaled_df)

# First for principal components
comp <- data.frame(pc$x[,1:3])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))
library(rgl)
# Multi 3D plot
library(plot3D)
scatter3D(comp$PC2, comp$PC1, comp$PC3)

#netkmeans
# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From scree plot elbow occurs at k = 3
# Apply k-means with k=3
k <- kmeans(comp, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# 3D plot
plot(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3D(comp$PC1, comp$PC3, comp$PC4, col=k$clust)

############kmeans
kclust <- kmeans(scaled_df,centers = 3,iter.max = 100)

pred_pc <- predict(pcmp, newdata=scaled_df)[,1:2]


ggplot(comp,aes(comp$PC1,comp$PC2))+
  geom_point(aes(color = as.factor(kclust$cluster)),size=3)
# Compare by cluster in boxplot
boxplot(df$latitude ~ k$clust,
        xlab='Cluster', ylab='Lat',
        main='Lat by Cluster')
boxplot(df$longitude ~ k$clust,
        xlab='Cluster', ylab='Long',
        main='Long by Cluster')
boxplot(df$Year ~ k$clust,
        xlab='Cluster', ylab='Year',
        main='Year by Cluster')
boxplot(df$Month ~ k$clust,
        xlab='Cluster', ylab='Month',
        main='Month by Cluster')
boxplot(df$Day ~ k$clust,
        xlab='Cluster', ylab='Day',
        main='Day by Cluster')
boxplot(df$Hour ~ k$clust,
        xlab='Cluster', ylab='Hour',
        main='Hour by Cluster')
boxplot(df$Min ~ k$clust,
        xlab='Cluster', ylab='Min',
        main='Min by Cluster')
hist(df$UFO_shape~ k$clust,
        xlab='Cluster', ylab='Shape',
        main='Shape by Cluster')
boxplot(df$Country ~ k$clust,
        xlab='Cluster', ylab='Country',
        main='Country by Cluster')
boxplot(df$state.province ~ k$clust,
        xlab='Cluster', ylab='state/prov',
        main='state/prov by Cluster')