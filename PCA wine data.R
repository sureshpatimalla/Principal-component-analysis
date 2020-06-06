# Loading wine data


wine<-read.csv(file.choose()) ## use read.csv for csv files
data <- wine 
View(data)

attach(data)
cor(data)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcawine<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcawine)
## princomp(data, cor = TRUE) not_same_as prcomp(data, scale=TRUE); similar, but different
summary(pcawine)
str(pcawine)
loadings(pcawine)

plot(pcawine) # graph showing importance of principal components 

# Comp.1 having highest importance (highest variance)

biplot(pcawine)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcawine$sdev*pcawine$sdev)*100/(sum(pcawine$sdev*pcawine$sdev)),type="b")
#pcaObj$loadings

pcawine$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(data,pcawine$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

write.csv(final1,file="wine_Hclustered.csv",row.names = F,col.names = F)

#####################################(Kmeans_clustering)#############################################################

# Clustering the data using Kmeans function --> Kmeans Clustering
# Model Building with k=5

#install.packages("animation")
library(animation)
fit <- kmeans.ani(norm_clus,5) # 50 cluster ploting
fit$cluster

wss = (nrow(norm_clus)-1)*sum(apply(norm_clus, 2, var))# Determine number of clusters by scree-plot
for (i in 2:8)
  wss[i] = sum(kmeans(norm_clus, centers=i)$withinss)
?plot
plot(wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

final2<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final2)
View(aggregate(final2[,-c(2,16:18)],by=list(membership_1),FUN=mean)) # Inferences can be

write.csv(final2,file="wine_kmeans.csv",row.names = F,col.names = F)

######################################################################################################33

