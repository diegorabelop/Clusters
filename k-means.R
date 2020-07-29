library(haven)

library(haven)
data <- read_sav("C:/Users/TB e Internet/Desktop/ESPANHA/SILVIA com PCA/BASE NORMA SILVIA_11-04-20_NO AFECTIVOS_17-04.sav")


colnames(data)
data2 <- data[,c("Psychotic", "Negative","Depressive", "Anxiey", "Dep_anx", "Excitement", "ident_caso")] 
anyNA(data2)
data3 <- na.omit(data2)
#dataset only cognitive variables comisiones_ptV2M
HCA <- data3[ c(1:6) ]
colnames(HCA)
library(NbClust) #http://www.sthda.com/english/wiki/print.php?id=239 
nb <- NbClust(HCA[,1:6], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index ="all")

library(stats)
set.seed(123)
kmeans_cluster <- kmeans(HCA, 2, iter.max = 10, nstart = 100)
# Print the results
print(kmeans_cluster)
aggregate(HCA, by=list(cluster=kmeans_cluster$cluster), mean)
baseline_Kmeans <- cbind(data2, cluster_baseline_Kmeans = kmeans_cluster$cluster)
head(baseline_Kmeans)
# Cluster number for each of the observations
kmeans_cluster$cluster
head(kmeans_cluster$cluster, 4)
# Cluster means
kmeans_cluster$centers
library(haven)
write_sav(baseline_Kmeans, "banco_baseline_Kmeans.sav")
############Follow-up########
HCA_2anos <- data[,c("Psychotic2", "Negative2", "Depressive2", "Anxiey2", "Dep_anx2", "Excitement2", "ident_caso")]  
colnames(HCA_2anos)
HCA2_a <- na.omit(HCA_2anos)
HCA2 <- HCA2_a[ c(1:6) ]
colnames(HCA2)
library(NbClust) #http://www.sthda.com/english/wiki/print.php?id=239 
nb2 <- NbClust(HCA2[,1:6], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index ="all")

library(stats)
set.seed(123)
kmeans_cluster2 <- kmeans(HCA2, 4, iter.max = 10, nstart = 100)
# Print the results
print(kmeans_cluster2)
aggregate(HCA2, by=list(cluster=kmeans_cluster2$cluster), mean)
follow_Kmeans <- cbind(data2, cluster_follow_Kmeans = kmeans_cluster2$cluster)
head(follow_Kmeans)
# Cluster number for each of the observations
kmeans_cluster2$cluster
head(kmeans_cluster2$cluster, 4)
# Cluster means
kmeans_cluster2$centers
library(haven)
write_sav(follow_Kmeans, "banco_follow_Kmeans.sav")
