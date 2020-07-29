
library(haven)

#library(haven)
#data <- read_sav("C:/Users/TB e Internet/Desktop/ESPANHA/BASE NORMA SILVIA_11-04-20_NO AFECTIVOS_17-04.sav")
library(haven)
data <- read_sav("C:/Users/TB e Internet/Desktop/ESPANHA/SILVIA com PCA/BASE NORMA SILVIA_11-04-20_NO AFECTIVOS_17-04.sav")

colnames(data)
data2 <- data[,c("Psychotic", "Negative","Depressive", "Anxiey", "Dep_anx", "Excitement", "ident_caso")] 
anyNA(data2)
data3 <- na.omit(data2)
#dataset only cognitive variables comisiones_ptV2M
HCA <- data3[ c(1:6) ]
colnames(HCA)
#Clustering 
library(plyr) ## pacote para dar nome aos fatores ###
library(dplyr) # for data cleaning
library(plyr) ## pacote para dar nome aos fatores ###
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(factoextra)
library(fpc)
library(NbClust)
## calculate Gower distance ##
gower_dist <- daisy(HCA, metric = "gower") ### [, -1] excluir a primeira coluna. Nao especificou o type
summary(gower_dist)
## criar a matrix ##
gower_mat <- as.matrix(gower_dist)
# Output most similar pair
HCA[ which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
HCA[ which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]
## choosing the algorithm ##
# Calculate silhouette width for many k using PAM
## testando essa ideia ## https://discuss.analyticsvidhya.com/t/clustering-technique-for-mixed-numeric-and-categorical-variables/6753/20 
library(fpc)
pamk.best <- pamk(gower_dist,krange = 1:6,usepam = T)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(gower_dist, pamk.best$nc))
summary(pamk.best)

### outro metodo ##
fviz_nbclust(HCA, pam, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "silhouette method")

fviz_nbclust(HCA, pam, method = "wss") + labs(subtitle = "Elbow method")

fviz_nbclust(HCA, pam, method = "gap_stat", nboot = 50)
##outro plot da silueta#
fviz_silhouette(silhouette(pam(gower_dist, pamk.best$nc))) #http://www.sthda.com/english/wiki/print.php?id=236 # 

# Plot sihouette width (higher is better)
#sil_width <- c(2:5)
#plot(2:5, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width") ## 2:11 significa numeo de coluna ##
#lines(2:5, sil_width)

library(NbClust) #http://www.sthda.com/english/wiki/print.php?id=239 
nb <- NbClust(HCA[,1:6], distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
##### analise descritiva dos clusters #####
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_results <- HCA %>% dplyr::select(1:6) %>% mutate(cluster = pam_fit$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results$the_summary
HCA[pam_fit$medoids, ]
cluster.output <- cbind(HCA,pam_fit$clustering)

cluster.output$`pam_fit$clustering` <- as.factor(cluster.output$`pam_fit$clustering`)
#rename
names(cluster.output)[names(cluster.output) == "pam_fit$clustering"] <- "cluster_baseline"
### GRAFICOS ####

library(Rtsne)
library(RColorBrewer)
tsne_obj <- Rtsne(gower_dist, is_distance = T)
tsne_data <- tsne_obj$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% mutate(cluster = factor(pam_fit$clustering), name = HCA$Psychotic)
#Mudando o nome dos cluster
library(plyr)
tsne_data$cluster <- revalue(tsne_data$cluster, c("1"="Uno", "2"="Dos", "3"="Tres")) # rename levels of factor
baseline_graph <- ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster)) + labs(x="Dimension 1", y="Dimension 2") + xlim(-10, 10) + ylim(-10, 10) + theme_classic() + scale_color_manual(values = c("red3", "navy", "black"))
baseline_graph

#Internal Validation#
#https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/

library("fpc")
# Compute cluster stats
cluster <- as.numeric(cluster.output$cluster)
clust_stats <- cluster.stats(d = dist(cluster.output), 
                             cluster, pam_fit$clustering)
# Corrected Rand index
clust_stats$corrected.rand
clust_stats$pearsongamma
clust_stats$dunn2

### running cluster mean
library(dplyr)
# Object with mean #
media_cluster <-  cluster.output %>% group_by(cluster_baseline) %>% summarise_at(vars(c(1:6)), funs(mean(., na.rm=TRUE))) 
media_cluster
#rename
names(cluster.output)[names(cluster.output) == "pam_fit$clustering"] <- "cluster_baseline"


### agrupando cluster com os demis bancos
banco_baseline <- cbind(data3, cluster.output$cluster_baseline)
names(banco_baseline)[names(banco_baseline) == "cluster.output$cluster_baseline"] <- "cluster_baseline"

library(haven)
write_sav(banco_baseline, "banco_baseline_PAM.sav")

#### FAZER CLUSTERS COM CONTROLES COLORIDO https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html


## Realizando Clusteriza??o apenas com sujeitos da segunda onda de coleta
#dataset only cognitive variables
HCA_2anos <- data[,c("Psychotic2", "Negative2", "Depressive2", "Anxiey2", "Dep_anx2", "Excitement2", "ident_caso")]  
colnames(HCA_2anos)
HCA2_a <- na.omit(HCA_2anos)
HCA2 <- HCA2_a[ c(1:6) ]
colnames(HCA2)
## calculate Gower distance ##
gower_dist2 <- daisy(HCA2, metric = "gower") ### [, -1] excluir a primeira coluna. Nao especificou o type
summary(gower_dist2)
## criar a matrix ##
gower_mat2 <- as.matrix(gower_dist2)
# Output most similar pair
HCA2[ which(gower_mat2 == min(gower_mat2[gower_mat2 != min(gower_mat2)]), arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
HCA2[ which(gower_mat2 == max(gower_mat2[gower_mat2 != max(gower_mat2)]), arr.ind = TRUE)[1, ], ]
## choosing the algorithm ##
# Calculate silhouette width for many k using PAM
## testando essa ideia ## https://discuss.analyticsvidhya.com/t/clustering-technique-for-mixed-numeric-and-categorical-variables/6753/20 
library(fpc)
pamk.best2 <- pamk(gower_dist2,krange = 1:6,usepam = T)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best2$nc, "\n")
plot(pam(gower_dist2, pamk.best2$nc))
summary(pamk.best2)

### outro metodo ##
fviz_nbclust(HCA2, pam, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "silhouette method")

fviz_nbclust(HCA2, pam, method = "wss") + labs(subtitle = "Elbow method")

fviz_nbclust(HCA2, pam, method = "gap_stat", nboot = 50)
##outro plot da silueta#
fviz_silhouette(silhouette(pam(gower_dist2, pamk.best2$nc))) #http://www.sthda.com/english/wiki/print.php?id=236 # 

# Plot sihouette width (higher is better)
#sil_width <- c(2:5)
#plot(2:5, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width") ## 2:11 significa numeo de coluna ##
#lines(2:5, sil_width)

library(NbClust) #http://www.sthda.com/english/wiki/print.php?id=239 
nb2 <- NbClust(HCA2[,1:6], distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
##### analise descritiva dos clusters #####
pam_fit2 <- pam(gower_dist2, diss = TRUE, k = 3)
pam_results2 <- HCA2 %>% dplyr::select(1:6) %>% mutate(cluster = pam_fit2$clustering) %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_results2$the_summary
HCA2[pam_fit2$medoids, ]


cluster.output2 <- cbind(HCA2,pam_fit2$clustering)

cluster.output2$`pam_fit2$clustering` <- as.factor(cluster.output2$`pam_fit2$clustering`)
#rename
names(cluster.output2)[names(cluster.output2) == "pam_fit2$clustering"] <- "cluster2"

### agrupando cluster com os demis bancos
banco_follow <- cbind(cluster.output2, HCA2_a)
library(haven)
library(haven)
write_sav(banco_follow, "banco_follow_PAM.sav")
