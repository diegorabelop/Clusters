rm(list=ls()); # clear R memory


library(haven)
data1 <- read_sav("C:/Users/TB e Internet/Desktop/mestrado.sav")
library(stats)
data2 <- subset(data1, subset=PTORCTR!="0", select=c("ZSNL",
                                                     "ZStroopInterferencia",
                                                     "ZTMTAinverso",
                                                     "ZTMTBinverso",
                                                     "ZFAS",
                                                     "Zmem_curto",
                                                     "Zmem_longo"))

colnames(data2)
data3 <- data2[!is.na(data2$ZSNL),]
data3 <- data2[!is.na(data2$ZStroopInterferencia),]
data3 <- data2[!is.na(data2$ZTMTAinverso),]
data3 <- data2[!is.na(data2$ZTMTBinverso),]
data3 <- data2[!is.na(data2$ZFAS),]
data3 <- data2[!is.na(data2$Zmem_curto),]
data3 <- data2[!is.na(data2$Zmem_longo),]

data3 <- data2[!is.na(data2$ZSNL),]
data4 <- data3[!is.na(data3$ZStroopInterferencia),]
data5 <- data4[!is.na(data4$ZTMTAinverso),]
data6 <- data5[!is.na(data5$ZTMTBinverso),]
data7 <- data6[!is.na(data6$ZFAS),]
data8 <- data7[!is.na(data7$Zmem_curto),]
data9 <- data8[!is.na(data8$Zmem_longo),]
anyNA(data9)
HCA <- data9
library(factoextra)

#numero ideal de cluster 
library(NbClust) 
nb <- NbClust(HCA[, 1:7], distance = "euclidean", min.nc = 2, max.nc = 10, method="ward.D", index ="all")
library(factoextra)
fviz_nbclust(nb) + theme_minimal()
fviz_nbclust(HCA[,1:7], hcut, method = "silhouette") +
   labs(subtitle = "silhouette method") #metodo escolhido

fviz_nbclust(HCA[,1:7], hcut, method = "wss") + labs(subtitle = "Elbow method")

fviz_nbclust(HCA[,1:7], hcut, method = "gap_stat", nboot = 50)
library(cluster)
library(stats)
clusters <- hclust(dist(HCA[,1:7], method = "euclidean"), method = 'ward.D')
plot(clusters)
rect.hclust(clusters, 3) 
HCA$clusternumber<-cutree(clusters, 3) 
HCA$clusternumber <- as.factor(HCA$clusternumber) # fator
HCA$cluster <- as.factor(HCA$clusternumber) # fator

library(factoextra)
dendograma <- fviz_dend(clusters, cex = 0.5, k = 3, k_colors = c("firebrick1", "dodgerblue3", "forestgreen")) ###Dendrograma com cor
dendograma2 <- fviz_dend(clusters, cex = 0.5, k = 3, k_colors = c("firebrick1", "dodgerblue3", "forestgreen"), type = "circular") ###dendograma circular ###
### CUSTOMIZAR DENDOGRAMA###

library(dplyr)
library(dendextend)
dend <-  as.dendrogram(clusters) %>%
  set("branches_lwd", 1) %>% # Branches line width
  set("branches_k_color", k = 3) %>% # Color branches by groups
  set("labels_colors", k = 3) %>%  # Color labels by groups
  set("labels_cex", 0.5)
dend
library(plyr)
media_cluster <-  HCA %>% group_by(cluster) %>% summarise_at(vars(ZSNL, ZStroopInterferencia, ZTMTAinverso, ZTMTBinverso, ZFAS, Zmem_curto, Zmem_longo), funs(mean(., na.rm=TRUE))) 
media_cluster
HCA$cluster <- revalue(HCA$cluster, c("1"="Intact", "2"="Moderate", "3"="Severe")) # rename levels of factor


### DISCRIMINANT FUNCTION ANALYSIS###

library(pander)
library(magrittr)
library(dplyr)
library(tables)
library(MASS)
HCA.lda <- lda(cluster ~ ZSNL + ZStroopInterferencia + ZTMTAinverso + ZTMTBinverso + ZFAS + Zmem_curto + Zmem_longo, HCA)
HCA.lda
predict.lda <- predict(HCA.lda)
freqtable <- table(predict.lda$class, HCA$cluster) 
rownames(freqtable) <- paste0("Predicted ", HCA$cluster %>% levels)
freqtable %>% addmargins %>% pander("Observed vs. Predicted Frequencies")
prop.table(freqtable) %>% addmargins %>% pander("Proportions")
data.frame(predict.lda$posterior, ObservedGroup = HCA$cluster, PredictedGroup = predict.lda$class, predict.lda$x) %>% head(20) %>% pander
HCA$LDA1 <- predict.lda$x[,1]
HCA$LDA2 <- predict.lda$x[,2]
library(dplyr)

gMeans <- HCA %>% group_by(cluster) %>% dplyr::select(LDA1,LDA2) %>%  summarise_each(funs(mean))


### Fazer Grafico da LDA ###

#ESTABELECER COR dos pontos ##
library(ggplot2)
library(RColorBrewer)
myColors <- c("navy", "darkorange3", "firebrick1")
names(myColors) <- levels(HCA$cluster)
colScale <- scale_colour_manual(name = "cluster",values = myColors)

# FAZER GRAFICO #
graph_LDA <- ggplot(HCA, aes(LDA1, LDA2, color = cluster)) + geom_point(alpha = 0.5) + geom_text(data = gMeans, aes(label = cluster), color = "black", vjust = 1.75) + geom_point(data = gMeans, aes(fill = cluster), size = 4, color = "black", pch = 21) + theme(legend.position = "none") + coord_equal() + theme_classic()
graph_LDA
graph_LDA1 <- graph_LDA + colScale +scale_fill_manual (values = c("navy", "darkorange3", "firebrick1")) ##colocar as cores pre-estabelecida. ColScale poe as cores nos pontos e scale_fill_manual poe as cores nos centroids ##
graph_LDA1
grafico_final <- graph_LDA1 + coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5)) ##deixar o grafico com a mesma escala o eixo XY ###
grafico_final

## grafico de media ##
library(dplyr)
# criando obj com as media #
media_cluster <-  HCA %>% group_by(cluster) %>% summarise_at(vars(ZSNL, ZTMTAinverso, ZTMTBinverso, ZFAS, ZStroopInterferencia, Zmem_curto, Zmem_longo), funs(mean(., na.rm=TRUE))) 
media_cluster
banco_completo1 <- cbind(data9,HCA$cluster)
library(haven)
write_sav(banco_completo1, "banco_completoMESTRADO.sav")
aovtotal <- aov(ZSNL + Zmem_curto + Zmem_longo + ZStroopInterferencia + ZTMTAinverso + ZTMTBinverso + ZSNL ~ cluster, data = HCA)
summary(aovtotal)
TukeyHSD(aovtotal)
aovSNL <- aov(ZSNL ~ cluster, data = HCA)
aovSNL
summary(aovSNL)
TukeyHSD(aovSNL)

aovStroop <- aov(ZStroopInterferencia ~ cluster, data = HCA)
aovStroop
summary(aovStroop)
TukeyHSD(aovStroop)

aovTMTA <- aov(ZTMTAinverso ~ cluster, data = HCA)
aovTMTA
summary(aovTMTA)
TukeyHSD(aovTMTA)

HCA$ZTMTBinverso <- as.numeric(HCA$ZTMTBinverso)
aovTMTB <- aov(ZTMTBinverso ~ cluster, data = HCA)
summary(aovTMTB)
TukeyHSD(aovTMTB)

aovFAS <- aov(ZFAS ~ cluster, data = HCA)
summary(aovFAS)
TukeyHSD(aovFAS)

aovMem_curto <- aov(Zmem_curto ~ cluster, data = HCA)
summary(aovMem_curto)
TukeyHSD(aovMem_curto)

aovMem_longo <- aov(Zmem_longo ~ cluster, data = HCA)
summary(aovMem_longo)
TukeyHSD(aovMem_longo)        
#################### PRECISAMOS FALAR SOBRE O CENTROIDE.
library(candisc)
x=lm(cbind(ZSNL, ZStroopInterferencia, ZTMTAinverso, ZTMTBinverso, 
           ZFAS, Zmem_curto, Zmem_longo)~cluster, data=HCA)
out2=candisc(x)
out2
summary(out2)

####################### encontrar lambda etc para reportar !!!

out2$structure # discriminant function loadings
plot(out2, which = 1:2, conf = 0.95, 
     col=c("gray60","lightpink","firebrick1"), pch = c(2, 1, 5),
     var.col="")


out3=candiscList(x, term="cluster")
summary(out3)


LD1 <- predict(r)$x[,1]
LD2 <- predict(r)$x[,2]

library(ggplot2)
ggplot(HCA, aes(LD1, y=LD2, col=HCA$cluster)) + 
  geom_point(size = 4, aes(color=HCA$cluster)) +
  theme_bw() + geom_text(aes(label = cluster)) ### ARRUMAR ESSA FIGURA

