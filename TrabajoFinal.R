library(ggfortify)
library(plotly)
library(corrplot)
library(ggplot2)  
library(factoextra)
library(dplyr)
library(reshape2)
library(cluster)
library(readxl)
library(dendextend)
library(ggplot2)
library(factoextra)
library(dplyr)
library(reshape2)
library(gplots)

datos <- read_excel("dataset1.xls")
str(datos)            
summary(datos)

#SCATTERPLOT

x <- datos$`Black Pctg.`
y <- datos$`Pctg. DEM`
plot(x, y, main = "ScatterPlot",
     xlab = "% of Africanamerican Population", ylab = "% of voting for democrats",
     pch = 19, frame = FALSE)
lines(lowess(x, y), col = "blue")

#PCA

x = cor(scale(datos[,-c(1,2,3)]))
col<- colorRampPalette(c("red", "white", "blue"))(20)
heatmap(x = x, col = col, symm = TRUE)
data_pcs<-prcomp(datos[,-c(1,2,3)], scale.=TRUE) 
data_pcs$rotation
data_pcs$x    
cor(data_pcs$x)
corrplot(cor(data_pcs$x), method = "color", tl.cex=0.5)  
biplot(data_pcs, scale = 0)
sum(apply(data_pcs$x, MARGIN=2, var)) 
summary(datos)
autoplot(data_pcs,
         label = TRUE, label.label = datos$`County Name`, label.size = 2,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, 
         scale=0)
ggplotly()
barplot(data_pcs$rotation[,1],ylab="PC1")
barplot(data_pcs$rotation[,2],ylab="PC2")

#KMEANS

datnorm<-scale(datos[,-c(1,2,3)]) 
dismatrix<- dist(datnorm, method = "euclidean")
fviz_dist(dismatrix)
set.seed(123)
fviz_nbclust(datnorm, kmeans, method = "wss", nstart=10)+
  labs(subtitle = "Elbow method")
set.seed(123)
fviz_nbclust(datnorm, kmeans, method = "silhouette", nstart=10)+
  labs(subtitle = "Silhouette method")
km <- kmeans(datnorm, centers = 4, nstart=40)
fviz_cluster(km,data=datnorm, geom="point", show.clust.cent=TRUE)
km$cluster
km$size
km$size/nrow(datnorm)
km$centers
km$withinss
km$tot.withinss  
centprofile<-melt(km$centers)
ggplot(centprofile, aes(x=Var2, y=value, group=Var1, colour=as.factor(Var1)) )+ geom_line()
datos$cluster<-km$cluster
datos[,-c(1,2,3)] %>% group_by(cluster) %>% summarise_all(mean) 
require("cluster")
sil <- silhouette(km$cluster, dist(datnorm))
head(sil)
fviz_silhouette(sil)
sil[sil[, "sil_width"] < 0,]
which(sil[, "sil_width"] < 0)
mean(sil[, "sil_width"])

#HIERARCHICAL CLUSTERING

hc<- hclust(dismatrix, method = "ward.D2")
plot(hc, hang=-1, main="dendrograma con distancia euclídea y enlace Ward", labels=FALSE)
plot(hc, hang=-1, main="dendrograma con distancia euclídea y enlace Ward")
heatmap.2(x = datnorm, scale = "none",
          distfun = function(x){dist(x, method = "euclidean")},
          hclustfun = function(x){hclust(x, method = "ward.D2")},
          density.info = "none",
          trace = "none",
          col = bluered(256),
          cexCol=0.8)
datos1<-datos
cluster <-  cutree(hc, h = 50)
datos1$cluster<-cluster
head(datos1)
table(cluster)
dev.off()
fviz_cluster(object=list(data=datos1[,-c(1,2,3)], cluster=cluster), geom="point", ellipse=TRUE)  
datosnorm2<-as.data.frame(datnorm)
datosnorm2$cluster<-cluster
resumenST2<-datosnorm2 %>% group_by(cluster) %>% summarise_all(mean) 
resumenST2
plot(hc, hang=-1, main="Distancia euclídea, enlace Ward, h=50", labels=FALSE)
rect.hclust(hc, h=50, border=c("red", "blue", "green", "pink"))
profile<-reshape2::melt(as.matrix(resumenST2[,2:6]))
ggplot(profile, aes(x=Var2, y=value, group=Var1, colour=as.factor(Var1)) )+ geom_line()
require("cluster")
sil <- silhouette(cluster, dist(datnorm))
head(sil)
fviz_silhouette(sil)
sil[sil[, "sil_width"] < 0,]
which(sil[, "sil_width"] < 0)
