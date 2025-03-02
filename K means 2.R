#### ANALISIS KLASTER NON-HIRARKI #####

library(gridExtra)
library(factoextra)

#===== Input Data
excel <- read.csv(file.choose(),header=TRUE,sep=";")
View(excel)
str(excel)
excel2=excel[,-1]
X1=excel2$X1
X2=excel2$X2
X3=excel2$X3
X4=excel2$X4
X5=excel2$X5
X6=excel2$X6
X7=excel2$X7
X8=excel2$X8
data <- cbind(X1,X2,X3,X4,X5,X6,X7,X8)
summary(is.na(data))
summary(data)

#===== Menghitung Jarak Euclidean
d<-dist(data, method="euclidean")
d
fviz_dist(d,gradient=list(low="white",mid="black",high="cyan"))

#===== Menentukan Jumlah Klaster
fviz_nbclust(data, kmeans, method="silhouette")

#===== Analisis Klaster K-means
set.seed(100)
cluster<-kmeans(data,centers=2,nstart=25)
cluster

#===== Visualisasi Klaster K-means
provinsi=excel$Provinsi
rownames(data)=provinsi
head(data)
fviz_cluster(cluster,data)





