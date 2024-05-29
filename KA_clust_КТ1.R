library(readxl)
data <- read_excel("worlddata.xlsx", sheet = "Лист1")
library(MASS)
library(ggplot2)
library(knitr)
library(colorspace)
library(cluster)
library(dplyr)
library(factoextra)
library(pvclust)
library(fpc)
library(pastecs)
library(dplyr)
library(dbscan)
library(NbClust)
install.packages("fossil")
library(fossil)
library(clValid)
library(hopkins)
library(tidyr)
library(clustertend)

head(data)
# Дендрограмма
dist.data<-dist(data[,2:9])
clust.data<-hclust(dist.data,"ward.D")
plot(clust.data)
rect.hclust(clust.data, k=3, border="green")

# Удаление аномальных значений - 34 и 74
data1 <- data[1:33,]
data2 <- data[35:178,]
data <- merge(data1, data2, all = TRUE)
data3 <- data[1:72,]
data4 <- data[74:177,]
data <- merge(data3, data4, all = TRUE)

dist.data<-dist(data[,2:9])
clust.data<-hclust(dist.data,"ward.D")
plot(clust.data)
rect.hclust(clust.data, k=5, border="green")

# Тест Хопкинса
data_st<-as.data.frame(lapply(data[,2:9],scale))
summary(data_st)
hopkins(data_st)
# Значение равно 0,08, значит маловероятно, что данные имеют статистически значимые кластеры

# VAT
ddf<-dist(data)
fviz_dist(ddf)
# Большинство данных показано светло-красного цвета, есть ярко-синие линии

# Определение оптимального количества кластеров
fviz_nbclust(data_st, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
fviz_nbclust(data_st, kmeans, method = "wss") + labs(subtitle = "Elbow method")
fviz_nbclust(data_st, kmeans, method = "gap_stat") + labs(subtitle = "Gap statistic method")
nc<-NbClust(data = data_st, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria") 
# Тесты показали, что оптимальное количество кластеров - 4

# Разбиение на 4 кластера
datas <- data[,2:9]
b_clasters<-kmeans(data_st,4)
fit<-pvclust(t(datas), method.hclust = "ward.D",method.dist = "euclidean")
plot(fit, cex = 0.9)
pvrect(fit, alpha = 0.7)
plotcluster(datas,b_clasters$cluster)

dat <- data[1:176, 1:9]
dat$gr4<-b_clasters$cluster
v1<-dat %>% filter(gr4==1)
vv1<-v1[,2:9]
stat.desc(vv1)
v2<-dat %>% filter(gr4==2)
vv2<-v2[,2:9]
stat.desc(vv2)
v3<-dat %>% filter(gr4==3)
vv3<-v3[,2:9]
stat.desc(vv3)
v4<-dat %>% filter(gr4==4)
vv4<-v4[,2:9]
stat.desc(vv4)

b_clasters
# Отношение межгрупповой дисперсии к общей составляет 59,2% — это означает, что кластеризация нам объяснила всего 60% вариации признака. 

# PAM
df <- data[,2:9]
ds <- scale(df)
pam.res <- pam(ds,4)
print(pam.res)
dd <- cbind(dat, cluster1 = pam.res$cluster)
head(dd, n = 4)
pam.res$medoids
fviz_cluster(
  pam.res,
  data = ds,
  choose.vars = NULL,
  stand = TRUE,
  axes = c(1, 2),
  geom = c("point", "text"),
  repel = FALSE,
  show.clust.cent = TRUE,
  ellipse = TRUE,
  ellipse.type = "convex",
  ellipse.level = 0.95,
  ellipse.alpha = 0.2,
  shape = NULL,
  pointsize = 1.5,
  labelsize = 12,
  main = "Cluster plot",
  xlab = NULL,
  ylab = NULL,
  outlier.color = "black",
  outlier.shape = 19,
  outlier.pointsize = pointsize,
  outlier.labelsize = labelsize,
  ggtheme = theme_grey()
)

# Метод Clara
clara.res <- clara(ds, 4, samples=15, pamLike=TRUE)
clara.res
ddd <- cbind(dat, cluster2 = clara.res$cluster)
head(ddd, n = 4)
head(clara.res$clustering, 10)
fviz_cluster(
  clara.res,
  data = ds,
  choose.vars = NULL,
  stand = TRUE,
  axes = c(1, 2),
  geom = c("point", "text"),
  repel = FALSE,
  show.clust.cent = TRUE,
  ellipse = TRUE,
  ellipse.type = "convex",
  ellipse.level = 0.95,
  ellipse.alpha = 0.2,
  shape = NULL,
  pointsize = 1.5,
  labelsize = 12,
  main = "Cluster plot",
  xlab = NULL,
  ylab = NULL,
  outlier.color = "black",
  outlier.shape = 19,
  outlier.pointsize = pointsize,
  outlier.labelsize = labelsize,
  ggtheme = theme_grey() )

# DBSCAN
dbscan::kNNdistplot(ds, k = 5)
abline(h = 2, lty = 2)
db <- fpc::dbscan(ds, eps = 2, MinPts = 5)
fviz_cluster(db, data = ds, stand = FALSE, ellipse = FALSE, show.clust.cent = FALSE, geom = "point",palette = "jco", ggtheme = theme_classic())
print.dbscan (db)
db$cluster
# Найдено 10 граничных точек

# Сравнение методов кластеризации
clmethods <- c("clara","kmeans","pam")
stab <- clValid(data_st, nClust = 2:6, clMethods = clmethods, validation = "stability")
summary(stab)
#     Score  Method Clusters
# APN 0.0228 pam    2       
# AD  1.8342 pam    6       
# ADM 0.0762 pam    2       
# FOM 0.7111 pam    4     
# Результаты показали, что предпочтительный метод - pam, при 2 кластерах параметр APN говорит, что более согласованы результаты кластеризации. Хотя остальные параметры не очень хорошие.

# Проверка результатов кластеризации
# Методом силуэта
b_clasters_2 <- kmeans(ds, 2)
sil <- silhouette(b_clasters_2$cluster, dist(ds))
fviz_silhouette (sil)
b_clast_3 <- kmeans(ds, 3)
sil <- silhouette(b_clast_3$cluster, dist(ds))
fviz_silhouette (sil)
b_clast_4 <- kmeans(ds, 4)
sil <- silhouette(b_clast_4$cluster, dist(ds))
fviz_silhouette (sil)
# Наименьшее количество отрицательных значений получилось при 3 кластерах и 2 кластерах. Av. sil.width самый высокий при 3 кластерах - 0,44

# Индекс Данна
km_stats <- cluster.stats(dist(ds), b_clasters_2$cluster)
km_stats$dunn
# 0.04004195
km_stats <- cluster.stats(dist(ds), b_clast_3$cluster)
km_stats$dunn
# 0.05393087
km_stats <- cluster.stats(dist(ds), b_clast_4$cluster)
km_stats$dunn
# 0.02756813
# Индексы во всех случаях маленькие, значит данные плохо разделяются на кластеры

# Индекс Рэнда
cluster.stats(d = dist(ds), clara.res$cluster, pam.res$cluster)$corrected.rand
# 0.809465 - методы не полностью согласованы

# Индекс Мейла
cluster.stats(d = dist(ds), clara.res$cluster, pam.res$cluster)$vi
# 0.5411124

# Анализ кластеров
ggplot(data = dat, aes(x = "", y = x1)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x2)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x3)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x4)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x5)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x6)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x7)) + geom_boxplot() + facet_grid(~gr4)
ggplot(data = dat, aes(x = "", y = x8)) + geom_boxplot() + facet_grid(~gr4)
# Медианные значения достаточно схожие в 3 из 4 кластеров и существует много выбросов, поэтому можно сделать вывод, что необходимо делать либо меньше кластеров (например, 2), либо кластеризация не применима для этих данных