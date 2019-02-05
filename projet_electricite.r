#chargement des données ...
setwd("C:/Users/Wafa/Documents/Data")
electricite=as.matrix(read.table("electricite.txt"))
electricite_weekend= electricite[,1:120] # sans les jours du weekend


########################################################################################
#                                  Electricite                                         #
########################################################################################
#Test du nombre approprié des clusters 
library(NbClust)
res<-NbClust(electricite, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6, method = "ward.D2")
#C'est trop lent :( et ca donne 4 :/ 

#Résumé
summary(electricite)

#*******************************ACP pour visualisation*********************************#
res.pca <- princomp(electricite)
temp=res.pca$scores[,1:29]
#Affichage des données selon les deux premiers axes données par l'ACP
plot(res.pca$scores[,1],res.pca$scores[,2])
# seuls les 29 premiers axes vont être pris car il portent 95% des informations

#HCPC
library(FactoMineR)
#appliquer une acp
res.pca  <- PCA(electricite, graph = FALSE,ncp = 29)
#HCPC sur les résultats de l'ACP
par(mfrow=c(2,2) )
res.hcpc <-HCPC(res.pca, consol=FALSE)
plot(res.hcpc, choice = "tree" ,cex = 0.6)  #dendogramme 
plot(res.hcpc, choice = "bar")  # ce qui nous dis qu'on a nbclasse 
plot(res.hcpc, choice ="map", draw.tree = FALSE, ind.names = FALSE, centers.plot = T)

#****************************************CAH*******************************************#
#Calcul de la matrices de distances (Distances euclidiennes)
matDistEuc.elec = dist(electricite)
#Ce calcule est relativement long, relatif à la dimension des données

#Execution de l'algorithme utlisant la critère d'aggregation de Ward
h_ward.elec = hclust(matDistEuc.elec, method="ward.D2")

#Visualisation du dendrogramme
plot(h_ward.elec)

#En utilisant la méthode de coupure du premier grand saut, on sépare 4 clusters
elect.CAH.class <- cutree(h_ward.elec, k=2)
plot(temp, col=elect.CAH.class)
#Création d'une matrice des centres des classes 
elec.CAH.center = matrix(data = 0, nrow = 2 , ncol = 168)
#Boucle pour sommer les valeures de toute les variables pour chacune des classes
for (i in 1:2914) {
  if (elect.CAH.class[i] == 1) {
    elec.CAH.center[1,]= elec.CAH.center[1,] + as.matrix(electricite)[i,]
  }
  else {
    elec.CAH.center[2,]= elec.CAH.center[2,] + as.matrix(electricite)[i,]
  }
}
#Les centres calculés sont les moyennes algébirques des élémentes les composant
elec.CAH.center[1,] = elec.CAH.center[1,] / length(elec.CAH.center[1,])
elec.CAH.center[2,] = elec.CAH.center[2,] / length(elec.CAH.center[2,])

#Dessin des courbes pour les deux centres
plot(x = 1:168, y = elec.CAH.center[2,] ,type = 'l' , col = 2)
lines(x = 1:168, y = elec.CAH.center[1,] ,type = 'l' , col = 1)

#**************************************Kmeans*****************************************#
#Nous testant  les valeurs de k dans [2,6]
par(mfrow=c(2,3))
# K = 2
KM_2 = kmeans(electricite,2)
plot(temp , col= KM_2$cluster)
plot(x = 1:168, y=as.matrix(KM_2$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_2$centers)[2,],type = 'l',col = 2)
KM_2$tot.withinss #5891
# K = 3 
KM_3 = kmeans(electricite,3)
plot(x = 1:168, y=as.matrix(KM_3$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_3$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_3$centers)[1,],type = 'l',col = 1)
KM_3$tot.withinss #5575
# K = 4
KM_4 = kmeans(electricite,4)
plot(x = 1:168, y=as.matrix(KM_4$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_4$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_4$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_4$centers)[2,],type = 'l',col = 2)
KM_4$tot.withinss #5325
# K = 5
KM_5 = kmeans(electricite,5)
plot(x = 1:168, y=as.matrix(KM_5$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_5$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_5$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_5$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_5$centers)[5,],type = 'l',col = 5)
KM_5$tot.withinss # 5200
#k=6
KM_6 = kmeans(electricite,6)
plot(x = 1:168, y=as.matrix(KM_6$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_6$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_6$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_6$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_6$centers)[5,],type = 'l',col = 5)
lines(x=1:168, y =as.matrix(KM_6$centers)[6,],type = 'l',col = 6)
KM_6$tot.withinss # 5086

# La classification en 2 classes permet de donner le max de l'inertie intra-classes. 

#****************************************PAM*******************************************#
library(cluster)
# Lancer PAM pour K=2 classes
quant.pam <- pam(electricite,4)
# Résultats
summary (quant.pam)
# Moyennes des classes
quant.pam$medoids
# Classes
quant.pam$clustering
plot(x=1:168, y=quant.pam$medoids[1,], type='l', col= 1)
lines(x=1:168, y =quant.pam$medoids[2,],type = 'l',col = 2)
lines(x=1:168, y =quant.pam$medoids[3,],type = 'l',col = 3)
lines(x=1:168, y =quant.pam$medoids[4,],type = 'l',col = 4)

#****************************************SOM*******************************************#
#On va prendre une grille de 12x12
library(class)
library(MASS)
library(kohonen)
elec.SOM = som(as.matrix(electricite),grid = somgrid(12,12,'rectangular'))
plot(elec.SOM,type = "quality")
plot(elec.SOM, type = "dist.neighbours")

#On ne voit pas de réelle frontière sur la distance plot, la encore un k-means sur le SOM
#On essaye d'utiliser k=3
elec.KM.SOM = kmeans(elec.SOM$codes,3)
plot(x= 1:168, y= as.matrix(elec.KM.SOM$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(elec.KM.SOM$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(elec.KM.SOM$centers)[3,],type = 'l',col = 3)


########################################################################################
#              Electricite_weekend : sans les jours du weekend                         #
########################################################################################

#****************************************CAH*******************************************#
#Calcul de la matrices de distances (Distances euclidiennes)
matDistEuc.elec = dist(electricite_weekend)
#Ce calcule est relativement long, relatif à la dimension des données

#Execution de l'algorithme utlisant la critère d'aggregation de Ward
h_ward.elec = hclust(matDistEuc.elec, method="ward.D2")

#Visualisation du dendrogramme
plot(h_ward.elec)

#En utilisant la méthode de coupure du premier grand saut, on sépare 4 clusters
elect.CAH.class <- cutree(h_ward.elec, k=2)

#Création d'une matrice des centres des classes 
elec.CAH.center = matrix(data = 0, nrow = 2 , ncol = 120)
#Boucle pour sommer les valeures de toute les variables pour chacune des classes
for (i in 1:2914) {
  if (elect.CAH.class[i] == 1) {
    elec.CAH.center[1,]= elec.CAH.center[1,] + as.matrix(electricite_weekend)[i,]
  }
  else {
    elec.CAH.center[2,]= elec.CAH.center[2,] + as.matrix(electricite_weekend)[i,]
  }
}
#Les centres calculés sont les moyennes algébirques des élémentes les composant
elec.CAH.center[1,] = elec.CAH.center[1,] / length(elec.CAH.center[1,])
elec.CAH.center[2,] = elec.CAH.center[2,] / length(elec.CAH.center[2,])

#Dessin des courbes pour les deux centres
plot(x = 1:120, y = elec.CAH.center[2,] ,type = 'l' , col = 2)
lines(x = 1:120, y = elec.CAH.center[1,] ,type = 'l' , col = 1)

#**************************************Kmeans*****************************************#
#Nous testant  les valeurs de k dans [2,6]
# K = 2
KM_2 = kmeans(electricite_weekend,2)
plot(x = 1:120, y=as.matrix(KM_2$centers)[1,],type = 'l',col = 1)
lines(x=1:120, y =as.matrix(KM_2$centers)[2,],type = 'l',col = 2)
KM_2$tot.withinss #3761
# K = 3 
KM_3 = kmeans(electricite_weekend,3)
plot(x = 1:120, y=as.matrix(KM_3$centers)[3,],type = 'l',col = 3)
lines(x=1:120, y =as.matrix(KM_3$centers)[2,],type = 'l',col = 2)
lines(x=1:120, y =as.matrix(KM_3$centers)[1,],type = 'l',col = 1)
KM_3$tot.withinss #3509
# K = 4
KM_4 = kmeans(electricite_weekend,4)
plot(x = 1:120, y=as.matrix(KM_4$centers)[1,],type = 'l',col = 1)
lines(x=1:120, y =as.matrix(KM_4$centers)[4,],type = 'l',col = 4)
lines(x=1:120, y =as.matrix(KM_4$centers)[3,],type = 'l',col = 3)
lines(x=1:120, y =as.matrix(KM_4$centers)[2,],type = 'l',col = 2)
KM_4$tot.withinss #3313
# K = 5
KM_5 = kmeans(electricite_weekend,5)
plot(x = 1:120, y=as.matrix(KM_5$centers)[1,],type = 'l',col = 1)
lines(x=1:120, y =as.matrix(KM_5$centers)[3,],type = 'l',col = 3)
lines(x=1:120, y =as.matrix(KM_5$centers)[4,],type = 'l',col = 4)
lines(x=1:120, y =as.matrix(KM_5$centers)[2,],type = 'l',col = 2)
lines(x=1:120, y =as.matrix(KM_5$centers)[5,],type = 'l',col = 5)
KM_5$tot.withinss # 3238
#k=6
KM_6 = kmeans(electricite_weekend,6)
plot(x = 1:120, y=as.matrix(KM_6$centers)[1,],type = 'l',col = 1)
lines(x=1:120, y =as.matrix(KM_6$centers)[3,],type = 'l',col = 3)
lines(x=1:120, y =as.matrix(KM_6$centers)[4,],type = 'l',col = 4)
lines(x=1:120, y =as.matrix(KM_6$centers)[2,],type = 'l',col = 2)
lines(x=1:120, y =as.matrix(KM_6$centers)[5,],type = 'l',col = 5)
lines(x=1:120, y =as.matrix(KM_6$centers)[6,],type = 'l',col = 6)
KM_6$tot.withinss # 3150

# La classification en 2 classes permet de donner le max de l'inertie intra-classes. 



#****************************************PAM*******************************************#
library(cluster)
# Lancer PAM pour K=2 classes
quant.pam <- pam(electricite_weekend,4)
# Résultats
summary (quant.pam)
# Moyennes des classes
quant.pam$medoids
# Classes
quant.pam$clustering
plot(x=1:120, y=quant.pam$medoids[1,], type='l', col= 1)
lines(x=1:120, y =quant.pam$medoids[2,],type = 'l',col = 2)
lines(x=1:120, y =quant.pam$medoids[3,],type = 'l',col = 3)
lines(x=1:120, y =quant.pam$medoids[4,],type = 'l',col = 4)

#****************************************SOM*******************************************#
#On va prendre une grille de 12x12
library(class)
library(MASS)
library(kohonen)
elec.SOM = som(as.matrix(electricite_weekend),grid = somgrid(12,12,'rectangular'))
plot(elec.SOM,type = "quality")
plot(elec.SOM, type = "dist.neighbours")

#On ne voit pas de réelle frontière sur la distance plot, la encore un k-means sur le SOM
#On essaye d'utiliser k=3
elec.KM.SOM = kmeans(elec.SOM$codes,3)
plot(x= 1:120, y= as.matrix(elec.KM.SOM$centers)[1,],type = 'l',col = 1)
lines(x=1:120, y =as.matrix(elec.KM.SOM$centers)[2,],type = 'l',col = 2)
lines(x=1:120, y =as.matrix(elec.KM.SOM$centers)[3,],type = 'l',col = 3)
