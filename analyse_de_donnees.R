# Modules
library(ggplot2)
library(FactoMineR)
library(skimr)
library(GGally)
library(corrplot)
library(factoextra)
library(PCDimension)

# Importation jeu de données

pauv<-read.csv2("C:/Users/leoga/Desktop/M1/Analyse de données/pauvrete_add.csv", fileEncoding="UTF-8", sep=",", stringsAsFactors = TRUE)
row.names(pauv)<-pauv$Dep
pauv$Dep<-NULL
str(pauv)

#1 Statistique descriptives
summary(pauv)
sapply(pauv[sapply(pauv, is.numeric)], sd, na.rm=TRUE)

head(pauv)

boxplot(pauv$Tx.fam.monop, horizontal=TRUE ,col="darkgreen", xlab="Taux de familles monoparentales")
boxplot(pauv$Tx.chomage, horizontal=TRUE, col = "lightblue", xlab="Taux de chômage")
boxplot(pauv$Sal.moyen, col="blue", horizontal=TRUE, notch=TRUE, xlab = "Salaire moyen")
hist(pauv$Pop.totale, col = "salmon",xlab="Population", ylab="Fréquence", main = "Répartition de la population")
boxplot(pauv$Tx.d.activite.25.64, col="pink", horizontal=TRUE, notch=TRUE, xlab = "Taux d'activité")

# Préparation des données pour ACP

pauv.cr <- sqrt(100/99)*scale(pauv[,1:14])
pauv.cr <- as.data.frame(pauv.cr)

mat.cor <- round(cor(pauv.cr), 3)
corrplot(mat.cor, tl.cex=0.8)

#2: ACP sans les var illustratives

pauv.acp<-PCA(pauv[,1:13])
fviz_pca_ind(pauv.acp, label = "none")
fviz_pca_var(pauv.acp, col.var = "black", repel=T)

#3/ ACP avec les var illustratives

pauv.acp1 <- PCA(pauv[,1:15], quanti.sup = 14, quali.sup = 15)
fviz_pca_ind(pauv.acp1)
fviz_pca_var(pauv.acp1, col.var = "black", repel=T)

pauv.acp1$eig

#On sait quen ACP normée, somme des vp = inertie totale

sum(pauv.acp1$eig[,1])

#Donc Itot = 13
#Et Imoyenne = Itot / nb d'axe = 13 / 10 = 1,3

#On regarde l'éboulis des valeurs propres pour identifier les axes factoriels potentiellement intéressants

fviz_eig(pauv.acp1, addlabels = TRUE)
brokenStick(1:10, 10)

#Interprétation des axes 

fviz_contrib(pauv.acp1, choice = "var", axes=1)

fviz_contrib(pauv.acp1, choice = "var", axes=2)

fviz_pca_var(pauv.acp1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T)

corrplot(pauv.acp1$var$contrib, is.corr=FALSE)

#Qualité de représentation et aux contributions des individus

fviz_pca_ind(pauv.acp1, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             label = "none")
pauv.acp1$ind$cos2

fviz_pca_ind(pauv.acp1, col.ind = pauv.acp1$call$X$Outre.mer,
             palette = c("#00AFBB", "#E7B800"),
             legend.title = "Outre-Mer",
             addEllipses = TRUE,
             labels = F)
fviz_pca_ind(pauv.acp1, col.ind = pauv.acp1$call$X$Outre.mer,
             palette = c("#00AFBB", "#E7B800"),
             legend.title = "Outre-Mer",
             addEllipses = TRUE,
             labels = F,
             ellipse.type="confidence")

fviz_cos2(pauv.acp1, choice = "ind", axes=1:2) +
  ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "red")

pauv.acp1$ind$contrib

fviz_contrib(pauv.acp1, choice = "ind", axes=1)
fviz_contrib(pauv.acp1, choice = "ind", axes=2)

#Qualité de représentation des variables

fviz_pca_var(pauv.acp1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_cos2(pauv.acp1, choice = "var", axes=1:2) +
  ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "red")

#Variables supplémentaires

pauv.acp1$quanti.sup

pauv.acp1$quali.sup

#Biplot
fviz_pca_biplot(pauv.acp1, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969" )

