#########################################################################################
#################################### Début du code ######################################
#########################################################################################


################################## Packages utilisées ###################################
library(dplyr)
library(ggplot2)
library(questionr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(tidyverse)
#########################################################################################


############# Importation des données relative aux candidatures ParcourSup ##############
setwd("/Users/sonia/Desktop/Projet S2/Success/ParcourSup")
candidature <- read.csv2('Listecandidatures-Mathematiques2021.csv',sep=';',na.string="")
#########################################################################################


################################ Traitement des données #################################
table(candidature$Série)
candidat_apres_reforme_2021 <- filter(candidature,Série=="Série Générale")
data.frame(questionr::freq.na(candidat_apres_reforme_2021))
length(na.omit(candidat_apres_reforme_2021$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1.1))
length(na.omit(candidat_apres_reforme_2021$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1))
length(na.omit(candidat_apres_reforme_2021$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1))
table(candidat_apres_reforme_2021$Département.établissement)

candidat_apres_reforme_2021_2 <- select(candidat_apres_reforme_2021,-c(
  Note.à.l.épreuve.de.Mathématiques	,	
  Note.à.l.épreuve.de.Physique.Chimie	,	
  Spécialité.3	,	
  Dominante.3	,	
  LV1.3	,	
  LV2.3	,	
  Classe.4	,	
  Spécialité.4	,	
  Dominante.4	,	
  Code.établissement.4,
  Libelle.établissement.4,
  Ville.établissement.4,
  LV1.4,
  LV2.4,
  Moyenne.candidat.en.Mathématiques.Trimestre.1,
  Moyenne.candidat.en.Physique.Chimie.Trimestre.1,
  Moyenne.candidat.en.Français.Trimestre.1,
  Moyenne.candidat.en.Mathématiques.Trimestre.2,
  Moyenne.candidat.en.Physique.Chimie.Trimestre.2,
  Moyenne.candidat.en.Français.Trimestre.2,
  Moyenne.candidat.en.Mathématiques.Trimestre.3,
  Moyenne.candidat.en.Physique.Chimie.Trimestre.3,
  Moyenne.candidat.en.Français.Trimestre.3,
  Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1.1,
  Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.2.1,
  Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.3.1,
  Spécialité.2,
  Moyenne.candidat.en.Français.Trimestre.3.1,
  Moyenne.candidat.en.Enseignement.scientifique.Trimestre.3.1,
  Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.3.1,
  Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.3.1,
  Spécialité.1,
  Dominante.2,
  LV1.2,
  LV2.2,
  Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.3,
  Classe.3,
  Code.établissement.3,
  Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.3,
  Libelle.établissement.3,
  Ville.établissement.3,
  Moyenne.candidat.en.Enseignement.scientifique.Trimestre.3,
  Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.3,
  Spécialité,
  Dominante,
  Moyenne.candidat.en.Mathématiques.Trimestre.1.1,
  Moyenne.candidat.en.Physique.Chimie.Trimestre.1.1,
  Moyenne.candidat.en.Mathématiques.Trimestre.2.1,
  Moyenne.candidat.en.Physique.Chimie.Trimestre.2.1,
  Moyenne.candidat.en.Mathématiques.Trimestre.3.1,
  Moyenne.candidat.en.Physique.Chimie.Trimestre.3.1,
  Dominante.1,
  LV1.1,
  LV2.1,
  Code.établissement.2,
  Classe.2,
  Libelle.établissement.2,
  Ville.établissement.2))

Tab_eval_nvx_sciences <- select(candidat_apres_reforme_2021_2,-c(
  Code.établissement.1,
  Code.établissement,
  Classe.1,
  Année.scolaire.1,
  Niveau.d.étude.2,
  Niveau.d.étude.3,
  Niveau.d.étude.6,
  Série.1,
  Classe,
  Libelle.établissement,
  Département.établissement.2,
  Département.établissement.3,
  Département.établissement.4,
  Département.établissement.5,
  Libellé.établissement,
  Scolarité.française.1,
  Libelle.établissement.1,
  Ville.établissement.1,
  Pays.1,
  Scolarité.française.2,
  Pays.2,
  Année,
  Année.1,
  Année.2,
  Année.3,
  Scolarité.française.3,
  Pays.3,
  Année.4,
  Scolarité.française.4,
  Type.de.formation.2,
  Type.de.formation.3,
  Type.de.formation.4,
  Niveau.d.étude.4,
  Pays.4,
  Année.scolaire,
  Niveau.d.étude.5,
))

table(Tab_eval_nvx_sciences$Niveau.d.étude.actuel)
table(Tab_eval_nvx_sciences$Série.Domaine.Filière)
table(Tab_eval_nvx_sciences$Série)

Tab_eval_nvx_sciences <- select(Tab_eval_nvx_sciences,-c(
  Série.Domaine.Filière,
  Niveau.d.étude.actuel,
  Série))

write.table(Tab_eval_nvx_sciences, file = 'Tab_eval_nvx_sciences.csv',sep=';')
#########################################################################################





#########################################################################################
################################## Analyse Descriptive ##################################
#########################################################################################

################################## Création de données ##################################
Tab_eval_nvx_sciences <- read.csv2('Tab_eval_nvx_sciences.csv',sep=';')

doublant_1ere <- filter(Tab_eval_nvx_sciences, 
                        Niveau.d.étude.1 == "Première" & Niveau.d.étude == "Première")
atypiques <- filter(Tab_eval_nvx_sciences, 
                    Niveau.d.étude.1 != "Seconde" | Niveau.d.étude != "Première")

d <- which(Tab_eval_nvx_sciences$Niveau.d.étude.1 != "Seconde", arr.ind = TRUE)
e <- which(Tab_eval_nvx_sciences$Niveau.d.étude != "Première", arr.ind = TRUE)
f <- factor(c(d,e))
f <- as.numeric(levels(f))

Tab_eval_nvx_sciences_1 <- slice(Tab_eval_nvx_sciences,-f)

Premiere_Spe_Math <- Tab_eval_nvx_sciences_1[(
  Tab_eval_nvx_sciences_1$Niveau.d.étude=="Première" & 
    !(is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1.1))),]

Premiere_Spe_Phy_Chi <- Tab_eval_nvx_sciences_1[(
  Tab_eval_nvx_sciences_1$Niveau.d.étude=="Première" & 
    !(is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.1.1))),]

Premiere_Spe_Autre <- Tab_eval_nvx_sciences_1[(
  Tab_eval_nvx_sciences_1$Niveau.d.étude=="Première" & 
    (is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1.1) & 
       is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.1.1))),]

Premiere_Spe_Math_et_Phy_Chi <- inner_join(Premiere_Spe_Math,Premiere_Spe_Phy_Chi)
Premiere_Spe_Math_unique <- anti_join(Premiere_Spe_Math,Premiere_Spe_Phy_Chi)
Premiere_Spe_Phy_Chi_unique <- anti_join(Premiere_Spe_Phy_Chi,Premiere_Spe_Math)

Terminale_Spe_Math <- Tab_eval_nvx_sciences_1[(
    !(is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1))),]

Terminale_Spe_Phy_Chi <- Tab_eval_nvx_sciences_1[(
    !(is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.1))),]

Terminale_Spe_Autre <- Tab_eval_nvx_sciences[(
    (is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1) & 
       is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.1))),]

Terminale_Spe_Math_et_Phy_Chi <- inner_join(Terminale_Spe_Math,Terminale_Spe_Phy_Chi)

Terminale_Spe_Math_unique <- anti_join(Terminale_Spe_Math,Terminale_Spe_Phy_Chi)
Terminale_Spe_Phy_Chi_unique <- anti_join(Terminale_Spe_Phy_Chi,Terminale_Spe_Math)

Terminale_Math_Exp <- Tab_eval_nvx_sciences_1[(
    !(is.na(Tab_eval_nvx_sciences_1$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1))),]

nb_Premiere_Spe_Math_unique <- nrow(Premiere_Spe_Math_unique)
nb_Premiere_Spe_Phy_Chi_unique <- nrow(Premiere_Spe_Phy_Chi_unique)
nb_Premiere_Spe_Autre <- nrow(Premiere_Spe_Autre)

nb_Terminale_Spe_Math_unique <- nrow(Terminale_Spe_Math_unique)
nb_Terminale_Spe_Phy_Chi_unique <- nrow(Terminale_Spe_Phy_Chi_unique)
nb_Terminale_Spe_Autre <- nrow(Terminale_Spe_Autre)

nb_Terminale_Math_Exp <- nrow(Terminale_Math_Exp)

nb_Premiere_Spe_Math_et_Phy_Chi <- nrow(Premiere_Spe_Math_et_Phy_Chi)
nb_Terminale_Spe_Math_et_Phy_Chi <- nrow(Terminale_Spe_Math_et_Phy_Chi)

vec1 <- c("Maths et Physique-Chimie",
  "Maths",
  "Physique-Chimie",
  "Autre")

vec2 <- c(nb_Premiere_Spe_Math_et_Phy_Chi,
  nb_Premiere_Spe_Math_unique,
  nb_Premiere_Spe_Phy_Chi_unique,
  nb_Premiere_Spe_Autre)

Repartition_Spe_Premiere <- data.frame(Spécialités = vec1, value = vec2)
#########################################################################################

#################################### Visualisation ######################################
pourcentage <- round(vec2/sum(vec2)*100,digits = 2)
labels_new <- paste(vec1,pourcentage)
final_label <- paste(labels_new,'%',sep = "")

pie(vec2,
    labels = final_label, 
    col = rainbow(length(final_label)),
    main = "Répartition des Spécialités en Première", 
    radius = 1)
legend('topright',vec1,cex = 0.7,fill = rainbow(length(vec2)))
#########################################################################################

################################# Création de données ##################################
vec1 <- c("Maths et Physique-Chimie",
          "Maths",
          "Physique-Chimie",
          "Autre")

vec2 <- c(nb_Terminale_Spe_Math_et_Phy_Chi,
          nb_Terminale_Spe_Math_unique,
          nb_Terminale_Spe_Phy_Chi_unique,
          nb_Terminale_Spe_Autre)

Repartition_Spe_Terminale <- data.frame(Spécialités = vec1, value = vec2)
#########################################################################################

#################################### Visualisation ######################################
pourcentage <- round(vec2/sum(vec2)*100,digits = 2)
labels_new <- paste(vec1,pourcentage)
final_label <- paste(labels_new,'%',sep = "")

pie(vec2,labels = final_label, 
    col = rainbow(length(final_label)),
    main = "Répartition des Spécialités en Terminale", 
    radius = 1)
legend('topright',vec1,cex = 0.7,fill = rainbow(length(vec2)))
#########################################################################################

################################# Création de données ##################################
nb_f_Premiere_Spe_Math_et_Phy_Chi <- nrow(Premiere_Spe_Math_et_Phy_Chi[Premiere_Spe_Math_et_Phy_Chi$Civilité=="Mme",])

nb_f_Premiere_Spe_Math_unique <- nrow(Premiere_Spe_Math_unique[Premiere_Spe_Math_unique$Civilité=="Mme",])

nb_f_Premiere_Spe_Phy_Chi_unique <- nrow(Premiere_Spe_Phy_Chi_unique[Premiere_Spe_Phy_Chi_unique$Civilité=="Mme",])

nb_f_Premiere_Spe_Autre <- nrow(Premiere_Spe_Autre[Premiere_Spe_Autre$Civilité=="Mme",])

nb_g_Premiere_Spe_Math_et_Phy_Chi<- nrow(Premiere_Spe_Math_et_Phy_Chi[Premiere_Spe_Math_et_Phy_Chi$Civilité=="M.",])

nb_g_Premiere_Spe_Math_unique <- nrow(Premiere_Spe_Math_unique[Premiere_Spe_Math_unique$Civilité=="M.",])

nb_g_Premiere_Spe_Phy_Chi_unique <- nrow(Premiere_Spe_Phy_Chi_unique[Premiere_Spe_Phy_Chi_unique$Civilité=="M.",])

nb_g_Premiere_Spe_Autre <- nrow(Premiere_Spe_Autre[Premiere_Spe_Autre$Civilité=="M.",])

vec1 <- c("Maths et Physique-Chimie",
          "Maths",
          "Physique-Chimie",
          "Autre")

vec2 <- c(nb_f_Premiere_Spe_Math_et_Phy_Chi,
          nb_f_Premiere_Spe_Math_unique,
          nb_f_Premiere_Spe_Phy_Chi_unique,
          nb_f_Premiere_Spe_Autre,
          nb_g_Premiere_Spe_Math_et_Phy_Chi,
          nb_g_Premiere_Spe_Math_unique,
          nb_g_Premiere_Spe_Phy_Chi_unique,
          nb_g_Premiere_Spe_Autre)

data <- data.frame(Civilité = rep(c("Mme","M."),each = 4),
                   Population = vec2,
                   Spécialité = rep(vec1,2))
#########################################################################################

#################################### Visualisation ######################################
data$Spécialité <- as.character(as.vector(data$Spécialité))
ggplot(data=data, aes(x=Spécialité, y = Population, fill = Civilité)) +
  geom_bar(stat="identity",position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  ggtitle("Répartition des choix de spécialités de Première en fonction du genre")
#########################################################################################

################################# Création de données ##################################
nb_f_Terminale_Spe_Math_et_Phy_Chi <- nrow(Terminale_Spe_Math_et_Phy_Chi[Terminale_Spe_Math_et_Phy_Chi$Civilité=="Mme",])

nb_f_Terminale_Spe_Math_unique <- nrow(Terminale_Spe_Math_unique[Terminale_Spe_Math_unique$Civilité=="Mme",])

nb_f_Terminale_Spe_Phy_Chi_unique <- nrow(Terminale_Spe_Phy_Chi_unique[Terminale_Spe_Phy_Chi_unique$Civilité=="Mme",])

nb_f_Terminale_Spe_Autre <- nrow(Terminale_Spe_Autre[Terminale_Spe_Autre$Civilité=="Mme",])

nb_g_Terminale_Spe_Math_et_Phy_Chi<- nrow(Terminale_Spe_Math_et_Phy_Chi[Terminale_Spe_Math_et_Phy_Chi$Civilité=="M.",])

nb_g_Terminale_Spe_Math_unique <- nrow(Terminale_Spe_Math_unique[Terminale_Spe_Math_unique$Civilité=="M.",])

nb_g_Terminale_Spe_Phy_Chi_unique <- nrow(Terminale_Spe_Phy_Chi_unique[Terminale_Spe_Phy_Chi_unique$Civilité=="M.",])

nb_g_Terminale_Spe_Autre <- nrow(Terminale_Spe_Autre[Terminale_Spe_Autre$Civilité=="M.",])

vec1 <- c("Maths et Physique-Chimie",
          "Maths",
          "Physique-Chimie",
          "Autre")

vec2 <- c(nb_f_Terminale_Spe_Math_et_Phy_Chi,
          nb_f_Terminale_Spe_Math_unique,
          nb_f_Terminale_Spe_Phy_Chi_unique,
          nb_f_Terminale_Spe_Autre,
          nb_g_Terminale_Spe_Math_et_Phy_Chi,
          nb_g_Terminale_Spe_Math_unique,
          nb_g_Terminale_Spe_Phy_Chi_unique,
          nb_g_Terminale_Spe_Autre)


data <- data.frame(Civilité = rep(c("Mme","M."),each = 4),
                   Population = vec2,
                   Spécialité = rep(vec1,2))
#########################################################################################

#################################### Visualisation ######################################
data$Spécialité <- as.character(as.vector(data$Spécialité))
ggplot(data=data, aes(x=Spécialité, y = Population, fill = Civilité)) + 
  geom_bar(stat="identity",position = position_dodge())+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal()+
  ggtitle("Répartition des choix de spécialités de Terminale en fonction du genre")
#########################################################################################


################################# Création de données ##################################
n1 <- nrow(Terminale_Math_Exp[Terminale_Math_Exp$Civilité=="Mme",])
n2 <- nrow(Terminale_Math_Exp[Terminale_Math_Exp$Civilité=="M.",])

data <- data.frame(Genre = c("Fille","Garçon"),
                   Population = c(n1,n2))

data$fraction = data$Population / sum(data$Population)

data$ymax = cumsum(data$fraction)
data$ymin = c(0,head(data$ymax,n=-1))
#########################################################################################

#################################### Visualisation ######################################
ggplot(data, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=Genre))+
  geom_rect()+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  ggtitle("Proportion Filles/Garçons en option Mathématiques Experte")
#########################################################################################

################################# Création de données ##################################
parc_Spe_Math_Phy_Chi <- inner_join(Premiere_Spe_Math_et_Phy_Chi,Terminale_Spe_Math_et_Phy_Chi)
nb_parc_Spe_Math_Phy_Chi <- nrow(parc_Spe_Math_Phy_Chi)

parc_Spe_Math_unique <- inner_join(Premiere_Spe_Math_unique,Terminale_Spe_Math_unique)
nb_parc_Spe_Math_unique<- nrow(parc_Spe_Math_unique)

parc_Spe_Phy_Chi_unique <- inner_join(Premiere_Spe_Phy_Chi_unique,Terminale_Spe_Phy_Chi_unique)
nb_parc_Spe_Phy_Chi_unique<- nrow(parc_Spe_Phy_Chi_unique)

abd_Spe_Math_Term <- anti_join(Premiere_Spe_Math,Terminale_Spe_Math)
nb_abd_Spe_Math_Term<- nrow(abd_Spe_Math_Term)

abd_Spe_Phy_Chi_Term <- anti_join(Premiere_Spe_Phy_Chi,Terminale_Spe_Phy_Chi)
nb_abd_Spe_Phy_Chi_Term<- nrow(abd_Spe_Phy_Chi_Term)

abd_Spe_Math_Phy_Chi_Term <- anti_join(Premiere_Spe_Math_et_Phy_Chi,Terminale_Spe_Math_et_Phy_Chi)
nb_abd_Spe_Math_Phy_Chi_Term <- nrow(abd_Spe_Math_Phy_Chi_Term)

vec1 <- c("parc_Spe_Math_Phy_Chi",
          "parc_Spe_Math_unique",
          "parc_Spe_Phy_Chi_unique")

vec2 <- c(nb_parc_Spe_Math_Phy_Chi,
          nb_parc_Spe_Math_unique,
          nb_parc_Spe_Phy_Chi_unique)

Repartition_parc_de_Spe <- data.frame(Spécialités = vec1, value = vec2)

vec1 <- c("abd_Spe_Math_Phy_Chi_Term",
          "abd_Spe_Math_Term",
          "abd_Spe_Phy_Chi_Term")

vec2 <- c(nb_abd_Spe_Math_Phy_Chi_Term,
          nb_abd_Spe_Math_Term,
          nb_abd_Spe_Phy_Chi_Term)

Repartition_abd_de_Spe <- data.frame(Spécialités = vec1, value = vec2)

Tab_Notes <- Tab_eval_nvx_sciences_1[,c(1,3,15:28)]

Moyennes_2 <- function(tab){
  
  moyenne <- matrix(data=NA,nrow = nrow(tab),ncol = 11)
  
  ls1 <- c(3,7)
  ls2 <- c(4,8)
  ls3 <- c(5,9)
  ls4 <- c(6,10)
  ls5 <- c(11,14)
  ls6 <- c(12,15)
  ls7 <- c(13,16)
  
  
  ls <- data.frame(ls1,ls2,ls3,ls4,ls5,ls6,ls7)
  
  for (i in 1:nrow(tab)){
    
    s1<- 0
    s2<- 0
    cmpt1<- 0
    cmpt2 <- 0
    moyenne[i,1]<-tab[i,1]
    moyenne[i,2]<-tab[i,2]
    
    
    for (j in 1:7){
      
      compt <- 0
      sum <- 0
      
      for (k in 1:2){
        
        if(!(is.na(tab[i,ls[k,j]]))){
          compt <- compt + 1
          sum <- sum + as.numeric(tab[i,ls[k,j]])
        }
      }
      
      if(compt>0){
        moyenne[i,j+2] <- sum/compt
      }
    }
    
    for(h in 3:6){
      if(!(is.na(moyenne[i,h]))){
        s1 <- s1 + as.numeric(moyenne[i,h])
        cmpt1 <- cmpt1 + 1
      }
    }
    if(cmpt1>0){
      moyenne[i,10] <- s1/cmpt1
    }
    
    for(h in 7:9){
      if(!(is.na(moyenne[i,h]))){
        s2 <- s2 + as.numeric(moyenne[i,h])
        cmpt2 <- cmpt2 + 1
      }
    }
    if(cmpt2>0){
      moyenne[i,11]<- s2/cmpt2
    }
    
  }
  tab1 <- data.frame(Classement = moyenne[,1], 
                     Civilité = moyenne[,2],
                     Moyenne.Ens_Scientifique.en.Terminale = moyenne[,3],
                     Moyenne.Spe_Math.en.Terminale = moyenne[,4],
                     Moyenne.Spe_Phy_chi.en.Terminale = moyenne[,5],
                     Moyenne.Math_Expert.en.Terminale = moyenne[,6], 
                     Moyenne.Ens_Scientifique.en.Premiere = moyenne[,7],
                     Moyenne.Spe_Math.en.Premiere = moyenne[,8],
                     Moyenne.Spe_Phy_chi.en.Premiere = moyenne[,9],
                     Moyenne.en.Terminale = moyenne[,10],
                     Moyenne.en.Premiere = moyenne[,11])
  tab1
}

Tab_Moy <- Moyennes_2(Tab_Notes)

a1<-as.numeric(na.omit(Tab_Moy$Moyenne.Ens_Scientifique.en.Terminale))
b1<-as.numeric(na.omit(Tab_Moy$Moyenne.Spe_Math.en.Terminale))
c1<-as.numeric(na.omit(Tab_Moy$Moyenne.Spe_Phy_chi.en.Terminale))
d1<-as.numeric(na.omit(Tab_Moy$Moyenne.Math_Expert.en.Terminale))


data <- data.frame(
  Matière = c(rep("Enseignement Scientifique",length(a1)),
              rep("Spé. Mathématiques",length(b1)), 
              rep("Spé. Physique-Chimie",length(c1)), 
              rep("Mathématiques Experte",length(d1))),
  Notes = c(a1,b1,c1,d1)
)
#########################################################################################

############################### Visualisation ###########################################
data %>%
  ggplot(aes(x=Matière,y=Notes,fill=Matière))+
  geom_boxplot()+
  geom_jitter(color="black",size=0.4,alpha=0.9)+
  theme(legend.position = "none",plot.title = element_text(size=11))+
  ggtitle("Répartition des moyennes dans les différentes matières scientifiques de Terminale")+
  xlab("")
#########################################################################################

############################### Création de donnée ######################################
e1<-as.numeric(na.omit(Tab_Moy$Moyenne.Ens_Scientifique.en.Premiere))
f1<-as.numeric(na.omit(Tab_Moy$Moyenne.Spe_Math.en.Premiere))
g1<-as.numeric(na.omit(Tab_Moy$Moyenne.Spe_Phy_chi.en.Premiere))

data <- data.frame(
  Matière = c(rep("Enseignement Scientifique",length(e1)),
              rep("Spé. Mathématiques",length(f1)), 
              rep("Spé. Physique-Chimie",length(g1))),
  Notes = c(e1,f1,g1)
)
#########################################################################################

################################# Visualisation #########################################
data %>%
  ggplot(aes(x=Matière,y=Notes,fill=Matière))+
  geom_boxplot()+geom_jitter(color="black",size=0.4,alpha=0.9)+
  theme(legend.position = "none",plot.title = element_text(size=11))+
  ggtitle("Répartition des moyennes dans les différentes matières scientifiques de Première")+
  xlab("")
#########################################################################################

################################# Création de données ##################################
n1 <- nrow(Terminale_Spe_Math_et_Phy_Chi[Terminale_Spe_Math_et_Phy_Chi$Civilité=="Mme",])
n2 <- nrow(Terminale_Spe_Math_et_Phy_Chi[Terminale_Spe_Math_et_Phy_Chi$Civilité=="M.",])

data <- data.frame(Genre = c("Fille","Garçon"),
                   Population = c(n1,n2))

data$fraction = data$Population / sum(data$Population)

data$ymax = cumsum(data$fraction)
data$ymin = c(0,head(data$ymax,n=-1))
#########################################################################################

#################################### Visualisation ######################################
ggplot(data, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=Genre))+
  geom_rect()+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  ggtitle("Proportion Filles/Garçons")
#########################################################################################

################################# Création de données ##################################
Terminale_Spe_Math_et_Phy_Chi$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1 <-
  as.numeric(Terminale_Spe_Math_et_Phy_Chi$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1)

n1 <- nrow(Terminale_Spe_Math_et_Phy_Chi[
  !is.na(Terminale_Spe_Math_et_Phy_Chi$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1),])
n2 <- nrow(Terminale_Spe_Math_et_Phy_Chi[
  is.na(Terminale_Spe_Math_et_Phy_Chi$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1),])

data <- data.frame(Option.Maths.Expert = c("Oui","Non"),
                   Population = c(n1,n2))

data$fraction = data$Population / sum(data$Population)

data$ymax = cumsum(data$fraction)
data$ymin = c(0,head(data$ymax,n=-1))
#########################################################################################

#################################### Visualisation ######################################
ggplot(data, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=Option.Maths.Expert))+
  geom_rect()+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  ggtitle("Proportion Option Maths Expert")
#########################################################################################







#########################################################################################
############################# Analyse Multidimensionnelle ###############################
#########################################################################################

################################# Packages recquis ######################################
library(FactoMineR)
library(factoextra)
library(ggplot2)
#########################################################################################


############################## Importation de données ###################################
df <- read.csv2('TableauDesNotes2.csv',sep=',',na.string="")
#########################################################################################

############################### Création de données #####################################
df <- df[order(df$Classement),] # tri en fonction de la colonne mpg
for (i in 1:nrow(df)){
  df[i,1] <- i
}
rownames(df) <- df$Classement
dt <- df

dt <- select(dt,-c(X,
                   Type.de.formation,
                   Département.établissement.1,
                   Département.établissement,
                   Niveau.d.étude,
                   Type.de.formation.1,
                   Niveau.d.étude.1,
                   Ville.établissement, 
                   Actuellement.en.L1.Math))

for (i in c(1,10:25)) {
  dt[,i] <- as.numeric(dt[,i])
}
dt <- dt[,c(1:2,5,3,4,6:9,10,14,19,23,11,15,20,24,12,16,21,25,13,17,18,22)]
dt <- select(dt,-c(Pays,Scolarité.française))

res.mfa3 <- MFA (dt, 
                 group = c(1,2,2,2,4,4,4,2,2), 
                 type = c("s","n","n","n","s","s","s","s","s"),
                 name.group = c("Classement ParcourSup",
                                "Réussite au Bac",
                                "Profil candidat",
                                "Langues.Vivantes",
                                "Ens.Scientifique", 
                                "Spe.Math",
                                "Spe.P&C",
                                "Math.Expert",
                                "Français"
                 ),
                 num.group.sup = c(), 
                 graph = FALSE)


res.hcpc3 <- HCPC(res.mfa3, graph = FALSE)
#########################################################################################

################################# Visualisation #########################################
fviz_contrib (res.mfa3, choice = "quanti.var", axes = 1, top = 8,
              palette = "jco",labels_track_height = 0.01)
fviz_contrib (res.mfa3, choice = "quali.var", axes = 1, top = 8,
              palette = "jco",labels_track_height = 0.01)
fviz_contrib (res.mfa3, choice = "group", axes = 1, top = 8,
              palette = "jco")
fviz_contrib (res.mfa3, choice = "ind", axes = 1, top = 20,
              palette = "jco")
fviz_screeplot(res.mfa3)

fviz_cluster(res.hcpc3,
             stand = TRUE,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
#########################################################################################




############################## Importation de données ###################################
df <- read.csv2('TableauDesNotes2.csv',sep=',',na.string="")
#########################################################################################

############################### Création de données #####################################
df$Classement <- as.numeric(df$Classement)
df <- df[order(df$Classement),] # tri en fonction de la colonne mpg
for (i in 1:nrow(df)){
  df[i,1] <- i
}
rownames(df) <- df$Classement

#Creation du tableau numeric des notes avec moyennes
CreaTabNum <- function(df){
  X <- df[,c(18:33,1:17,34)]
  for(i in 1:16){
    X[,i]<-type.convert(X[,i], dec= ".")
  }
  return(X)
}

#Sous tableau relatif aux candidats de spécialités PhysiqueChimie et Math (80%)
ReducBaseDonneeLigne <- function(X){
  a1 <- which(is.na(X$Moyenne.candidat.en.Enseignement.scientifique.Trimestre.1), 
              arr.ind = TRUE)
  a2 <- which(is.na(X$Moyenne.candidat.en.Enseignement.scientifique.Trimestre.1.1), 
              arr.ind = TRUE)
  b <- which(is.na(X$Moyenne.candidat.en.Français.Trimestre.1.1), 
             arr.ind = TRUE)
  c1 <- which(is.na(X$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1), 
              arr.ind = TRUE)
  c2 <- which(is.na(X$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1.1), 
              arr.ind = TRUE)
  d1 <- which(is.na(X$Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.1), 
              arr.ind = TRUE)
  d2 <- which(is.na(X$Moyenne.candidat.en.Physique.Chimie.Spécialité.Trimestre.1.1), 
              arr.ind = TRUE)
  
  
  f <- factor(c(a1,a2,b,c1,c2,d1,d2))
  f <- as.numeric(levels(f))
  
  Y <- slice(X,-f)
  return(Y)
}

#Tableau des Moyennes sur 2 ans

Moyennes <- function(tab){
  
  T <- matrix(data=NA,nrow = nrow(tab),ncol = 26)
  
  ls1 <- c(1,5)
  ls2 <- c(2,6)
  ls3 <- c(3,7)
  ls4 <- c(4,8)
  ls5 <- c(9,13)
  ls6 <- c(10,14)
  ls7 <- c(11,15)
  ls8 <- c(12,16)
  
  ls <- data.frame(ls1,ls2,ls3,ls4,ls5,ls6,ls7,ls8)
  
  for (l in 1:nrow(tab)) {
    for (i in 1:ncol(ls)) {
      
      i1 <- ls[1,i]
      i2 <- ls[2,i]
      
      m1 <- tab[l,i1]
      m2 <- tab[l,i2]
      
      if(is.na(m2)&is.na(m1)){
        T[l,i] <- NA
      }else if(is.na(m2)&!is.na(m1)){
        T[l,i] <- m1
      }else{
        T[l,i] <- round((m1+m2)/2, digits = 2)
      }
    }
    
    for(i in 17:34){
      T[l,i-8] <- tab[l,i]
    }
  } 
  tab1 <- data.frame(Moyenne.candidat.en.Enseignement.Scientifique.en.Terminale = T[,1], 
                     Moyenne.candidat.en.Mathématiques.Spécialité.en.Terminale = T[,2],
                     Moyenne.candidat.en.Physique.Chimie.Spécialité.en.Terminale = T[,3],
                     Moyenne.candidat.en.Math_Expert.en.Terminale = T[,4],
                     Moyenne.candidat.en.Francais.en.Premiere = T[,5],
                     Moyenne.Enseignement.Scientifique.en.Premiere = T[,6], 
                     Moyenne.candidat.en.Mathématiques.Spécialité.en.Premiere = T[,7],
                     Moyenne.candidat.en.Physique.Chimie.Spécialité.en.Premiere = T[,8],
                     X = T[,9],
                     Classement = T[,10],
                     Code.aménagement..oui..si = T[,11],
                     Civilité = T[,12],
                     Boursier = T[,13],
                     Département.établissement = T[,14],
                     Mention.diplôme = T[,15],
                     Scolarité.française = T[,16],
                     Type.de.formation = T[,17],
                     Niveau.d.étude = T[,18],
                     Ville.établissement = T[,19],
                     Département.établissement.1 = T[,20],
                     Pays = T[,21],
                     LV1 = T[,22],
                     LV2 = T[,23],
                     Type.de.formation.1 = T[,24],
                     Niveau.d.étude.1 = T[,25],
                     Actuellement.en.L1.Math = T[,26]
  )
  return(tab1)
} 

ConvertNumToNom <- function(tab){
  T <- tab
  for (i in 1:nrow(tab)) {
    if(is.na(tab[i,4])){
      T[i,4] <- 0
    }else{
      T[i,4] <- 1
    }
    if(tab[i,14] == 34){
      T[i,14] <- 0
    }else{
      T[i,14] <- 1
    }
  }
  return(T)
}

ReducBaseDonneeCol <- function(tab){
  T <- tab[,c(1:8,12:15,26)]
  return(T)
}

CreaTabNum2 <- function(df){
  X <- df
  for (i in c(1:3,5:8)){
    X[,i] <- as.numeric(X[,i])
  }
  return(X)
}

PermutCol <- function(df){
  X <- df[,c(1,6,2,7,3,8,4,5,9:13)]
}





A <- CreaTabNum(df)
B <- ReducBaseDonneeLigne(A)
C <- Moyennes(B)
D <- ConvertNumToNom(C)
E <- ReducBaseDonneeCol(D)
F <- CreaTabNum2(E)
G <- PermutCol(F)


Tab <- G

res.mfa <- MFA (Tab, 
                group = c(2,2,2,1,1,1,1,1,1,1), 
                type = c("s","s","s","n","s","n","n","n","n","n"),
                name.group = c("Moy.en.Ens.Scientifique", 
                               "Moy.en.Spe.Math", 
                               "Moy.en.Spe.Physique.Chimie", 
                               "Option.Math.Expert", 
                               "Moy.en.Français", 
                               "Civilité", 
                               "Boursier",
                               "Departement.Etablissement",
                               "Mention.Diplome",
                               "Actuellement.en.L1.Math"),
                num.group.sup = c(4,5,6,7,8,9,10), 
                graph = FALSE)
#########################################################################################

################################# Visualisation #########################################
eig.val <- get_eigenvalue(res.mfa)
head(eig.val)

fviz_screeplot(res.mfa)
group <- get_mfa_var(res.mfa, "group")

# Coordonnées des groupes
head(group$coord)
# Cos2: qualité de représentation des groupes
head(group$cos2)
# Contributions des dimensions
head(group$contrib)

#Pour visualiser les groupes de variables, tapez ceci:
fviz_mfa_var(res.mfa, "group")

# Contribution à la première dimension
fviz_contrib (res.mfa, "group", axes = 1)
# Contribution à la deuxième dimension
fviz_contrib (res.mfa, "group", axes = 2)

quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 

# Coordonnées
head(quanti.var$coord)
# Cos2: qualité de représentation
head(quanti.var$cos2)
# Contributions aux dimensions
head(quanti.var$contrib)

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

# Contributions à la dimension 1
fviz_contrib (res.mfa, choice = "quanti.var", axes = 1, top = 20,
              palette = "jco")

# Contributions à la dimension 2
fviz_contrib (res.mfa, choice = "quanti.var", axes = 2, top = 20,
              palette = "jco")

#Graphique des individus
#Extraire le résultat des individus:
ind <- get_mfa_ind(res.mfa)

#Visualisation des individus. Coloration en fonction du cosinus carré:
fviz_mfa_ind(res.mfa, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE)


fviz_mfa_ind(res.mfa, 
             habillage = "Actuellement.en.L1.Math", # color by groups 
             palette = c("green", "blue"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE 
) 


# 2. HCPC
res.hcpc <- HCPC(res.mfa, graph = FALSE)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.1      # Augment l'espace pour le texte
)

fviz_screeplot(res.mfa)

fviz_contrib (res.mfa, choice = "quanti.var", axes = 1, top = 20,
              palette = "jco",labels_track_height = 0.01)

fviz_contrib (res.mfa, choice = "quanti.var", axes = 2, top = 20,
              palette = "jco")

fviz_cluster(res.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# Principal components + tree
plot(res.hcpc, choice = "3D.map")


#########################################################################################






#########################################################################################
################################# Régression Linéaire ###################################
#########################################################################################



#################################### Packages ###########################################
library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)
library(GGally)
#########################################################################################

################################### Traitement des données ##############################
notes_S1 <- read.csv2('notes-S1-promo-anonyme.csv',sep=',',na.string="")

dg <- read.csv2('TableauDesNotesDesEtudiantsEnL1Math.csv',sep=',',na.string=" ", dec=".")
dg <- dg[-13,]
dg$Classement <- as.numeric(dg$Classement)
y <- c(1:32)

for (i in 21:29){
  dg[,i] <- as.numeric(dg[,i])
}

dg$Moyenne.S1 <- as.numeric(dg$Moyenne.S1)
dg$Moyenne.S1 <- round(dg$Moyenne.S1,digits = 2)
dg$Moyenne.Maths <- as.numeric(dg$Moyenne.Maths)
dg$Moyenne.Maths <- round(dg$Moyenne.Maths,digits = 2)
##########################################################################################

################################### Test de corrélation ##################################
dg1 <- dg[,c(1,17:29)]

for (i in 1:nrow(dg1)) {
  if (dg1$Civilité[i] == "M."){
    dg1$Civilité[i] <- 1
  }else{
    dg1$Civilité[i] <- 2
  }
  if (dg1$Boursier[i] == "Boursier"){
    dg1$Boursier[i] <- 1
  }else{
    dg1$Boursier[i] <- 0
  }
  if (dg1$Mention.diplôme[i] == "Admis mention Assez Bien"){
    dg1$Mention.diplôme[i] <- 1
  }else if(dg1$Mention.diplôme[i] == "Admis mention Bien") {
    dg1$Mention.diplôme[i] <- 2
  }else if(dg1$Mention.diplôme[i] == "Admis mention Très Bien") {
    dg1$Mention.diplôme[i] <- 3
  }else if(dg1$Mention.diplôme[i] == "Admis sans mention") {
    dg1$Mention.diplôme[i] <- 0
  }
  if (dg1$Département.établissement[i] == 34){
    dg1$Département.établissement[i] <- 1
  }else{
    dg1$Département.établissement[i] <- 2
  }
}
for (i in 2:5) {
  dg1[,i] <- as.numeric(dg1[,i])
}

ggcorr(dg1,
       nbreaks = 10,
       method = c("pairwise", "pearson"),
       hjust = 1, 
       size = 3,
       label = TRUE,
       label_size = 3,
       label_round = 2,
       label_alpha = TRUE,
       legend.size = 10,
       color = "grey50",
       layout.exp = 2)
#########################################################################################

############################## Test de Linéarité sur les variables numérique ############
a <- ggscatter(dg,  
               x = "Classement", 
               y = "Moyenne.Maths", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 400)
summary(lm(Moyenne.Maths ~ (Classement),data = dx))


#scatterplot(Moyenne.Maths ~ Moy.Ens.Scientifique.Terminale, data = dx)
b <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Ens.Scientifique.Terminale", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Ens.Scientifique.Terminale,data = dx))

#scatterplot(Moyenne.Maths ~ Moy.Spe.Math.Terminale, data = dx)
c <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Spe.Math.Terminale", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Spe.Math.Terminale,data = dx))

#scatterplot(Moyenne.Maths ~ Moy.Spe.Phy.Chi.Terminale, data = dx)
d <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Spe.Phy.Chi.Terminale", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Spe.Phy.Chi.Terminale,data = dx))

#scatterplot(Moyenne.Maths ~ (Moy.Math.Expert), data = dx)
e <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Math.Expert", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Math.Expert,data = dx))

#scatterplot(Moyenne.Maths ~ Moy.Ens.Scientifique.Premiere, data = dx)
f <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Ens.Scientifique.Premiere", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Ens.Scientifique.Premiere,data = dx))

#scatterplot(Moyenne.Maths ~ Moy.Spe.Math.Premiere, data = dx)
g <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Spe.Math.Premiere", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Spe.Math.Premiere,data = dx))

#scatterplot(Moyenne.Maths ~ Moy.Spe.Phy.Chi.Premiere, data = dx)
h <- ggscatter(dg,  
               y = "Moyenne.Maths", 
               x = "Moy.Spe.Phy.Chi.Premiere", 
               add = "reg.line", 
               conf.int = TRUE
) + 
  stat_cor(method = "pearson", label.x = 10)
summary(lm(Moyenne.Maths ~ Moy.Spe.Phy.Chi.Premiere,data = dx))

library(gridExtra)
grid.arrange(a, b, c, d,ncol=2, nrow = 2)
grid.arrange(e, f, g, h,ncol=2, nrow = 2)
##############################################################################

###################################### Liaison Linéaire #######################
library(car)
scatterplotMatrix(dg[,c(16,1,21:22)])
scatterplotMatrix(dg[,c(16,23:25)])
scatterplotMatrix(dg[,c(16,26:27)])
###################################################################################

################################## Ajustement du Modèle ###########################
dg_comp <- dg[,c(1,16,17:29)]
dg_comp <- dg_comp %>% 
  mutate_if(is.character, as.factor)
dg_comp <- dg_comp %>%
  select(-Moy.Generale.Terminale,
         -Moy.Generale.Premiere,
         -Département.établissement, 
         -Moy.Math.Expert)
mod.rlm1 <- lm(Moyenne.Maths~.,dg_comp)
summary(mod.rlm1)
###################################################################################


################################## Capture Output #################################
dg_comp <- dg[,c(1,16,17:29)]
dg_comp <- dg_comp %>% 
  +   mutate_if(is.character, as.factor)
dg_comp <- dg_comp %>%
  +   select(-Moy.Generale.Terminale,-Moy.Generale.Premiere,-Département.établissement)
mod.rlm1 <- lm(Moyenne.Maths~.,dg_comp)
summary(mod.rlm1)
###################################################################################


######################################## VIF ######################################
library(performance)
dg_comp <- dg[,c(1,16,17:29)]
dg_comp <- dg_comp %>% 
  mutate_if(is.character, as.factor)
dg_comp <- dg_comp %>%
  select(-Moy.Generale.Terminale,
         -Moy.Generale.Premiere,
         -Département.établissement, 
         -Moy.Math.Expert)
mod.rlm1 <- lm(Moyenne.Maths~.,dg_comp)
check_collinearity(mod.rlm1)

dg_comp <- dg[,c(1,16,17:29)]
dg_comp <- dg_comp %>% 
  mutate_if(is.character, as.factor)
dg_comp <- dg_comp %>%
  select(-Moy.Generale.Terminale,
         -Moy.Generale.Premiere,
         -Département.établissement, 
         -Moy.Math.Expert,
         -Classement)
mod.rlm <- lm(Moyenne.Maths~.,dg_comp)
check_collinearity(mod.rlm1)

dg_comp <- dg[,c(1,16,17:29)]
dg_comp <- dg_comp %>% 
  mutate_if(is.character, as.factor)
dg_comp <- dg_comp %>%
  select(-Moy.Generale.Terminale,
         -Moy.Generale.Premiere,
         -Département.établissement, 
         -Moy.Math.Expert,
         -Classement,
         -Mention.diplôme)
mod.rlm <- lm(Moyenne.Maths~.,dg_comp)
check_collinearity(mod.rlm)

dg_comp <- dg[,c(1,16,17:29)]
dg_comp <- dg_comp %>% 
  mutate_if(is.character, as.factor)
dg_comp <- dg_comp %>%
  select(-Moy.Generale.Terminale,
         -Moy.Generale.Premiere,
         -Département.établissement, 
         -Moy.Math.Expert,
         -Classement)
mod.rlm <- lm(Moyenne.Maths~.,dg_comp)
summary(mod.rlm)
check_collinearity(mod.rlm)
###############################################################################################


########################### Normalité et Homoscédasticité des résidu ##########################
check_model(mod.rlm)
check_normality(mod.rlm)
check_heteroscedasticity(mod.rlm)
###############################################################################################


########################### Interpretations des résultats du modèle complet ###################
summary(mod.rlm)
car::Anova(mod.rlm)
###############################################################################################


###############################################################################################
mod.rlm2 <- update(mod.rlm, .~.-Moy.Ens.Scientifique.Premiere)
Anova(mod.rlm2)

mod.rlm3 <- update(mod.rlm2, .~.-Moy.Spe.Phy.Chi.Premiere)
Anova(mod.rlm3)

mod.rlm4 <- update(mod.rlm3, .~.-Moy.Spe.Math.Premiere)
Anova(mod.rlm4)

mod.rlm5 <- update(mod.rlm4, .~.-Moy.Spe.Phy.Chi.Terminale)
Anova(mod.rlm5)

mod.rlm6 <- update(mod.rlm5, .~.-Civilité)
Anova(mod.rlm6)

mod.rlm7 <- update(mod.rlm6, .~.-Moy.Spe.Math.Terminale)
Anova(mod.rlm7)
summary(mod.rlm7)
check_model(mod.rlm7)
check_normality(mod.rlm7)
check_heteroscedasticity(mod.rlm7)
check_outliers(mod.rlm7)



###############################################################################################


###############################################################################################
library(finalfit)
explanatory_final = c("Boursier", 
                      "Mention.diplôme", 
                      "Moy.Ens.Scientifique.Terminale")
dependent = 'Moyenne.Maths'
dg_comp %>%
  lmmulti(dependent, explanatory_final)  %>% 
  fit2df()

###############################################################################################


###############################################################################################

dg_comp$Boursier <- as.character(dg_comp$Boursier)
dg_comp$Mention.diplôme <- as.character(dg_comp$Mention.diplôme)


droiteReg <- function(T){
  
  x <- matrix(nrow = nrow(T),ncol = 2)
  for (i in 1:nrow(T)) {
    a <- 0 
    b <- 0
    if (T[i,3]=="Non boursier"){
      a <- 3.91
    } else if(T[i,4] == "Admis mention Bien") {
      b <- 2.23
    }else if(T[i,4] == "Admis mention Très Bien") {
      b <- 7.99
    }else if(T[i,4] == "Admis sans mention") {
      b <- -2.83
    }
    x[i,1] <- 20.18 - a + b - 0.65*T[i,5]
    x[i,2] <- T[i,1]
  }
  return(x)
}

X <- droiteReg(dg_comp)
X <- as.data.frame(X)
colnames(X) <- c("Valeur.Predite", "Moyenne.Math")

ggplot(X, aes(x = Moyenne.Math, y = Valeur.Predite)) + 
  geom_point() +
  geom_smooth(method=lm)

dg_comp[1,5]






