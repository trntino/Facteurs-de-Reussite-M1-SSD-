setwd("/Users/sonia/Desktop/Projet S2/Success/ParcourSup")


liste_admis <- read.csv2('liste-admis.csv',sep=';',na.string="")
classement_finalparcoursup <- read.csv2('classement-finalparcoursup.csv',sep=';',na.string="")
TableauDesNotes <- read.csv2('TableauDesNotes.csv',sep=',',na.string="")
AMC <- read.csv2('AMC-test-rentree-2021-anonyme.csv',sep=',',na.string="")
correspondance_V2 <- read.csv2('correspondance-V2.csv',sep=',',na.string="")
correspondance <- read.csv2('correspondance.csv',sep=',',na.string="")
View(classement_finalparcoursup)
View(TableauDesNotes)
View(AMC)
View(correspondance_V2)
View(correspondance)


#################################################################################
notes_S1 <- read.csv2('notes-S1-promo-anonyme.csv',sep=',',na.string="")


notes_S1$X <- as.numeric(notes_S1$X)
for (i in 3:nrow(notes_S1)) {
  if(is.na(notes_S1[i,1])){
    notes_S1 <- notes_S1[-i,]
  }
}

notes_S1$X.4 <- as.numeric(notes_S1$X.4)
for (i in 3:nrow(notes_S1)) {
  if(is.na(notes_S1$X.4[i])){
    notes_S1 <- notes_S1[-i,]
  }
}


colnames(notes_S1) <- notes_S1[1,1:16]

names(notes_S1)[1:3] <- c("Classement","Code Test rentree", "Code Notes S1")

names(notes_S1)[16] <- "Moyenne Maths"

notes_S1 <- notes_S1[-c(1,2),]

T <- intersect(notes_S1$Classement,df$Classement)


###################################################################################
df <- read.csv2('TableauDesNotesDesCandidatsEnL1.csv',sep=' ',na.string="")
notes_S1 <- read.csv2('NotesPremierSemestre.csv',sep=',',na.string="")
X <- read.csv2('TableauDesMoyennesEnSciences.csv',sep = ',', na.strings = "NA")

View(notes_S1)
View(df)
View(X)

dg <- notes_S1
dg$Classement <- as.numeric(dg$Classement)
df$Classement <- as.numeric(df$Classement)
df$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1 <- as.numeric(df$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1 )
df$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1 <- as.numeric(df$Moyenne.candidat.en.Mathématiques.Expertes.Trimestre.1)
notes_S1$Classement <- as.numeric(notes_S1$Classement)
X$Classement <- as.numeric(X$Classement)
X$Moyenne.Ens_Scientifique.en.Terminale <- as.numeric(X$Moyenne.Ens_Scientifique.en.Terminale)
X$Moyenne.Spe_Math.en.Terminale<- as.numeric(X$Moyenne.Spe_Math.en.Terminale)
X$Moyenne.Spe_Phy_chi.en.Terminale<- as.numeric(X$Moyenne.Spe_Phy_chi.en.Terminale)
X$Moyenne.Math_Expert.en.Terminale<- as.numeric(X$Moyenne.Math_Expert.en.Terminale)
X$Moyenne.Ens_Scientifique.en.Premiere<- as.numeric(X$Moyenne.Ens_Scientifique.en.Premiere)
X$Moyenne.Spe_Math.en.Premiere<- as.numeric(X$Moyenne.Spe_Math.en.Premiere)
X$Moyenne.Spe_Phy_chi.en.Premiere<- as.numeric(X$Moyenne.Spe_Phy_chi.en.Premiere)
X$Moyenne.en.Terminale<- as.numeric(X$Moyenne.en.Terminale)
X$Moyenne.en.Premiere<- as.numeric(X$Moyenne.en.Premiere)
T <- as.numeric(intersect(df$Classement,notes_S1$Classement))
V<- as.numeric(intersect(X$Classement,notes_S1$Classement))

treizecolonnes<-data.frame(matrix("x",nrow = nrow(dg),13))
names(treizecolonnes)<-c("Civilité","Boursier","Département.établissement","Mention.diplôme",
                         "Moy.Ens.Scientifique.Terminale",
                         "Moy.Spe.Phy.Chi.Terminale","Moy.Spe.Math.Terminale", "Moy.Math.Expert",
                         "Moy.Ens.Scientifique.Premiere","Moy.Spe.Math.Premiere","Moy.Spe.Phy.Chi.Premiere",
                         "Moy.Generale.Terminale","Moy.Generale.Premiere")
dg<-cbind(dg,treizecolonnes)
head(dg)



for (i in 1:nrow(notes_S1)) {
  k <- 0
  for (j in T) {
    
    if(is.na(dg$Classement[i])){
      dg <- dg[-i,]
    } else if (dg$Classement[i]==j){
      k <- 1
      for (n in 1:nrow(df)) {
        if(!(is.na(df$Classement[n])) & df$Classement[n]==j){
          dg$Civilité[i] <- df$Civilité[n]
          dg$Boursier[i] <- df$Boursier[n]
          dg$Département.établissement[i] <- df$Département.établissement[n]
          dg$Mention.diplôme[i] <- df$Mention.diplôme[n]
          dg$Moy.Spe.Math.Terminale[i] <- df$Moyenne.candidat.en.Mathématiques.Spécialité.Trimestre.1[n]
          
        }
      }
      for (n in 1:nrow(X)) {
        if(!(is.na(X$Classement[n])) & X$Classement[n]==j){
          
          dg$Moy.Ens.Scientifique.Terminale[i] <- X$Moyenne.Ens_Scientifique.en.Terminale[n]
          dg$Moy.Spe.Math.Terminale[i] <- X$Moyenne.Spe_Math.en.Terminale[n]
          dg$Moy.Spe.Phy.Chi.Terminale[i] <- X$Moyenne.Spe_Phy_chi.en.Terminale[n]
          dg$Moy.Math.Expert[i] <- X$Moyenne.Math_Expert.en.Terminale[n]
          dg$Moy.Ens.Scientifique.Premiere[i] <- X$Moyenne.Ens_Scientifique.en.Premiere[n]
          dg$Moy.Spe.Math.Premiere[i] <- X$Moyenne.Spe_Math.en.Premiere[n]
          dg$Moy.Spe.Phy.Chi.Premiere[i] <- X$Moyenne.Spe_Phy_chi.en.Premiere[n]
          dg$Moy.Generale.Terminale[i] <- X$Moyenne.en.Terminale[n]
          dg$Moy.Generale.Premiere[i] <- X$Moyenne.en.Premiere[n]
          
          
        }
        
      }
      
    }
  }
  if(k == 0){
    dg <- dg[-i,]
  }
}



for (i in 1:nrow(dg)) {
  if(dg$Civilité[i]=="x"){
    dg <- dg[-i,]
  }
}
View(dg)

write.table(dg,file = 'TableauDesNotesDesEtudiantsEnL1Math.csv',sep=',')



###############################################################################################

df <- read.csv2('TableauDesNotes.csv',sep=',',na.string="")
notes_S1 <- read.csv2('NotesPremierSemestre.csv',sep=',',na.string="")
T <- intersect(notes_S1$Classement,df$Classement)
Tab <- read.csv2('TableauFinalDesNotes.csv',sep=',',na.string="")

colonne <- data.frame(matrix("x",nrow = nrow(df),1))
names(colonne) <- "Actuellement.en.L1.Math"

X <- cbind(df,colonne)
X$Classement <- as.numeric(X$Classement)
T <- as.numeric(T)

for (i in 1:nrow(X)) {
  k<-0
  for (j in T) {
    if (is.na(X$Classement[i])){
      k <- 0
    }else if(X$Classement[i]==j){
      k <- 1
    }
  }
  if(k==1){
    X$Actuellement.en.L1.Math[i] <- "oui"
  }else{
    X$Actuellement.en.L1.Math[i] <- "non"
  }
}

table(X$Actuellement.en.L1.Math)







