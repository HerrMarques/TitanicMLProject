library(randomForest)

# Définir le nouveau répertoire de travail ----
setwd("/Users/alexmarques/Programmieren/Portfólio/Kaggle/TitanicMLProject/")

# Téléchargez les ensembles de données----
ensemble_dapprentissage <- read.csv("input/train.csv",stringsAsFactors = FALSE, header = TRUE)
ensemble_test  <- read.csv("input/test.csv", header = TRUE,stringsAsFactors = FALSE)

ensemble_dapprentissage$Tester <- FALSE
ensemble_test$Tester <- TRUE
ensemble_test$Survived <- NA

tout<- rbind(ensemble_dapprentissage,ensemble_test)

# Nettoyer les donnés----

tout[tout$Embarked=='',"Embarked"]<-"S"

limite_dage<-boxplot.stats(tout$Age)$stats[5]
filtre_outlier<-tout$Age<= limite_dage
equation_dage="Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
modele_age<- lm(
  formula = equation_dage,
  data = tout[filtre_outlier,]
                )
predictions_dage<- round(predict(modele_age,
                  newdata = tout[is.na(tout$Age),c("Pclass", "Sex", "Fare", "SibSp" , "Parch"  , "Embarked")]),
                  2
                        )
tout[is.na(tout$Age),"Age"]<-predictions_dage

fare.moyenne <- median(tout$Fare,na.rm = TRUE)
tout[is.na(tout$Fare),"Fare"]<- fare.moyenne

tout$Pclass<-as.factor(tout$Pclass)
tout$Sex<-as.factor(tout$Sex)
tout$Ticket<-as.factor(tout$Ticket)
tout$Cabin<-as.factor(tout$Cabin)
tout$Embarked<-as.factor(tout$Embarked)

rm(modele_age,equation_dage,filtre_outlier,limite_dage,predictions_dage)

# Séparer à nouveau les ensembles de données
ensemble_dapprentissage<- tout[tout$Tester==FALSE,]
ensemble_test<- tout[tout$Tester==TRUE,]

ensemble_dapprentissage$Survived<- as.factor(ensemble_dapprentissage$Survived)

equation_survecu<- "Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch + Embarked"
survecu<- as.formula(equation_survecu)
modele<- randomForest(formula=survecu,data=ensemble_dapprentissage,ntree=500,mtry=3,
             node=0.001*nrow(ensemble_dapprentissage))

survecu_finalle<- predict(modele,newdata=ensemble_test)