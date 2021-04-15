library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(tidyr)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(rpart)
library(corrplot)

# Analisis Exploratorio
train<- read.csv("train.csv", stringsAsFactors = FALSE)
test<- read.csv("test.csv", stringsAsFactors = FALSE)
train<-train[1:1460,]

glimpse(train[1:10,])

#separacion de datos para training y testing, donde se deja el 70% 
#para training y el 30% para testing
porcentaje<-0.7
datos<-read.csv("train.csv", stringsAsFactors = FALSE)
set.seed(123)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

# Revisamos el tipo de datos del conjunto de entrenamiento
head(train)

# Revisamos el tipo de datos del conjunto de test
head(test)

glimpse(train[1:10,])
scatter.smooth(train$LotFrontage, train$SalePrice)
scatter.smooth(train$LotArea, train$SalePrice)
scatter.smooth(train$GrLivArea, train$SalePrice)
scatter.smooth(train$YearBuilt, train$SalePrice)
scatter.smooth(train$BsmtUnfSF, train$SalePrice)
scatter.smooth(train$TotalBsmtSF, train$SalePrice)
scatter.smooth(train$X1stFlrSF, train$SalePrice)
scatter.smooth(train$GarageYrBlt, train$SalePrice)
scatter.smooth(train$GarageArea, train$SalePrice)
scatter.smooth(train$YearRemodAdd, train$SalePrice)
scatter.smooth(train$TotRmsAbvGrd, train$SalePrice)
scatter.smooth(train$MoSold, train$SalePrice)
scatter.smooth(train$OverallQual, train$SalePrice)
