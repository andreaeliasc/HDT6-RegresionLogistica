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

#variables dicotomicas
datos$grupo <- ifelse(datos$SalePrice<178000, "3", 
                      ifelse(datos$SalePrice<301000, "2",
                             ifelse(datos$SalePrice<756000,"1",NA)))

datos$grupo2 <- ifelse(datos$SalePrice<178000, "3", 
                      ifelse(datos$SalePrice<301000, "2",NA))

datos$grupo3 <- ifelse(datos$SalePrice<178000, "3", NA)

datos$grupo <- as.factor(datos$grupo)
datos$grupo2 <- as.factor(datos$grupo2)
datos$grupo3 <- as.factor(datos$grupo3)

datos<-cbind(datos,dummy(datos$grupo,verbose = T),dummy(datos$grupo2,verbose = T), dummy(datos$grupo3,verbose = T))

datos <- datos[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice", "datos1","datos2","datos3")]

datos <- na.omit(datos)

head(datos, 30)
