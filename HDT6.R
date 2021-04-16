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
library(dummies)

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


porcentaje<-0.7
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

head(train)


head(test)


#Modelo de regresion logistica
#CasasCaras
modelo<-glm(datos1~., data = train[,c(2:10,12)],family = binomial(), maxit=100)
modelo

modelo2<-glm(datos2~., data = train[,c(2:10,13)],family = binomial(), maxit=100)
modelo2

modelo3<-glm(datos3~., data = train[,c(2:10,14)],family = binomial(), maxit=100)
modelo3

#Graficas
#Casas Caras
ggplot(data = train[,c("GrLivArea","datos1")], aes(x = GrLivArea, y = datos1)) +
  geom_point(aes(color = as.factor(datos1)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "pink",
              se = FALSE) +
  theme_bw() +
  labs(title = "Regresion logistica: Casas Caras",
       y = "Probabilidad default") +
  theme(legend.position = "none")

#Casas Intermedias

ggplot(data = train[,c("GrLivArea","datos2")], aes(x = GrLivArea, y = datos2)) +
  geom_point(aes(color = as.factor(datos2)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "pink",
              se = FALSE) +
  theme_bw() +
  labs(title = "Regresion logistica: Casas Intermedias",
       y = "Probabilidad default") +
  theme(legend.position = "none")

#Casas Economicas

ggplot(data = train[,c("GrLivArea","datos3")], aes(x = GrLivArea, y = datos3)) +
  geom_point(aes(color = as.factor(datos3)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "pink",
              se = FALSE) +
  theme_bw() +
  labs(title = "Regresion logistica: Casas Economicas",
       y = "Probabilidad default") +
  theme(legend.position = "none")



# Determinaci?n de la colinealidad de las variables
#casas Caras
matriz_cor <- cor(datos[,c(1:10,12)])
matriz_cor
corrplot(matriz_cor)

confusionMatrix(as.factor(prediccion),as.factor(test$datos1))

#casas Intermedias
matriz_cor1 <- cor(datos2[,c(1:10,13)])
matriz_cor1
corrplot(matriz_cor1)

#casas Economicas
matriz_cor2 <- cor(datos3[,c(1:10,14)])
matriz_cor2
corrplot(matriz_cor2)


pred<-predict(modelo,newdata = test[,1:14], type = "response")
prediccion<-ifelse(pred>=0.8,1,0)




# Cantidad de predicciones falladas
test[which(test[,12]!=test[,14]),]

count(test[which(test[,12]!=test[,14]),])

# Se elimna la columna extra de prediccion para trabajar con estos datos nuevamente
test[,15]<- NULL

# Matriz de confusion para el modelo de regresion logistica 
confusionMatrix(as.factor(prediccion),as.factor(test$datos1))

confusionMatrix(as.factor(prediccion),as.factor(test$datos2))


confusionMatrix(as.factor(prediccion),as.factor(test$datos3))


# Naive Bayes
train$datos1 <- as.factor(train$datos1)
modelo<-naiveBayes(datos1~.,data=train)

#Matriz de confusion
testing <- test[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd","SalePrice","datos1")]
testing <- na.omit(testing)
testing$datos1 <- as.factor(testing$datos1)

predBayes<-predict(modelo, newdata = testing[,1:11])
predBayes

confusionMatrix(as.factor(testing$datos1),as.factor(predBayes))

#Arbol de Decisi?n
dt_model<-rpart(train$datos1~.,train,method = "class")
rpart.plot(dt_model)

prediccion <- predict(dt_model, newdata = test[1:11])

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta

cfm<-table(test$datos1,test$prediccion)
cfm

#Random Forest
modeloRF1<-randomForest(train$datos1~.,train)
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<-test
testCompleto$predRF<-prediccionRF1
testCompleto$predRF<-testCompleto$predRF
cfmRandomForest <- table(testCompleto$predRF, testCompleto$datos1)
plot(cfmRandomForest);text(cfmRandomForest)

cfmRandomForest <- confusionMatrix(table(testCompleto$predRF, testCompleto$datos1))
cfmRandomForest

#regresion lineal
fitMLM_SalePrice<-lm(datos1~.,data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "datos1")])
predMLM<-predict(fitMLM_SalePrice,newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd")])

plot(test$datos1,col="blue")
points(predMLM, col="red")

