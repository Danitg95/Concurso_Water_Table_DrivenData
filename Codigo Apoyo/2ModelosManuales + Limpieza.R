# Cargo las funciones que voy a utilizar despu?s
source("FuncionesMineriaAida.R") #no pongo la ruta porque est? en el mismo directorio


# Cargo las librerias que me van a hacer falta
library(questionr)
library(psych)
library(car)
library(Hmisc)
library(readxl)
library(ggplot2)
library(corrplot)
library(caret)
library(questionr)
library(gridExtra)
library(tidyr)
library(OneR)
library(pROC)


datos<-readRDS("datosElecciones2")
varObjBin <- as.factor(datos$varObjBin)
varObjCont<-datos$varObjCont
input<-as.data.frame(datos[,-(1:2)])
#### Comienzo con la regresi?n lineal
todo<-data.frame(varObjCont,input)


#Obtengo la partici?n
set.seed(12345)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

#Construyo un modelo preliminar con todas las variables
modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)
modelo1$rank #vemos el n?mero de par?metros que tiene
Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test)
par(mar=c(7, 9, 4.1, 2.1))
importanciaVariables(modelo1) 

#Pruebo un modelo con menos variables, bas?ndome en la importancia de las variables
modelo2<-lm(varObjCont~CCAA+ServiciosPtge+Age_under19_Ptge+Inmuebles
            +totalEmpresas,data=data_train)
summary(modelo2)
modelo2$rank
Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test) 
importanciaVariables(modelo2) 

#Pruebo otro modelo con menos variables
modelo3<-lm(varObjCont~CCAA+ServiciosPtge+Age_under19_Ptge+Inmuebles+totalEmpresas+CCAA:ServiciosPtge+
              CCAA:Inmuebles+Inmuebles:totalEmpresas+CCAA:Age_under19_Ptge,data=data_train)
summary(modelo3)
modelo3$rank
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 
importanciaVariables(modelo3) 

#Pruebo con una interaccion sobre el anterior
modelo4<-lm(varObjCont~CCAA+ServiciosPtge+Inmuebles+totalEmpresas+CCAA:ServiciosPtge+CCAA:Inmuebles
            +CCAA:Age_under19_Ptge,data=data_train)
modelo4$rank
Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test)
importanciaVariables(modelo4) #no parece que aporte mucho la interaccion

#Pruebo con una interaccion sobre el anterior
modelo5<-lm(varObjCont~CCAA+Inmuebles+CCAA:Age_under19_Ptge,data=data_train)
modelo5$rank
Rsq(modelo5,"varObjCont",data_train)
Rsq(modelo5,"varObjCont",data_test)
importanciaVariables(modelo5) 

#Pruebo con una interaccion sobre el anterior
modelo6<-lm(varObjCont~CCAA+ServiciosPtge,data=data_train)
modelo6$rank
Rsq(modelo6,"varObjCont",data_train)
Rsq(modelo6,"varObjCont",data_test)
importanciaVariables(modelo6) 


#Comparamos los modelos
modelos<-list(modelo1,modelo2,modelo3,modelo4, modelo5, modelo6)
sapply(modelos,function(x) x$rank)
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
#por el principio de parsimonia, nos quedamos con el 6


# Si quisieramos mayor capacidad predictiva, ganarÃ­a el modelo 1 pero con un mayor nÂº de pram
# En caso de que la simplicidad sea mas importante que la predicciÃ³n, tomariamos el 6 que tiene 9
# variables menos a costa de perder solametne un 4% de capacidad predictiva

# Vemos los coeficientes del modelo ganador y los interpretamos
coef(modelo6)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(modelo6,"varObjCont",data_train)
Rsq(modelo6,"varObjCont",data_test) 

# Vemos las variables m?s importantes del modelo ganador
importanciaVariables(modelo6) 


