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


# DIA4 --------------------------------------------------------------------


# Parto de los datos sin at?picos ni ausentes ni las variables "inutiles"
datos<-readRDS("datosElecciones3")
varObjCont<-datos$varObjCont
varObjBin<-datos$varObjBin
input<-datos[,-(1:2)]

#### Comienzo con la regresi?n log?stica, creo un dataframe con las input y la objBin
todo<-data.frame(varObjBin,input)
summary(todo)

#Hago la partici?n
set.seed(12345)
trainIndex <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

#pruebo un primer modelo con todas las variables
modeloInicial<-glm(varObjBin~.,data=data_train,family=binomial)
summary(modeloInicial)
pseudoR2(modeloInicial,data_train,"varObjBin")
pseudoR2(modeloInicial,data_test,"varObjBin")
modeloInicial$rank #n?mero de par?metros

par(mar=c(9, 12, 4.1, 2.1))#ajustar el segundo valor si no cabe el nombre de las variables
importanciaVariablesLog(modeloInicial) 

#fijandome en la importancia de las variables, selecciono s?lo las m?s importantes
modelo2<-glm(varObjBin~CCAA+Age_over65_Ptge+AgricultureUnemploymentPtge+
               IndustriaPtge+Densidad+ConstructionUnemploymentPtge,
             data=data_train,family=binomial)
pseudoR2(modelo2,data_train,"varObjBin")#es un poquito mejor que el anterior y el n. de parametros es casi la mitad
pseudoR2(modelo2,data_test,"varObjBin")
modelo2$rank
importanciaVariablesLog(modelo2)  #quiz?s sobre Acidez y pH

#lo pruebo
modelo3<-glm(varObjBin~CCAA+Age_over65_Ptge+AgricultureUnemploymentPtge,
             data=data_train,family=binomial)
pseudoR2(modelo3,data_train,"varObjBin")
pseudoR2(modelo3,data_test,"varObjBin")
modelo3$rank
importanciaVariablesLog(modelo3)


#lo pruebo
modelo4<-glm(varObjBin~CCAA,data=data_train,family=binomial)
pseudoR2(modelo4,data_train,"varObjBin")
pseudoR2(modelo4,data_test,"varObjBin")
modelo4$rank
importanciaVariablesLog(modelo4)


#Los comparamos
modelos<-list(modeloInicial,modelo2,modelo3, modelo4)
sapply(modelos,function(x) pseudoR2(x,data_test,"varObjBin"))
sapply(modelos,function(x) x$rank) 
#Es mejor el 1, es el unico que da un valor de prediccion aceptable
exp(modelo3$coefficients) #obtengo los odds-ratio y los interpreto

#Obtenemos todas las transformaciones
Transfbin<-Transf_Auto(Filter(is.numeric, input[,-c(14,15)]),varObjBin)#indico que no quiero transformar las aleatorias (estan en las columnas 13 y 14)
names(Transfbin)

#Pruebo la discretizaci?n de las variables cuantitativas
discbin<-droplevels(optbin(data.frame(Filter(is.numeric, input[,-c(14,15)]),varObjBin)))[,-(ncol(Filter(is.numeric, input[,-c(14,15)]))+1)]
names(discbin)<-paste("disc", names(discbin), sep = "_")

#Verificamos el reparto de las nuevas categor?as
#Al fusionar perdemos informaci?n
apply(discbin,2,freq) #todas est?n bien representadas

#Guardo todo
datos_todobin<-data.frame(varObjBin,input,Transfbin,discbin)
#rehago la partici?n
data_train <- datos_todobin[trainIndex,]
data_test <- datos_todobin[-trainIndex,]

#Probamos modelos de selecci?n de variables
#Pruebo s?lo STEP con BIC por no alargar la clase
#Se deben probar todas las combinaciones de AIC/BIC y forw/back/step
#para todos los "full"

null<-glm(varObjBin~1,data=data_train,family=binomial) #Modelo minimo

full<-glm(varObjBin~.,data=data_train[,1:16],family=binomial)

fullInt<-glm(varObjBin~.^2,data=data_train[,1:16],family=binomial)

fullT<-glm(varObjBin~.,data=data_train[,1:27],family=binomial)

fullTodo<-glm(varObjBin~.,data=data_train,family=binomial)

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
modeloStepBIC_transf<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fullTodo), direction="both",k=log(nrow(data_train)))

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
modeloStepAIC_transf<-step(null, scope=list(lower=null, upper=fullT), direction="both")
modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fullTodo), direction="both")

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
modeloBackBIC_int<-step(full, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
modeloBackBIC_transf<-step(full, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
modeloBackBIC_todo<-step(full, scope=list(lower=null, upper=fullTodo), direction="both",k=log(nrow(data_train)))

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="both")
modeloBackAIC_int<-step(full, scope=list(lower=null, upper=fullInt), direction="both")
modeloBackAIC_transf<-step(full, scope=list(lower=null, upper=fullT), direction="both")
modeloBackAIC_todo<-step(full, scope=list(lower=null, upper=fullTodo), direction="both")

modelos<-list(modelo3,modeloStepBIC,modeloStepBIC_int,modeloStepBIC_transf,modeloStepBIC_todo,
              modeloStepAIC,modeloStepAIC_int,modeloStepAIC_transf,modeloStepAIC_todo,
              modeloBackBIC,modeloBackBIC_int,  modeloBackAIC)

sapply(modelos,function(x) pseudoR2(x,data_test,"varObjBin"))
sapply(modelos,function(x) x$rank) 
# El modelo 3 es el mas eficiente dado que tiene el menor numero de params.
# Si queremos mayor capacidad predictiva podemos irnos al modelo 6 que gana un 1% 
# a costa de 4 variables

#Evaluamos el primero con la curva ROC
(curvaRocTrain<-roc(data_train$varObjBin, predict(modeloInicial,data_train,type = "response"), direction="<"))
(curvaRocTest<-roc(data_test$varObjBin, predict(modeloInicial,data_test,type = "response"), direction="<"))
plot(curvaRocTrain)
plot(curvaRocTest,add=T,col=2) #es bastante bueno y estable

#Evaluamos todos tambi?n con la curva ROC
sapply(modelos,function(x) roc(data_test$varObjBin, predict(x,data_test,type = "response"), direction="<")$auc)
sapply(modelos,function(x) x$rank) 
#Parece que el mejor es modeloStepBIC_todo pero habr? que comprobarlo con validaci?n cruzada
#Validacion cruzada repetida para elegir entre todos con ROC
auxVarObj<-data_train$varObjBin
data_train$varObjBin<-make.names(data_train$varObjBin) #formateo la variable objetivo para que funcione el codigo
total<-c()
#omito el modeloStepBIC_int pues es igual que el modeloStepBIC
modelos<-list(modelo3,modeloStepBIC,modeloStepBIC_int,modeloStepBIC_transf,modeloStepBIC_todo,
              modeloStepAIC,modeloStepAIC_int,modeloStepAIC_transf,modeloStepAIC_todo,
              modeloBackBIC,modeloBackBIC_int,  modeloBackAIC)

formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo", ifelse(i<10,paste0("0",i),i)),
                                                                nrow(vcr$resample))))
}

#recupero la variable objetivo en su formato original
data_train$varObjBin<-auxVarObj

#Resultados de la validaci?n cruzada
par(mar=c(5.1,5.1 , 6, 2.1)) #ajusto el margen superior
boxplot(roc~modelo,data=total,main="?rea bajo la curva ROC") 
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
#el 2 y el 4 son peores tienen calidad similar a 1 y 3, resp, pero m?s par?metros
aggregate(roc~modelo, data = total, function(x) c(mean(x),sd(x))) #muy similares
#elegimos el modeloStepBIC_transf es casi igual que el mejor y tiene menos par?metros


## BUscamos el mejor punto de corte
#veo el reparto original.
freq(todo$varObjBin) #ese ha de ser el error de referencia

#Buscamos el que maximiza el ?ndice de youden
test_roc<-roc(data_test$varObjBin, predict(modeloStepBIC_todo,data_test,type = "response"), direction="<")
plot(test_roc,print.thres="best") #best es el punto de corte maximiza youden
#busco el que iguala sensibilidad y especificidad
test_roc$thresholds[which.min(abs(test_roc$sensitivities-test_roc$specificities))]

#Comparamos las 3 opciones
sensEspCorte(modeloStepBIC_todo,data_test,"varObjBin",0.5,"1") #max. tasa de acierto
sensEspCorte(modeloStepBIC_todo,data_test,"varObjBin",0.688,"1") #max. ?ndice de Youden
sensEspCorte(modeloStepBIC_todo,data_test,"varObjBin",0.8492,"1") #Igualar sens. y esp.
#con 0.688 no pierdo mucho en acuracy, pero consigo igualar sens y esp

# Vemos las variables m?s importantes del modelo ganador
importanciaVariablesLog(modeloStepBIC_todo) 

# Vemos los coeficientes del modelo ganador para interpretarlos
exp(coef(modeloStepBIC_todo))


# SEGUIR DESDE AQUI

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modeloStepBIC_todo,data_train,"varObjBin")
pseudoR2(modeloStepBIC_todo,data_test,"varObjBin")
roc(data_train$varObjBin, predict(modeloStepBIC_todo,data_train,type = "response"), direction="<")
roc(data_test$varObjBin, predict(modeloStepBIC_todo,data_test,type = "response"), direction="<")
sensEspCorte(modeloStepBIC_todo,data_train,"varObjBin",0.688,"1")
sensEspCorte(modeloStepBIC_todo,data_test,"varObjBin",0.688, "1")
#es un modelo bastante estable



