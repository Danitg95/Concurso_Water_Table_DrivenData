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

#------------------------------------------------------------
# Parto de los datos sin at?picos ni ausentes y sin las variables 'inutiles'
datosDep<-readRDS("datosElecciones2")
varObjCont<-datosDep$varObjCont
varObjBin<-datosDep$varObjBin
input<-datosDep[,-c(1:2)]

#Obtengo las mejores transformaciones
TransfCont<-Transf_Auto(Filter(is.numeric, input[,-c(31,32)]),varObjCont)#indico que no quiero transformar las aleatorias (estan en las columnas 14 y 15)
names(TransfCont)

#Pruebo la discretizaci?n de las variables cuantitativas
discCont<-droplevels(optbin(data.frame(Filter(is.numeric, input[,-c(31,32)]),bin(varObjCont,nbins=5,method = "content"))))[,-(ncol(Filter(is.numeric, input[,-c(14,15)]))+1)]
names(discCont)<-paste("disc", names(discCont), sep = "_")

#Verificamos el reparto de las nuevas categor?as
apply(discCont,2,freq) #ServiciosPtge, ConstruccionPtge, AgricultureUnemploymentPtge, IndustriaPtge tienen categor?as infrarrepresentadas
# Como la imputaci?n ha sido aleatoria estos resultados pueden variar de una ejecuci?n a otra

aggregate(varObjCont, by=list(discCont$disc_ServiciosPtge), mean) # unimos las dos entre s?
discCont$disc_ServiciosPtge<-car::recode(discCont$disc_ServiciosPtge, "c('(-0.0717,3.33]','(3.33,7.92]', '(7.92,12.4]')='(-0.0717,12.4]'")

aggregate(varObjCont, by=list(discCont$disc_ConstruccionPtge), mean) # unimos las dos entre s?
discCont$disc_ConstruccionPtge<-car::recode(discCont$disc_ConstruccionPtge, "c('(2.28,5.24]','(5.24,6.58]', '(6.58,9.96]')='(2.28,9.96]'")

aggregate(varObjCont, by=list(discCont$disc_AgricultureUnemploymentPtge), mean) # La junto con la primera
discCont$disc_AgricultureUnemploymentPtge<-car::recode(discCont$disc_AgricultureUnemploymentPtge, "c('(6.79,7.5]','(7.5,7.62]')='(6.79,7.62]'")

aggregate(varObjCont, by=list(discCont$disc_IndustriaPtge), mean) # La junto con la ?ltima
discCont$disc_IndustriaPtge<-car::recode(discCont$disc_IndustriaPtge, "c('(1.38,3.16]','(3.16,3.66]', '(3.66,5.16]')='(1.38,5.16]'")

apply(discCont,2,freq) #todo bien ya

#Junto las originales, transformaciones y discretizadas
datos_todocont<-data.frame(varObjCont,input,TransfCont,discCont)

#Hago la partici?n
set.seed(12345)
trainIndex <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train <- datos_todocont[trainIndex,]
data_test <- datos_todocont[-trainIndex,]


# Una vez finalizado este proceso, se puede considerar que los datos est?n depurados. Los guardamos
#saveRDS(data.frame(varObjBin,varObjCont,input),"datosElecciones3")

#Para evitar problemas con las semillas
RNGkind(sample.kind = "Rounding")



#-------------------------------------------
# DetecciÃ³n de las relaciones entre las variables input y objetivo
#datos<-readRDS("datosElecciones3")
#varObjBin <- as.factor(datos$varObjBin)
#varObjCont<-datos$varObjCont
#input<-as.data.frame(datos[,-(1:2)])



# DIA3 --------------------------------------------------------------------

# Este fue el modelo ganador el d?a 2
modeloManual<-lm(varObjCont~CCAA+ServiciosPtge,data=data_train)
summary(modeloManual)
Rsq(modeloManual,"varObjCont",data_train)
Rsq(modeloManual,"varObjCont",data_test)
modeloManual$rank

# Seleccion de variables "cl?sica" (variables originales)
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:16)]) #Modelo maximo, seleccionamos las columnas de las variables originales
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
Rsq(modeloStepAIC,"varObjCont",data_test)
modeloStepAIC$rank

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC,"varObjCont",data_test) 
modeloStepBIC$rank #Un pelin mejor que el anterior y un par?metro menos


modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank #son iguales

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
Rsq(modeloBackBIC,"varObjCont",data_test)
modeloBackBIC$rank # son iguales

#Genero interacciones s?lo con las originales
fullInt<-lm(varObjCont~.^2, data=data_train[,c(1:16)])

#El backward puede ser muy lento (o incluso no funcionar) cuando hay muchos posibles efectos
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
Rsq(modeloStepAIC_int,"varObjCont",data_test) #Es algo mejor que los anteriores
modeloStepAIC_int$rank #muchos par?metros

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC_int,"varObjCont",data_test) #Un pelin mejor que el AIC
modeloStepBIC_int$rank #menos par?metros

#De los de interacciones, es preferible el modeloStepBIC_int. Mayor R2 y menos par?metros

# Pruebo con las transf y las originales
fullT<-lm(varObjCont~., data=data_train[,c(1:27)])

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
Rsq(modeloStepAIC_trans,"varObjCont",data_test) #aumenta significativamente el R2 con respecto a sin transf
modeloStepAIC_trans$rank 

modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC_trans,"varObjCont",data_test) #aumenta significativamente el R2 con respecto a sin transf
modeloStepBIC_trans$rank

#Por parsimonia, puede ser preferible el BIC

# Pruebo con las transf, las originales y las discretizadas
fulltodo<-lm(varObjCont~., data=data_train[,-c(31)])

modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both")
Rsq(modeloStepAIC_todo,"varObjCont",data_test)
modeloStepAIC_todo$rank

modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC_todo,"varObjCont",data_test) 
modeloStepBIC_todo$rank #7 par?metros menos

#Por parsimonia, puede ser preferible el BIC

#Pruebo con todas y las interacciones, pueden resultar modelos sobreajustados, sobretodo con AIC
#pruebo solo BIC
fullIntT<-lm(varObjCont~.^2, data=data_train)

modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC_todoInt,"varObjCont",data_test)
modeloStepBIC_todoInt$rank

#Comparar con R2 en test
#incluir en la lista siguiente todos los modelos que se quieran probar
modelos<-list(modeloManual,modeloStepAIC,modeloStepBIC,modeloBackAIC, modeloBackBIC, modeloStepAIC_int,modeloStepBIC_int,modeloStepAIC_trans,modeloStepBIC_trans,modeloStepAIC_todo,modeloStepBIC_todo,modeloStepBIC_todoInt)
sapply(modelos,function(x) x$rank)
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
#modeloStepBIC_todo parece tener buen balance entre capacidad predictiva y complejidad


## Pruebo los modelos con validacion cruzada repetida
total<-c()
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo", ifelse(i<10,paste0("0",i),i)),nrow(vcr$resample))))
}

par(mar=c(5.1,5.1,6,2.1)) #ajusto el margen superior
boxplot(Rsquared~modelo,data=total,main="R-Square")
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) x$rank), cex.axis=1)
#Se descarta el 4 por el n?mero de par?metros
aggregate(Rsquared~modelo, data = total, function(x) c(mean(x),sd(x)))
#el 9 tiene poca variabilidad y buen R2

#Si es m?s importante la predicci?n que la interpretaci?n, ganar?a el 10.
#Si es al rev?s, perder un 3% no es excesivo y se simplifica mucho el modelo eligiendo el 9 (incluso el 7 si se quiere simplificar a?n m?s).

formula(modeloStepBIC_todoInt)
#Una vez decidido el mejor modelo, hay que evaluarlo (ver final del C?digoCompletoEjemploDia2)
#Estudiamos su bondad, su estabilidad, las variables que lo componen
#Interpretamos los par?metros, etc.

