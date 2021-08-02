
library(FuncMining)
library(CruzadasBin)
library(CruzadasCont)
library(e1071)
library(caret)
library(MASS)
library(dummies)
library(nnet)
library(NeuralNetTools)
library(ggplot2)
library(plotly)
library(dfexplore)
library(reshape)
library(pROC)
library(reshape2)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(gbm)
library(xgboost)
library(caretEnsemble)


varObjBin <- trainbis$stat


# XGBOOST
library(xgboost)

status_group = trainbis$status_group
label = as.integer(trainbis$status_group)-1
trainbis$status_group = NULL


n = nrow(trainbis)
train.index = sample(n,floor(0.9*n))
train.data = as.matrix(trainbis[train.index,])
train.label = label[train.index]
test.data = as.matrix(trainbis[-train.index,])
test.label = label[-train.index]

xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

num_class = length(levels(status_group))

params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=100,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit


test_final.data  = as.matrix(testbis)




xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(status_group)

xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(status_group)[test.label+1]

result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))



# Cruzada de arboles

library(caret)

set.seed(12345)

gbmgrid <- expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                       n.minobsinnode=c(5,10,20),
                       n.trees=c(100,500,1000,5000),
                       interaction.depth=c(2))


control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 


gbm <- train(make.names(factor(status_group))~.,data=trainbis,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

plot(gbm)
# coger de estas graficas los puntos ganadores y hacer el modelo
# de cada uno de ellos, para sacar el diagrama de cajas y bigotes



# 1. Regresión logística --------------------------------------------------

modeloInicial<-glm(status_group.functional~.,data=trainbis,family=binomial)
summary(modeloInicial)
pseudoR2(modeloInicial,data_train,"varObjBin")
pseudoR2(modeloInicial,data_test,"varObjBin")
modeloInicial$rank #n?mero de par?metros
par(mar=c(9, 12, 4.1, 2.1))#ajustar el segundo valor si no cabe el nombre de las variables
importanciaVariablesLog(modeloInicial) 


# 2. Regresión logística automática ---------------------------------------


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


# 3. Cruzadas -------------------------------------------------------------


medias1<-cruzadalogistica(data=saheartbis,
                          vardep="chd",listconti=c("sbp", "tobacco", "ldl","age", "typea",
                                                   "famhist.Absent"),
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)

medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=saheartbis,
                          vardep="chd",listconti=c("sbp", "tobacco",
                                                   "ldl","age", "typea","famhist.Absent"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=5,
                          size=c(5),decay=c(0.1),repeticiones=5,itera=200)

medias2$modelo="avnnet"


medias3<-cruzadaarbolbin(data=saheartbis,
                         vardep="chd",listconti=c("sbp", "tobacco",
                                                  "ldl","age", "typea","famhist.Absent"),
                         listclass=c(""),grupos=4,sinicio=1234,repe=5,
                         cp=c(0),minbucket =5)

medias3$modelo="arbol"


medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
                      listconti=c("sbp", "tobacco",
                                  "ldl","age", "typea","famhist.Absent"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,nodesize=10,
                      mtry=6,ntree=200,replace=TRUE)

medias4$modelo="bagging"

medias5<-cruzadarfbin(data=saheartbis, vardep="chd",
                      listconti=c("sbp", "tobacco",
                                  "ldl","age", "typea","famhist.Absent"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,nodesize=10,
                      mtry=3,ntree=200,replace=TRUE)

medias5$modelo="rf"


medias6<-cruzadagbmbin(data=saheartbis, vardep="chd",
                       listconti=c("sbp", "tobacco",
                                   "ldl","age", "typea","famhist.Absent"),
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,
                       n.minobsinnode=10,shrinkage=0.001,n.trees=5000,interaction.depth=2)

medias6$modelo="gbm"

medias7<-cruzadaxgbmbin(data=saheartbis, vardep="chd",
                        listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        min_child_weight=10,eta=0.08,nrounds=100,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0,lambda_bias=0)


medias7$modelo="xgbm"

medias8<-cruzadaSVMbin(data=saheartbis, vardep="chd",
                       listconti=c("sbp", "tobacco",
                                   "ldl","age", "typea","famhist.Absent"),
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,
                       C=0.05)

medias8$modelo="SVM"


medias9<-cruzadaSVMbinPoly(data=saheartbis, vardep="chd",
                           listconti=c("sbp", "tobacco",
                                       "ldl","age", "typea","famhist.Absent"),
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=5,
                           C=0.5,degree=2,scale=0.1)

medias9$modelo="SVMPoly"


medias10<-cruzadaSVMbinRBF(data=saheartbis, vardep="chd",
                           listconti=c("sbp", "tobacco",
                                       "ldl","age", "typea","famhist.Absent"),
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=5,
                           C=1,sigma=0.1)

medias10$modelo="SVMRBF"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,
              medias6,medias7,medias8,medias9,medias10)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")



