
setwd("c:/")

# **************************
# TUNEADO SVM BINARIA
# **************************

load ("saheartbis.Rda")
library(caret)
#  SVM LINEAL: SOLO PARÁMETRO C

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 


SVM<- train(data=saheartbis,factor(chd)~sbp+tobacco+ldl+age+
  typea+famhist.Absent,
  method="svmLinear",trControl=control,
 tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)


#  SVM Polinomial: PARÁMETROS C, degree, scale

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
 degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method = "cv",
 number=4,savePredictions = "all") 


SVM<- train(data=saheartbis,factor(chd)~sbp+tobacco+ldl+age+
  typea+famhist.Absent,
  method="svmPoly",trControl=control,
 tuneGrid=SVMgrid,verbose=FALSE)

SVM

SVM$results

# LOS GRÁFICOS DOS A DOS NO SIRVEN
# plot(SVM$results$C,SVM$results$Accuracy)
# plot(SVM$results$degree,SVM$results$Accuracy)
# plot(SVM$results$scale,SVM$results$Accuracy)

dat<-as.data.frame(SVM$results)
library(ggplot2)

# PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
ggplot(dat, aes(x=factor(C), y=Accuracy, 
 color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# SOLO DEGREE=2
dat2<-dat[dat$degree==2,]  
 
ggplot(dat2, aes(x=factor(C), y=Accuracy, 
 colour=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

#  SVM RBF: PARÁMETROS C, sigma

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30),
 sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))

control<-trainControl(method = "cv",
 number=4,savePredictions = "all") 


SVM<- train(data=saheartbis,factor(chd)~sbp+tobacco+ldl+age+
  typea+famhist.Absent,
  method="svmRadial",trControl=control,
 tuneGrid=SVMgrid,verbose=FALSE)

SVM

dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x=factor(C), y=Accuracy, 
 color=factor(sigma)))+ 
 geom_point(position=position_dodge(width=0.5),size=3)


load ("saheartbis.Rda")
source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")
source ("cruzada rf binaria.R")
source ("cruzada gbm binaria.R")
source ("cruzada xgboost binaria.R")
source ("cruzada SVM binaria lineal.R")
source ("cruzada SVM binaria polinomial.R")
source ("cruzada SVM binaria RBF.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("sbp", "tobacco", "ldl","age", "typea",
  "famhist.Absent"),
 listclass=c(""), grupos=4,sinicio=1234,repe=5)

 medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("sbp", "tobacco",
  "ldl","age", "typea","famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200,trace=FALSE)

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
  mtry=6,ntree=200,replace=TRUE,sampsize=150)

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
n.minobsinnode=5,shrinkage=0.001,n.trees=3000,interaction.depth=2)

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
  grupos=4,sinicio=1234,repe=5,C=0.05)

medias8$modelo="SVM"


medias9<-cruzadaSVMbinPoly(data=saheartbis, vardep="chd",
   listconti=c("sbp", "tobacco",
  "ldl","age", "typea","famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  C=0.01,degree=2,scale=2)

medias9$modelo="SVMPoly"


medias10<-cruzadaSVMbinRBF(data=saheartbis, vardep="chd",
   listconti=c("sbp", "tobacco",
  "ldl","age", "typea","famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  C=5,sigma=0.01)

medias10$modelo="SVMRBF"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,
 medias6,medias7,medias8,medias9,medias10)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC")

uni<-union1
uni$modelo <- with(uni,
 reorder(modelo,tasa, mean))
par(cex.axis=0.8,las=2)
boxplot(data=uni,tasa~modelo,col="pink")



# **************************
# TUNEADO SVM CONTINUA
# **************************

load ("compressbien.Rda")

#  SVM LINEAL: SOLO PARÁMETRO C

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,
 savePredictions = "all") 


SVM<- train(data=compressbien,cstrength~age+water+cement+blast,
  method="svmLinear",trControl=control,
 tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$RMSE)


#  SVM Polinomial: PARÁMETROS C, degree, scale

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
 degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method = "cv",
 number=4,savePredictions = "all") 


SVM<- train(data=compressbien,cstrength~age+water+cement+blast,
  method="svmPoly",trControl=control,
 tuneGrid=SVMgrid,verbose=FALSE)

SVM

SVM$results

# LOS GRÁFICOS DOS A DOS NO SIRVEN
# plot(SVM$results$C,SVM$results$RMSE)
# plot(SVM$results$degree,SVM$results$RMSE)
# plot(SVM$results$scale,SVM$results$RMSE)

dat<-as.data.frame(SVM$results)
library(ggplot2)

# PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
ggplot(dat, aes(x=factor(C), y=RMSE, 
 color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# AFINO MÁS
dat2<-dat[dat$degree==3&dat$scale<5&dat$scale>0.1,]  
 
ggplot(dat2, aes(x=factor(C), y=RMSE, 
 colour=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

#  SVM RBF: PARÁMETROS C, sigma

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30),
 sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))

control<-trainControl(method = "cv",
 number=4,savePredictions = "all") 


SVM<- train(data=compressbien,cstrength~age+water+cement+blast,
  method="svmRadial",trControl=control,
 tuneGrid=SVMgrid,verbose=FALSE)

SVM

dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x=factor(C), y=RMSE, 
 color=factor(sigma)))+ 
 geom_point(position=position_dodge(width=0.5),size=3)




source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
source("cruzada gbm continua.R")
source("cruzada xgboost continua.R")
source("cruzada SVM continua lineal.R")
source("cruzada SVM continua polinomial.R")
source("cruzada SVM continua RBF.R")
load("compressbien.Rda")

data<-compressbien

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)

medias1$modelo="avnnet"

medias2<-cruzadalin(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5)

medias2$modelo="lineal"

medias3<-cruzadaarbol(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
 grupos=4,sinicio=1234,repe=5,cp=0,minbucket=5)

medias3$modelo="arbol"

medias4<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=20,replace=TRUE,ntree=200,mtry=4)

medias4$modelo="bagging"


medias5<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=10,replace=TRUE,ntree=600,mtry=3)

medias5$modelo="rf"


medias6<-cruzadagbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
n.minobsinnode=20,shrinkage=0.10,n.trees=500,interaction.depth=2)

medias6$modelo="gbm"


medias7<-cruzadaxgbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=20,eta=0.1,nrounds=500,max_depth=2,
  gamma=0,colsample_bytree=1,subsample=1)

medias7$modelo="xgbm"

medias8<-cruzadaSVM(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,C=0.01)

medias8$modelo="SVM"

medias9<-cruzadaSVMpoly(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,C=0.05,degree=3,scale=0.5)

medias9$modelo="SVMPoly"


medias10<-cruzadaSVMRBF(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,C=30,sigma=0.2)

medias10$modelo="SVMRBF"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,
 medias7,medias8,medias9,medias10)

par(cex.axis=0.8)
boxplot(data=union1,error~modelo,col="pink")


union1<-rbind(medias1,medias3,medias4,medias5,medias6,medias7,
medias9,medias10)

par(cex.axis=0.8)
boxplot(data=union1,error~modelo)

uni<-union1
uni$modelo <- with(uni,
 reorder(modelo,error, mean))

par(cex.axis=0.8,las=2)
boxplot(data=uni,error~modelo,col="pink")


