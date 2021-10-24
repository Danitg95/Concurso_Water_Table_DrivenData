#------------------
# Autor: Daniel Toral
# Date: 01.08.2021
# Inputs: Training, test, labels from Drivendata.org (Pump). 
# Output: Charts, submission.
# Changes: To include categorical variables.
#------------------

# Librerias ---------------------------------------------------------------

    library(data.table)    # Fast Data processing.
    library(tictoc)        # To measure computing time.
    library(inspectdf)     # Automatic EDA.
    library(DataExplorer)  # Automatic EDA.
    library(dplyr)         # Data processing with pipes.
    library(ranger)        # Fast RandomForest.
    library(magrittr)      # Piping.
    library(ggplot2)       # The most beautiful charts in the ML world.
    library(forcats)       # Treat categorical variables
    library(missRanger)    # NA's imputation with ranger.
    library(FuncMining)    # Several data mining functions
    library(dfexplore)     # Explore data-frames
    library(dummies)       # Create dummy variables


 
# Data --------------------------------------------------------------------

#------- INI ------- Data Loading
trainvarOri    <- as.data.frame(fread("./data/train.csv"))
testOri     <- fread("./data/test.csv", data.table = FALSE)
trainlabOri <- fread("./data/train_labels.csv", data.table = FALSE)

trainOri <- merge(trainvarOri, trainlabOri, by.x = c('id'), by.y = c('id'))
rm(trainvarOri)
rm(trainlabOri)

# Quito el recorded_by por ser constante
trainOri <- trainOri[,-c(20)]
testOri <- testOri[,-c(20)]
#------- END ------- Data Loading


# Tratamiento de Missing --------------------------------------------------

 # Guardar numericas, caracter, factores en diferentes listados 
 # para copiar y pegar
 
 train_num        <- names(Filter(is.numeric, trainOri))
 train_carac      <- names(Filter(is.character, trainOri))
 train_factor     <- names(Filter(is.factor, trainOri))
 
 
 test_num         <- names(Filter(is.numeric, testOri))
 test_carac       <- names(Filter(is.character, testOri))
 test_factor      <- names(Filter(is.factor, testOri))
 
 dput(train_num)
 dput(train_carac)
 dput(train_factor)
 
 # Observar gráficamente missing y estructura
 
 trainmis1                <-as.data.frame(sapply(trainOri[,train_num],function(x) sum(is.na(x))))
 names(trainmis1)[1]      <-"nmiss"
 
 testmis1                 <-as.data.frame(sapply(testOri[,test_num],function(x) sum(is.na(x))))
 names(testmis1)[1]       <-"nmiss"
 
 # 2)OBSERVAR LOS MISSING EN VARIABLES DE CARACTER O FACTORES

 trainmis2           <-as.data.frame(sapply(trainOri[,train_carac],function(x) sum(is.na(x))))
 names(trainmis2)[1] <-"nmiss"
 
 testmis2            <-as.data.frame(sapply(testOri[,test_carac],function(x) sum(is.na(x))))
 names(testmis2)[1]  <-"nmiss"
 
 # 3) CREAR LISTA DE VARIABLES CON MÁS DE K MISSING PARA ELIMINAR
 
 # a) uno las tablas  tablamis1 y 2
 
 tablatotaltrain <-as.data.frame(rbind(trainmis1,trainmis2))
 tablatotaltest  <- as.data.frame(rbind(testmis1, testmis2))
 
 # b) lista variables con más de k=300 missing
 # Primero guardo los nombres
 
 tablatotaltrain$nombrevar<-row.names(tablatotaltrain)
 tablatotaltest $nombrevar<-row.names(tablatotaltest)
 
 listamistrain     <- tablatotaltrain[which(tablatotaltrain$nmiss > 300),]
 listamistest      <- tablatotaltest [which(tablatotaltest$nmiss  > 300),]
 
 listaborrartrain  <- dput(listamistrain$nombrevar)
 listaborrartest   <- dput(listamistest$nombrevar)
 
 # 4) ELIMINAR VARIABLES DE LA LISTA CON MUCHOS MISSING 
 
 train2  <- trainOri[, !(colnames(trainOri) %in% listaborrartrain)]
 test2   <- testOri [, !(colnames(testOri) %in% listaborrartest)]


 # *******************************************************************
 # ANTES DE IMPUTAR, BUSCAMOS OBSERVACIONES CON MÁS DE K missing, sabiendo 
 # en nuestro ejemplo hay  variables, eliminaremos observaciones con más de 
 # 10 missing (esto es arbitrario)
 
 train2$contarmis<- apply(train2, 1, function(x) sum(is.na(x)))
 train2          <-train2[train2$contarmis<=10,]
 
 test2$contarmis <- apply(test2, 1, function(x) sum(is.na(x)))
 test2           <-test2[test2$contarmis<=10,]
 
 
 # *******************************************************************
 
 # 5) GUARDAR VARIABLES NUMÉRICAS CON ALGÚN MISSING PARA IMPUTAR
 
 listatrain       <- tablatotaltrain[which(tablatotaltrain$nmiss<=300&tablatotaltrain$nmiss>0),]
 listamisimptrain <- dput(listatrain$nombrevar)
 
 completrain      <- tablatotaltrain[which((tablatotaltrain$nmiss==0)),]
 listacompletrain <- dput(completrain$nombrevar)
 
 listatest        <- tablatotaltest[which(tablatotaltest$nmiss<=300&tablatotaltest$nmiss>0),]
 listamisimptest  <- dput(listatest$nombrevar)
 
 completest       <- tablatotaltest[which((tablatotaltest$nmiss==0)),]
 listacompletest  <- dput(completest$nombrevar)
 
 
 # 6) IMPUTAR POR LA MEDIANA O MEDIA EN CONTINUAS

 fullnumtrain <- intersect(listamisimptrain, train_num)
 fullnumtest  <- intersect(listamisimptest, test_num)
 
 train3 <- as.data.frame(sapply(train2[,fullnumtrain],function(x) ifelse(is.na(x),median(x,na.rm=TRUE),x)))
 test3  <- as.data.frame(sapply(test2[,fullnumtest],function(x) ifelse(is.na(x),median(x,na.rm=TRUE),x)))
 
 # 7) IMPUTAR POR LA MODA EN CARACTER
 
 fullcartrain <- intersect(listamisimptrain, train_carac)
 fullcartest  <- intersect(listamisimptest , test_carac)
 
 
 Mode <- function(x){
   a = table(x) # x is a vector
   return(names(a[which.max(a)]))
 }
 
 train4 <- as.data.frame(sapply(train2[,fullcartrain],function(x) ifelse(is.na(x),Mode(x),x)))
 test4  <- as.data.frame(sapply(test2 [,fullcartest],function(x) ifelse(is.na(x),Mode(x),x)))
 
 # 8) UNIR a) imputadas continuas b) imputadas caracter c) columnas complementarias
 
 train_fin <- as.data.frame(cbind( train2[,listacompletrain]))
 test_fin  <- as.data.frame(cbind( test2[,listacompletest]))
 
 rm(list=setdiff(ls(), c("train_fin", "test_fin", "testOri")))

# Tratamiento variables categóricas (dummies) ---------------------------------------

 # Buscar variables numéricas con k o menos 
 # valores diferentes, para ver si pueden ser categóricas
 
 # (Igualmente habría que investigar en el listado de numéricas 
 # si se ha colado alguna categórica)
 
 train_fin_num     <- names(Filter(is.numeric, train_fin))
 train_fin_carac   <- names(Filter(is.character, train_fin))
 train_fin_factores<- names(Filter(is.factor, train_fin))
 
 test_fin_num     <- names(Filter(is.numeric, test_fin))
 test_fin_carac   <- names(Filter(is.character, test_fin))
 test_fin_factores<- names(Filter(is.factor, test_fin))
 
 
 train_lista <- names(train_fin[, sapply(train_fin[,train_fin_num], 
                                    function(col) length(unique(col))) < 4])
 test_lista <- names(test_fin[, sapply(test_fin[,test_fin_num], 
                                         function(col) length(unique(col))) < 4])
 
 # Lista de Frecuencias de las categóricas, útil pues algunos niveles
 # con pocas observaciones no deben ser tenidos en cuenta 
 # en modelos de machine learning para evitar sobreajuste
 

 
 # Obtener dummies (en el ejemplo solo con caracter, 
 # hay que tener en cuenta que muchas variables numéricas
 # pueden ser categóricas). Las dummies sustituyen a las variables originales,
 # con lo que es mejor crear un archivo nuevo por si queremos utilizar las 
 # originales en algún momento

 
 test_fin$status_group <- "BORRAR"
 
 
 full <- rbind(test_fin, train_fin)
 
 full_carac <- names(Filter(is.character, full[,-c(15,18,13,16,11,12,17,20,21,22)]))
 
 fullbis <- dummy.data.frame(full, full_carac, sep = ".")
 
 fullbis <- cbind(fullbis, full[, c(15,18,13,11,16,12,17,20,21,22)])

 # Para borrar las dummies con menos de k observaciones se utiliza el 
 # listado de frecuencias frecu obtenido anteriormente
 
 library(plyr)
 full_frecu<-ldply(full[,full_carac],function(x) t(rbind(names(table(x)),table(x))))
 names(full_frecu)<-c("variable","nivel","frecuencia")
 full_frecu$frecuencia<-as.numeric(full_frecu$frecuencia)
 
 # 1) Obtengo filas de frecu con frecuencia menor que k=20 como ejemplo
 
 full_frecu20<-full_frecu[full_frecu$frecuencia<20,]
 
 # 2) Obtengo listado de los niveles en el mismo formato que las dummies,
 # con separador .
 full_frecu20$dum<-paste(full_frecu20$variable,full_frecu20$nivel,sep=".")
 full_listamal<-dput(full_frecu20$dum)
 
 # 3) Borro las dummies de amesbis que coinciden con la listamal
 fullbis[,full_listamal]<-NULL
 
 testbis         <- fullbis[which(fullbis$status_group.BORRAR == 1),]
 trainbis        <- fullbis[which(fullbis$status_group.BORRAR == 0),]
 
 
 trainlabOri <- fread("./data/train_labels.csv", data.table = FALSE)
 trainbis <- merge(trainlabOri, trainbis, by.x = c('id'), by.y = c('id'))
 
 testbis$status_group.functional                 <- NULL
 testbis$`status_group.functional needs repair`  <- NULL
 testbis$`status_group.non functional`           <- NULL
 testbis$status_group.BORRAR                     <- NULL
 
 trainbis$status_group.functional                <- NULL
 trainbis$`status_group.functional needs repair` <- NULL
 trainbis$`status_group.non functional`          <- NULL
 trainbis$status_group.BORRAR                    <- NULL

 rm(list=setdiff(ls(), c("testbis", "trainbis")))
 
 trainbis$status_group <- as.factor(trainbis$status_group)
 names(trainbis)       <-make.names(names(trainbis), unique=TRUE)
 names(testbis)       <-make.names(names(testbis), unique=TRUE)
 
 # Lanzamiento del modelo
 
 tic()
 
 #-- Grid Search
 mynumtre <- c(500,1000)
 mymtry <- c(3)
 mymnsi <- c(1)
 myseed <- c(1234, 8765)
 migrid <- expand.grid(numtre = mynumtre, mitry = mymtry, mymnsi = mymnsi, seed =  myseed)
 migrid$mierr <- 0
 
 tic()
 for (i in 1:nrow(migrid)) {
    set.seed(migrid$myseed[i])
     mymodel <- ranger( 
         
         num.trees     = migrid$numtre[i],
         mtry          = migrid$mimtry[i],
         min.node.size =  migrid$mymnsi[i],
         
         status_group ~ . , 
         data          = trainbis,
         importance    = 'impurity',
         verbose       = TRUE
     )
     
     acierto_val <- 1 - mymodel$prediction.error
     # acierto_val
     
     migrid$mierr[i] <- acierto_val
     print(migrid[i,])
     
 } 
 toc()
 
 #-- Best iteration
 best_iter <- migrid %>%
     filter( mierr == max(mierr))
 
 #   numtre mitry mymnsi     mierr
 # 1    600     5      2 0.8127778
 
 #-- Run model for the best iteration.
 #
 set.seed(best_iter$myseed[1])
 mymodel <- ranger( 
     status_group ~ . , 
     data          = trainbis,
     num.trees     = best_iter$numtre[1],
     mtry          = best_iter$mimtry[1],
     min.node.size = best_iter$mymnsi[1],
     importance    = 'impurity',
     verbose       = TRUE
 )
 
 acierto_val <- 1 - mymodel$prediction.error
acierto_val

#------ END ------ MODELING - RANDOMFOREST - ranger

#----- INI --------- PREDICTION + SUBMISSION
#

tic()
mypred <- predict( mymodel, testbis)$predictions
toc()

mysub <- data.frame(
    id = testbis$id,
    status_group = mypred
)

var_impor <- as.data.frame(mymodel$variable.importance)
names(var_impor) <- c('importancia') 
var_impor$variables <- rownames(var_impor)
rownames(var_impor) <- NULL

fwrite(
    mysub, 
    paste("./submissions/07_final_dia_de_clase_", 
          round(acierto_val,4),
          "_numvars_",  nrow(var_impor), 
          "_.csv", sep =  "")
)


#trees 1000 - min.n.s = 3 - 10 mitry - Local: 0.81678 - Plataforma: 0.8001
