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

trainOri <- trainOri[,-c(20)]
testOri <- testOri[,-c(20)]
#------- END ------- Data Loading

# EDA (with inspectdf) ----------------------------------------------------


# # ------- INI ------- EDA (with inspectdf)
# # Horizontal bar plot for categorical column composition
 # x <- inspect_cat(trainOri) 
 # show_plot(x)
 
 #-- Niveles de las categoricas.
     # recorded_by: A tirar. Es cte!.
     # subvillaje, ward, wpt_names : Muchos niveles distintos
     # Cuidadín con: funder, installer, scheme_name
 #-- Parecidas:
     # payment - payment_type
     # quantity - quantity_group
     # source - source_type

# # Correlation betwee numeric columns + confidence intervals
 # x <- inspect_cor(trainOri)
 # show_plot(x)
 #-- Correlación: 
     # district_code/region_code ~0.6
     # construction_year/gps_height ~0.6
     # latitude / longitude ~ -0.35


# # Bar plot of most frequent category for each categorical column
 # x <- inspect_imb(trainOri)
 # show_plot(x)
 
# # Bar plot showing memory usage for each column
 # x <- inspect_mem(trainOri)
 # show_plot(x)
# 
# # Occurence of NAs in each column ranked in descending order
 # x <- inspect_na(trainOri)
 # show_plot(x)
 #-- NAs:
     # public_meeting: 5.6%
     # permit: 5.1%
     # resto de vars: limpias.
 
# # Histograms for numeric columns
 # x <- inspect_num(trainOri)
 # show_plot(x)
 #-- Hist de numericas.
 # amount_tsh: outliers (pocos)
 # construction_year: hay muchos ceros (NAs enmascarados)
 # district_code: ¿categórico?
 # gps_height: valores negativos y de ceros?.
 # latitude/longitud: ¿ceros?.
 # num_private: ¿ceros?
 # population: outliers / ceros?
 # region_code: ¿categorica?.
 

# # Barplot of column types
 # x <- inspect_types(trainOri)
 # show_plot(x)
 # 
#------- END ------- EDA (with inspectdf)


 
 
 

 

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


 #dfplot(trainOri)
 
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
 # en nuestro ejemplo hay 82 variables, eliminaremos observaciones con más de 
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
 fullcartest  <- intersect (listamisimptest , test_carac)
 
 
 Mode <- function(x){
   a = table(x) # x is a vector
   return(names(a[which.max(a)]))
 }
 
 train4 <- as.data.frame(sapply(train2[,fullcartrain],function(x) ifelse(is.na(x),Mode(x),x)))
 test4  <- as.data.frame(sapply(test2 [,fullcartest],function(x) ifelse(is.na(x),Mode(x),x)))
 
 # 8) UNIR a) imputadas continuas b) imputadas caracter c) columnas complementarias
 
 #train_fin <- as.data.frame(cbind(train3,train4,train2[,listacompletrain]))
 #test_fin  <- as.data.frame(cbind(test3, test4, test2[,listacompletest]))
 train_fin <- as.data.frame(cbind( train2[,listacompletrain]))
 test_fin  <- as.data.frame(cbind( test2[,listacompletest]))
 
 rm(list=setdiff(ls(), c("train_fin", "test_fin")))

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
 full <- full[,-c(15,18,13)]
 full_carac <- names(Filter(is.character, full))
 
 fullbis <- dummy.data.frame(full, full_carac, sep = ".")
 
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
 
 
 # Lanzamiento del modelo
 
 tic()
 mymodel <- ranger(status_group~., 
     data = trainbis,
     num.trees = 5000,
     importance = 'impurity',
     verbose = TRUE
 )
 toc()
 
 acierto_val <- 1 - mymodel$prediction.error
 acierto_val
 
# Transformaciones --------------------------------------------------------
 train_fin$status_
 #Obtenemos todas las transformaciones
 Transfbin<-Transf_Auto(Filter(is.numeric, train_fin), train_fin$status_group)#indico que no quiero transformar las aleatorias (estan en las columnas 13 y 14)
 names(Transfbin)
 

 
# Discretización ----------------------------------------------------------

 
 #Pruebo la discretizaci?n de las variables cuantitativas
 discbin<-droplevels(optbin(data.frame(Filter(is.numeric, input[,-c(14,15)]),varObjBin)))[,-(ncol(Filter(is.numeric, input[,-c(14,15)]))+1)]
 names(discbin)<-paste("disc", names(discbin), sep = "_")
 
 #Verificamos el reparto de las nuevas categor?as
 apply(discCont,2,freq) #ServiciosPtge, ConstruccionPtge, AgricultureUnemploymentPtge, IndustriaPtge tienen categor?as infrarrepresentadas
 # Como la imputaci?n ha sido aleatoria estos resultados pueden variar de una ejecuci?n a otra
 
 discCont$disc_ServiciosPtge<-car::recode(discCont$disc_ServiciosPtge, "c('(-0.0717,3.33]','(3.33,7.92]', '(7.92,12.4]')='(-0.0717,12.4]'")
 
 datos_todobin<-data.frame(varObjBin,input,Transfbin,discbin)