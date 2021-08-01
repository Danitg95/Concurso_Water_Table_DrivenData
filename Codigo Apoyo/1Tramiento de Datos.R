
suppressPackageStartupMessages(
  {
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
    library(dplyr)
    library(tidyr)
    library(OneR)
    library(pROC)
    library(FuncMining)
  } 
)

datos <- read_excel("Evaluacion_DatosEleccionesEuropeas2019.xlsx")

datos <- as.data.frame(rbind(testOri, trainOrifull[,-c(41)]))


str(datos)

summary(datos)


# Cuento el n?mero de valores diferentes para las num?ricas
possible_factor <- as.data.frame(sapply(Filter(is.numeric, datos),function(x) length(unique(x))))
names(possible_factor) <- c('val')
possible_factor$var <- rownames(possible_factor)
rownames(possible_factor) <- NULL

possible_factor[which(possible_factor$val < 10),]$var





 # Tras analizar el n?mero de valores distintos en las variables cuantitativas,
# no aparece ninguna con menos de 10 valores, por lo que no es necesario 
# convertir ninguna variable a factor.

summary(datos)


# Al analizar las variables porcentaje (0-100), encuentro que las variables 
# "UnemploymentPtge" y "AutonomosPtge" tienen valores m?ximos por encima de 100%.
# Corregimos dichos valores a NA
datos$UnemploymentPtge <-replace(datos$UnemploymentPtge, which((datos$UnemploymentPtge < 0)|(datos$UnemploymentPtge>100)), NA)
datos$AutonomosPtge <-replace(datos$AutonomosPtge, which((datos$AutonomosPtge < 0)|(datos$AutonomosPtge>100)), NA)
summary(datos$UnemploymentPtge)
summary(datos$AutonomosPtge)


# Es raro densidad, se estudia -> Emperador
# Es raro totalempresas -> ok, madrid
# Es raro iinmueblees -> Madrid

# Ver el reparto de frecuencias de las categor?as de las variables cualitativas
freq(datos$CCAA) #Hay categor?as con poca represnetaci?n
freq(datos$PartidoCCAA) 


#Variables cualitativas con categor?as poco representadas (la opci?n else permite agrupar "todo lo dem?s")
datos$CCAA  <-  car::recode(datos$CCAA, "'Ceuta'='Ultramar'; 'Melilla'='Ultramar'; 'Canarias'= 'Ultramar'; 'Baleares' = 'Ultramar'")


aggregate(VarObjCont~CCAA, data = datos, mean)
boxplot_cuantcuali(datos$VarObjCont, datos$CCAA,"Abstenciones por CCAA")
boxplot_cuantcuali(datos$VarObjCont, datos$PartidoCCAA,"Abstenciones")


#Indico la variableObj, el ID y las Input (los at?picos y los missings se gestionan s?lo de las input)
varObjCont<-datos$VarObjCont
varObjBin<-datos$VarObjBin
input<-as.data.frame(datos[,-(1:3)])
row.names(input)<-datos$CodigoINE

##At?picos
for (i in names(which(sapply(input, class)=="numeric"))){
  outliers(paste0("input$",i))
}


input$PartidoCCAA <- as.factor(input$PartidoCCAA)
saveRDS(data.frame(varObjBin,varObjCont,input),"datosElecciones0")

datos<-readRDS("datosElecciones0")
varObjCont<-datos$varObjCont
varObjBin<-datos$varObjBin
input<-as.data.frame(datos[,-(1:2)])


## MISSINGS
#Proporci?n de missings por observaci?n
input$prop_missings<-rowMeans(is.na(input))
summary(input$prop_missings) #Ninguna observacion supera el 50% de ausentes

## Dado que hemos incluido una nueva variable input, debemos verificar que cumple las mismas condiciones que las input originales
length(unique(input$prop_missings)) #solo toma 8 valores distintos, la ponemos como factor
input$prop_missings<-as.factor(input$prop_missings) 

#verificamos el reparto
freq(input$prop_missings) #hay dos niveles con poco %, reagrupo y cambio nombres

input$prop_missings<-car::recode(input$prop_missings, "'0' = '0%'; '0.0344827586206897' = '3,4%'; c(0.0689655172413793, 0.103448275862069 , 0.137931034482759, 0.206896551724138, 0.241379310344828, 0.344827586206897)='> 6,8%'")
freq(input$prop_missings)

#Proporci?n de missings por variable
(prop_missingsVars<-colMeans(is.na(input))) #Ninguna variable supera el 50% de ausentes, 

# Ninguna variable supera el 5% de missings, aun asi dejo M_SUperficie para controlarla.
#Antes de imputar, creo variables auxiliares sobre falta de respuesta
#######  LA QUITO PORQUE SOLO TIENE UN NIVEL
#input$M_Superficie <- as.factor(is.na(input$Superficie))


summary(input)
## Imputaciones
# Imputo todas las cuantitativas, seleccionar el tipo de imputaci?n: media (mean, sin comillas), mediana (nada) o aleatorio ("random")
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) impute(x,"random"))
# Imputo todas las cualitativas, seleccionar el tipo de imputaci?n: moda  (nada) o aleatorio ("random")
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) impute(x,"random"))
# Se cambia el tipo de factor a character al imputar, as? que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , as.factor)

# Reviso que no queden datos missings
summary(input)

# Una vez finalizado este proceso, se puede considerar que los datos est?n depurados. Los guardamos
saveRDS(data.frame(varObjBin,varObjCont,input),"datosElecciones")

#Para evitar problemas con las semillas
RNGkind(sample.kind = "Rounding")



#-------------------------------------------
# DetecciÃ³n de las relaciones entre las variables input y objetivo
datos<-readRDS("datosElecciones")
varObjBin <- as.factor(datos$varObjBin)
varObjCont<-datos$varObjCont
input<-as.data.frame(datos[,-(1:2)])

#Veo grÃ¡ficamente el efecto de dos variables cualitativas sobre la binaria
mosaico(input$CCAA,varObjBin,"CCAA") # En este influye
mosaico(input$PartidoCCAA,varObjBin,"Partido CCAA") # En este influye
mosaico(input$prop_missings,varObjBin,"Propiedades Missing") # En este influye
summary(input)


#Bucle para obtener todos los gr?ficos. Si hay muchas variables, no es recomendable
for (i in which(sapply(input, class)=="numeric")){
  print(boxplot_cuantcuali(input[,i],varObjBin,names(input)[i]))
}
# Censo y poblaciÃ³n se observa que no influyen
# PersonasInmueble y Age_over65_Ptge si influyen


#Veo gr?ficamente el efecto de dos variables cualitativas sobre la continua
boxplot_cuantcuali(varObjCont,input$CCAA,"varObjCont")
boxplot_cuantcuali(varObjCont,input$PartidoCCAA,"varObjCont")


#Todas las variables num?ricas frente a la objetivo continua
corrplot(cor(data.frame(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")
dispersion(Filter(is.numeric, input),varObjCont)
# VarObjCont tiene correlaciÃ³n inversa con Age_Over65_Ptge
# VarObjCont tiene correlaciÃ³n directa con SeriviciosPtge
# ComercTTEHosteleriaPtge en menor medida
# PersonasInmueble tambien en menor medida


# Creo las variables aleatorias
set.seed(987654)
input$aleatorio<-runif(nrow(input))
input$aleatorio2<-runif(nrow(input))

summary(input)
#Obtengo la importancia de las variables. 
par(mar=c(12, 5.1, 4.1, 2.1)) 
graficoVcramer(input,varObjBin) 
graficoVcramer(input,varObjCont)

# Guardo los cambios de las variables "eliminadas"
saveRDS(data.frame(varObjBin,varObjCont,input),"datosElecciones2")

