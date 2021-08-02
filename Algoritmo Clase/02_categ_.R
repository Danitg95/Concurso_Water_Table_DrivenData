#------------------
# Autor: Carlos Ortega
# Date: 2021_07_23
# Inputs: Training, test, labels from Drivendata.org (Pump). 
# Output: Charts, submission.
# Changes: To include categorical variabvles.
#------------------

#------ Libraries
suppressPackageStartupMessages(
  {
    library(data.table)    # Fast Data processing.
    library(tictoc)        # To measure computing time.
    library(tidytable)     # data.table with dplyr syntax.
    library(inspectdf)     # Automatic EDA.
    library(DataExplorer)  # Automatic EDA.
    library(dplyr)         # Data processing with pipes.
    library(ranger)        # Fast RandomForest.
    library(magrittr)      # Piping.
    library(ggplot2)       # The most beautiful charts in the ML world.
    library(forcats)       # Treat categorical variables
    library(missRanger)    # NA's imputation with ranger.
  } 
)


#------- INI ------- Data Loading
trainOri    <- as.data.frame(fread("./data/train.csv"))
testOri     <- fread("./data/test.csv", data.table = FALSE)
trainlabOri <- fread("./data/train_labels.csv", data.table = FALSE)
#------- END ------- Data Loading


# #------- INI ------- EDA (with inspectdf)
# # Horizontal bar plot for categorical column composition
# x <- inspect_cat(trainOri) 
# show_plot(x)
# 
# # Correlation betwee numeric columns + confidence intervals
# x <- inspect_cor(trainOri)
# show_plot(x)
# 
# # Bar plot of most frequent category for each categorical column
# x <- inspect_imb(trainOri)
# show_plot(x)
# 
# # Bar plot showing memory usage for each column
# x <- inspect_mem(trainOri)
# show_plot(x)
# 
# # Occurence of NAs in each column ranked in descending order
# x <- inspect_na(trainOri)
# show_plot(x)
# 
# # Histograms for numeric columns
# x <- inspect_num(trainOri)
# show_plot(x)
# 
# # Barplot of column types
# x <- inspect_types(trainOri)
# show_plot(x)
#------- END ------- EDA (with inspectdf)
#------ Results
#-- Niveles de las categoricas.
# recorded_by: A tirar. Es cte!.
# subvillaje, ward, wpt_names : Tope de niveles.
# Cuidadín con: funder, installer, scheme_name
#-- Parecias:
# payment - payment_type
# quantity - quantity_group
# source - source_type
# 
#-- Correlación: 
# district_code/region_code ~0.6
# construction_year/gps_height ~0.6
#-- NAs:
# public_meeting: 5.6%
# permit: 5.1%
# resto de vars: limpias.
#-- Hist de numericas.
# amount_tsh: outliers (pocos)
# construction_year: hay muchos ceros (NAs enmascarados)
# district_code: ¿categórico?
# gps_height: valores negativos y de ceros?.
# latitude/longitud: ¿ceros?.
# num_private: ¿ceros?
# population: outliers / ceros?
# region_code: ¿categorica?.

#------ INI - FEATURE ENGINEERING
# Decide el equipo de comun acuerdo usar ls numericas para empezar
trainOrinum <- trainOri %>% 
  select(where(is.numeric))

# Tenemos que pegar la "target" que está en "trainlabOri".
# Como no nos fiamos hacemos un join.
trainOrinumlab <- merge(trainOrinum, trainlabOri, by.x = c('id'), by.y = c('id'))

# Factor status_group que si no ranger no funciona...
trainOrinumlab$status_group <- as.factor(trainOrinumlab$status_group)

#Alternativa...
#all.equal(trainOrinum$id, trainlabOri$id )

#---- Voy a incorporar las variables lógicas
#--- cuidado que hay NAs... TRUE -> 1, FALSE = 0
trainOrilogic <- trainOri %>%
  select(where(is.logical)) %>%
  mutate(
    mipermit = if_else(permit == TRUE, 1, 0),
    mipubmee = if_else(public_meeting == TRUE, 1, 0)
  ) %>%
  select(mipermit, mipubmee) %>%
  rename(permit = mipermit, public_meeting = mipubmee)
#--- Qué hago con los NAs?
#--- junto las numericas y las logicas 
trainOrinumlogic <- cbind(trainOrinum, trainOrilogic)
#--- Y ahora imputo los NAs.
trainOrinumlogicimput <- missRanger(trainOrinumlogic, pmm.k = 5, num.trees = 100)

#----- Incluir Categóricas....
trainCate <- trainOri %>%
  select(where(is.character), -recorded_by)

myfun <- function(x) {
  num_level <- length(unique(x))
  return(num_level)
}
levels_val <- as.data.frame(apply(trainCate, 2, myfun))  
names(levels_val) <- c('num_lev')
levels_val$var <- rownames(levels_val)
rownames(levels_val) <- NULL
levels_val %<>% 
  arrange(num_lev)
# Elegir hasta lga....
minivel <- 200
var_god <- levels_val %>%
  filter(num_lev < minivel ) %>%
  select(var) %>%
  pull()

#--- Las definitivas categoricas con las que me quedo
trainCatedef <- trainOri[ , var_god]

#--- Conjunto de entrenamiento definitivo
trainOrinumlogicmputCate <- cbind(trainOrinumlogicimput, trainCatedef)

#--- Añado target train Orinumlogicimput como factor!!
trainOrinumlogicmputCate$status_group <- as.factor(trainlabOri$status_group)

#------------- TEST -----------
#---- Tranformo el Test...  elijo las num/logic...
testOrinumlogic <- testOri %>%
  select(where(is.numeric), where(is.logical)) %>%
  mutate(permit = as.numeric(permit), public_meeting = as.numeric(public_meeting))

testOrinumlogicimput <- missRanger(testOrinumlogic, pmm.k = 5, num.trees = 100)

testOrinumlogicimputCate <- cbind(testOrinumlogicimput, testOri[ , var_god ])

#------ END - FEATURE ENGINEERING

#------ INI ------ MODELING - RANDOMFOREST - ranger
# mymodel <- ranger( y = ......) - ranger(y ~ .) - ranger(y ~ var1 + var2 + var3...)
# mymodel <- algoritmo( x = X, y = Y)
tic()
mymodel <- ranger( 
  status_group ~ . , 
  data = trainOrinumlogicmputCate,
  importance = 'impurity',
  num.trees = 5000,
  verbose = TRUE
)
toc()
# num.trees = 500
# min.node.size = 1

acierto_val <- 1 - mymodel$prediction.error
acierto_val

#------ END ------ MODELING - RANDOMFOREST - ranger

#----- INI --------- IMPORTANCIA VARIABLES
var_impor <- as.data.frame(mymodel$variable.importance)
names(var_impor) <- c('importancia') 
var_impor$variables <- rownames(var_impor)
rownames(var_impor) <- NULL

var_impor %<>%
  arrange(desc(importancia))
var_impor
# long, lat, gps, id....

var_impor %>%
  ggplot(aes(x = fct_reorder(variables, importancia), y = importancia)) +
  geom_col(group = 1, fill = 'darkred') +
  labs(
    x = '', 
    y = 'Importancia Relativa',
    title = "IMPORTANCIA MODELO",
    subtitle = paste("algoritmo ranger - acierto: ", round(acierto_val,4), sep = ""), 
    caption = paste("num vars: ", nrow(var_impor), sep = "") 
  ) +
  coord_flip() + 
  theme_bw()
ggsave(
  paste("./charts/02_Importancia_algoritmo_ranger_variables_acierto_", 
        round(acierto_val,4),
        "_numvars_",  nrow(var_impor), 
        "_.png", sep =  "")
)

#----- END --------- IMPORTANCIA VARIABLES

#----- INI --------- PREDICTION + SUBMISSION
tic()
mypred <- predict( mymodel, testbis)$predictions
toc()

mysub <- data.frame(
  id = testOri$id,
  status_group = mypred
)

fwrite(
  mysub, 
  paste("./submissions/02_ranger_acierto_", 
        round(acierto_val,4),
        "_numvars_",  nrow(var_impor), 
        "_.csv", sep =  "")
)

#----- END --------- PREDICTION + SUBMISSION

#----- Resultados
# 00 - trees 500 - min.n.s = 1 - 10 vars num - Local: 0.7128 - Plataforma: 0.7130
# 01 - trees 500 - min.n.s = 1 - 12 vars num - Local: 0.7141 - Plataforma: 0.7158
# 02 - trees 500 - min.n.s = 1 - 32 vars num - Local: 0.8122 - Plataforma: 0.8164

#------
var_impor
var_impor$importancia[1] <- 30000
