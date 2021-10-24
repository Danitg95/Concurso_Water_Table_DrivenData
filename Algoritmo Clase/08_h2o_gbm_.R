#------------------
# Autor: Carlos Ortega
# Date: 2021_09_3
# Inputs: Training, test, labels from Drivendata.org (Pump). 
# Output: Charts, submission.
# Changes: H2O gbm
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
    library(h2o)           # Opensource ML framework/library
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

#--- Por necesidades de H2O -> Categoricas (character) a Factor.
#-- Train
trainOrinumlogicmputCate_fact <- trainOrinumlogicmputCate %>%
  mutate_if(is.character, as_factor) %>% as.data.table()

#-- Test
testOrinumlogicimputCate_fact <- testOrinumlogicimputCate %>%
  mutate_if(is.character, as_factor) %>% as.data.table()

#------ INI ------ MODELING - RANDOMFOREST - H2O
tic()
h2o.init(max_mem_size = "4g")

#--- Convert train and test to h2o objects.
train_h2o  <- as.h2o(trainOrinumlogicmputCate_fact)
test_h2o <- as.h2o(testOrinumlogicimputCate_fact)


response   <- "status_group"
predictors <- setdiff(names(train_h2o), response)

mymodel_h2o <- h2o.gbm(
  y = response,  
  x = predictors,
  training_frame = train_h2o
)

#--- See model output
mymodel_h2o

#--- Error Estimation
acierto_val <- h2o.logloss(mymodel_h2o)
acierto_val

#------ END ------ MODELING - RANDOMFOREST - ranger

#----- INI --------- PREDICTION + SUBMISSION
tic()
mypred <- h2o.predict( mymodel_h2o, test_h2o)
mypred_df <- as.data.frame(mypred)
toc()

#--- Submission
mysub <- data.frame(
  id = testOri$id,
  status_group = mypred_df$predict
)
fwrite(
  mysub, 
  paste("./submissions/08_h2o_gbm_", 
        round(acierto_val,4),
        "_numvars_",  ncol(test_h2o), 
        "_.csv", sep =  "")
)

#----- END --------- PREDICTION + SUBMISSION


#----- Resultados
# 00 - trees 500 - min.n.s = 1 - 10 vars num - Local: 0.7128 - Plataforma: 0.7130
# 01 - trees 500 - min.n.s = 1 - 12 vars num - Local: 0.7141 - Plataforma: 0.7158
# 02 - trees 500 - min.n.s = 1 - 32 vars num - Local: 0.8122 - Plataforma: 0.8164
# 03 - trees 500 - min.n.s = 1 - 36 vars num + categ. (up to scheme_name) - Local: 0.8177 - Plataforma: 0.8146
# 03 - trees 600 - min.n.s = 2 - mtry = 5 - 32 vars num (same as model_2) - Local: 0.8127 - Plataforma: 0.8177
# 07 - trees 500 - min.n.s = 1 - 32 vars num - Local: - Plataforma: 0.8046  - H2O - logloss: 0.47..
# 08 - h2o gbm - 0.7832 - loglosss = 0.5083
#------
