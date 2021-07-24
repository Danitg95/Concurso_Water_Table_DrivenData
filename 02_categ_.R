#------------------
# Autor: Carlos Ortega
# Date: 2021_07_23
# Inputs: Training, test, labels from Drivendata.org (Pump). 
# Output: Charts, submission.
# Changes: Initial Version
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
    library(magrittr)      # Pipelines  
    library(ggplot2)       # The most beautiful charts in the world
    library(forcats)       # Treat categorical variables
    library(missRanger)    # Treat missing in ranger
    } 
)


#------- INI ------- Data Loading
trainOri                           <- as.data.frame(fread("./data/train.csv"))
testOri                            <- fread("./data/test.csv", data.table = FALSE)
trainlabOri                        <- fread("./data/train_labels.csv", data.table = FALSE)
#------- END ------- Data Loading


#------- INI ------- EDA (with inspectdf)
# Horizontal bar plot for categorical column composition
x                                  <- inspect_cat(trainOri) 
show_plot(x)

# Correlation betwee numeric columns + confidence intervals
x                                  <- inspect_cor(trainOri)
show_plot(x)

# Bar plot of most frequent category for each categorical column
x                                  <- inspect_imb(trainOri)
show_plot(x)

# Bar plot showing memory usage for each column
x                                  <- inspect_mem(trainOri)
show_plot(x)

# Occurence of NAs in each column ranked in descending order
x                                  <- inspect_na(trainOri)
show_plot(x)

# Histograms for numeric columns
x                                  <- inspect_num(trainOri)
show_plot(x)

# Barplot of column types
x                                  <- inspect_types(trainOri)
show_plot(x)
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
trainOrinum                        <- trainOri %>% 
                   select(where(is.numeric))

# Tenemos que pegar la "target" que está en "trainlabOri".
# Como no nos fiamos hacemos un join.
trainOrinumlab                     <- merge(trainOrinum, trainlabOri, by.x = c('id'), by.y = c('id'))

# Factor status_group que si no ranger no funciona...
trainOrinumlab$status_group        <- as.factor(trainOrinumlab$status_group)

#Alternativa...
#all.equal(trainOrinum$id, trainlabOri$id )

# --- voy a incorporar las variables lógicas

trainOrilogic                      <- trainOri %>%
                    select.(where(is.logical)) %>%
                    mutate(
                      mipermit = if_else(permit == TRUE, 1, 0),
                      mipubmee = if_else(public_meeting == TRUE, 1, 0)
                    ) %>%
                    select(mipermit, mipubmee) %>%
                    rename(permite =mipermit, public_meeting = mipubmee)

# ¿Qué hago con los NA?
# Junto las numéricas y las lógicas
trainOrinumlogic                   <- cbind(trainOrinum, trainOrilogic)

# Y ahora imputo los NA
trainOrinumlogicimput              <- missRanger(trainOrinumlogic, pmm.k = 5, num.tress = 100)


# Incluir categóricas
trainCate                          <- trainOri %>%
  select(where(is.character), - recorded_by)

myfun                              <- function(x){
  num_level                        <- length(unique(x))
  return(num_level)
}

levels_val                         <- as.data.frame(apply(trainCate, 2, myfun))
names(levels_val)                  <- c('num_lev')
levels_val$var                     <- rownames(levels_val)
rownames(levels_val)               <- NULL
levels_val %<>%
  arrange(num_lev)

# Elegir hasta lga
minivel                            <- 200 
var_good                           <- levels_val %>%
    filter(num_lev < minivel) %>%
    select(var) %>%
    pull()

# -- Las defintivas categoricas con las que me quedo
# trainCatdef                      <- trainOri[,var_good]


# Transformo el test... elijo las num/logic
testOrinumlogic                    <- testOri %>%
  select(where(is.numeric), where(is.logical)) %>%
  mutate(permit = as.numeric(permit), public_meeting(as.numeric(public_meeting)))

  
testOrinumlogicimput               <- missRanger(testOrinumlogic, pmm.k = 5, num.trees = 100)

# Añado target train orinumlogicimput como factor!!
trainOrinumlogicimput$status_group <- as.factor(trainlabOri$status_group)


#------ END - FEATURE ENGINEERING

#------ INI ------ MODELING - RANDOMFOREST - ranger
# mymodel                          <- ranger( y = ......) - ranger(y ~ .) - ranger(y ~ var1 + var2 + var3...)
# mymodel                          <- algoritmo( x = X, y = Y)
# 
mymodel                            <- ranger( 
                    status_group ~ . , 
                    data = trainOrinumlogicimput,
                    verbose = TRUE,
                    importance = 'impurity'
                 )

acierto_val                        <- 1 - mymodel$prediction.error
acierto_val


#------ END ------ MODELING - RANDOMFOREST - ranger

# ---- INI ---- IMPORTANCIA VARIABLES
var_import                         <- as.data.frame(mymodel$variable.importance)
names(var_import)                  <- c('importancia')
var_import$variables               <- rownames(var_import)
rownames(var_import)               <-NULL

var_import %<>%
    arrange(desc(importancia))
var_import
# long, lat, gps, id...

var_import %>%
  ggplot(aes(x = fct_reorder(variables, importancia), y = importancia)) + 
        geom_col(group = 1, fill = 'darkred') + 
        labs( x = '', 
              y = 'Importancia relativa', 
              title = 'IMPORTANCIA MODELO',
              subtitle = paste('Algoritmo Ranger - acierto: ', 
                               round(acierto_val,4), sep =""),
              caption =  paste("num_vars: ", nrow(var_import),
                               sep = "")
              ) +
        coord_flip() + 
        theme_bw()
ggsave(
       paste("./charts/00_Importancia_variables_acierto_", 
             round(acierto_val, 4), 
             "_numvars_", 
             nrow(var_import), 
             "_.png", sep = "")
       )
# --- END --- IMPORTANCIA DE LAS VARIABLES

# ---- INI --- PREDICTION

mypred                             <- predict(mymodel, testOrinumlogicimput)$predictions

mysub                              <- data.frame(
                    id = testOri$id,
                    status_group = mypred
                    )

fwrite(mysub, 
       paste("./submissions/00_ranger_acierto_", 
             round(acierto_val, 4), 
             "_numvars_", 
             nrow(var_import), 
             "_.csv", sep = "")
       )


# ---- Resultados
# 00 - trees 500 - min.n.s = 1 - 10 vars num - 0.7128 - Plataf 0.7130