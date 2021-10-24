#------ H2O - Example ---------

#-- Library loading
library(h2o)

#-- Start h2o cluster
h2o.init()

#--- Automl model - default parameters.
prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
prostate <- h2o.importFile(path = prostate_path, header = TRUE)
y <- "CAPSULE"
prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification

#----- All algorithms.
aml <- h2o.automl(y = y, training_frame = prostate, max_runtime_secs = 30)

lb <- h2o.get_leaderboard(aml)
head(lb)

#--- Automl model - excluding Stack + Deeplearning and 20 models.
aml_v2 <- h2o.automl(
                  y = y, 
                  training_frame = prostate, 
                  exclude_algos = c('DeepLearning', 'StackedEnsemble', 'GLM', 'DRF', 'GBM'),
                  max_models = 20
                    )

lb_v2 <- h2o.get_leaderboard(aml_v2)
lbv2_dt <- as.data.table(lb_v2)
lbv2_dt


