  
  
  rm(list=ls())
  
  #Escoger directorio
  dir_set <- function(){
    if(Sys.info()["user"]=="JuanJose"){
      setwd("/Users/JuanJose/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Uniandes/9 Semestre - 1 PEG/Big Data/Proyecto Final")
    }
    else {
      setwd("")
    }
  }
  
  dir_set()
  
  #Librerias
  pacman::p_load(tidyverse,stargazer,modelsummary,haven,lmtest, fastDummies,
                 estimatr,magrittr,kableExtra,car,rdrobust,mass,ClusterBootstrap,Boot, pscl, Synth)
  
  pacman::p_load(tidyverse,caret,tidymodels,glmnet,parallel,doParallel,MLmetrics,themis,rattle,rlang,randomForest,mlr,rpart,rpart.plot,kableExtra,xgboost,SuperLearner)
  
  
  set.seed(1234)
  n_cores<-detectCores()
  cl <- makePSOCKcluster(n_cores - 1) 
  registerDoParallel(cl)
  
  ############### IMPORT DATA ########################################################
  
  COLEGIO<-readRDS("Saber11.rds")

  
  COLEGIO_ESTRATO<-readRDS("Saber11_COLEGIO_ESTRATO.rds")  
  
  ############### TEST DEL TRAIN ########################################################
  set.seed(1234)
  COLEGIO<-COLEGIO%>%
    filter(!is.na(UNIANDES.LEAD))%>%
    filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70 |DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)%>%
    filter(ESTRATO<=3)
  
  COLEGIO_ESTRATO<-COLEGIO_ESTRATO%>%
    filter(!is.na(UNIANDES.LEAD))%>%
    filter(DEPTO==8|DEPTO==13|DEPTO==20 |DEPTO==23| DEPTO==44| DEPTO==47 |DEPTO==70 |DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76)%>%
    filter(ESTRATO<=3)
  
  set.seed(1234)
  Train_N <- floor(0.7*nrow(COLEGIO))
  Train_ind <- sample(1:nrow(COLEGIO), size = Train_N)
  
  Train_Colegio<-COLEGIO[Train_ind, ]
  Test_Colegio<-COLEGIO[-Train_ind, ]
  Train_Colegio_recipe<- recipe(UNIANDES.LEAD ~., data = Train_Colegio)
  
  
  COLEGIO_ESTRATO3<-COLEGIO_ESTRATO%>%
    filter(ESTRATO<=3)

  ############### MODELOS ########################################################
  
  control = trainControl(method = "cv",number = 5,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE)
  
  ##### RANDOM FOREST #####
  set.seed(1234)
  registerDoParallel(cl)
  
  #train cv 
  tunegrid <- expand.grid(.mtry = (1:15))
  randomForest<-caret::train(Train_Colegio_recipe,data=Train_Colegio, method='rf',tungeGrid=tunegrid,trControl=control)

#predict train cv for metrics 
Test_Colegio$p_RF<-round(predict(randomForest,Test_Colegio))
COLEGIO_ESTRATO$p_RF<-round(predict(randomForest,COLEGIO_ESTRATO))
predictions_colegio$p_RF<-round(predict(randomForest,predictions_colegio))
RF_metric_train<-data.frame(cv=max(randomForest[["results"]][["RMSE"]]),Test=RMSE(y_pred=Test_Colegio$p_RF,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_RF,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                            mtry=randomForest[["bestTune"]][["mtry"]])
stargazer(type = "text",RF_metric_train,out = "outputs/RF_metric_train.txt",summary = F)


saveRDS(COLEGIO_ESTRATO,"predictions.rds")


##### RANDOM FOREST - Ranger #####
set.seed(1234)
registerDoParallel(cl)

tunegrid <- expand.grid(max.depth=c(2,4,6,9), min.node.size = seq(10, 100, length.out = 10))
randomForest_ranger<-caret::train(Train_Colegio_recipe,data=Train_Colegio, method='ranger',tungeGrid=tunegrid,trControl=control)

#predict train cv for metrics 
Test_Colegio$p_RF_ranger<-round(predict(randomForest_ranger,Test_Colegio))
COLEGIO_ESTRATO$p_RF_ranger<-round(predict(randomForest_ranger,COLEGIO_ESTRATO))

predictions_colegio$p_RF_ranger<-round(predict(randomForest_ranger,predictions_colegio))

RF_ranger_metric_train<-data.frame(cv=max(randomForest_ranger[["results"]][["RMSE"]]),Test=MSE(y_pred=Test_Colegio$p_RF_ranger,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_RF_ranger,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                                   mtry=randomForest_ranger[["bestTune"]][["mtry"]],  splitrule=randomForest_ranger[["bestTune"]][["splitrule"]],  min.node.size=randomForest_ranger[["bestTune"]][["min.node.size"]])
stargazer(type = "text",RF_ranger_metric_train,out = "outputs/RF_ranger_metric_train.txt",summary = F)

saveRDS(COLEGIO_ESTRATO,"predictions.rds")



##### XGBoost #####
set.seed(1234)
registerDoParallel(cl)

#train
tunegrid <- expand.grid(nrounds = c(250,500),max_depth = c(2,3,4,6,8),eta = c(0.01,0.3,0.5),gamma = c(0,1), min_child_weight = c(10, 25,50),colsample_bytree = c(0.7), subsample = c(0.6))
xgb <-caret::train(Train_Colegio_recipe,data=Train_Colegio,method = "xgbTree",tuneGrid = tunegrid,trControl = control)

#predict train cv for metrics 
Test_Colegio$p_xgb<-round(predict(xgb,Test_Colegio))
COLEGIO_ESTRATO$p_xgb<-round(predict(xgb,COLEGIO_ESTRATO))
COLEGIO_ESTRATO3$p_xgb<-round(predict(xgb,COLEGIO_ESTRATO3))
COLEGIO$p_xgb<-round(predict(xgb,COLEGIO))
xgb_metric_train<-data.frame(cv=max(xgb[["results"]][["RMSE"]]),Test=MSE(y_pred=Test_Colegio$p_xgb,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_xgb,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                             nrounds=xgb[["bestTune"]][["nrounds"]], max_depth=xgb[["bestTune"]][["nrounds"]], eta=xgb[["bestTune"]][["eta"]], gamma=xgb[["bestTune"]][["gamma"]], min_child_weight=xgb[["bestTune"]][["min_child_weight"]] )
stargazer(type = "text",xgb_metric_train,out = "outputs/RF_metric_train.txt",summary = F)


saveRDS(COLEGIO_ESTRATO,"predictions.rds")


##### glmnet #####
set.seed(1234)
registerDoParallel(cl)
tunegrid<-expand.grid(alpha = seq(0,1,0.1), lambda=seq(0,1,0.01) )
glmnet <- caret::train(Train_Colegio_recipe,data=Train_Colegio,trControl = control,tuneGrid =tunegrid ,method = "glmnet",metric="MSE", family="gaussian")

#predict train cv for metrics 
Test_Colegio$p_glmnet<-round(predict(glmnet,Test_Colegio))
COLEGIO_ESTRATO$p_glmnet<-round(predict(glmnet,COLEGIO_ESTRATO))
COLEGIO_ESTRATO3$p_glmnet<-round(predict(glmnet,COLEGIO_ESTRATO3))
COLEGIO$p_glmnet<-round(predict(glmnet,COLEGIO))


glmnet_metric_train<-data.frame(cv=max(glmnet[["results"]][["RMSE"]]),Test=MSE(y_pred=Test_Colegio$p_glmnet,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_glmnet,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                               alpha=glmnet[["bestTune"]][["alpha"]], lambda=glmnet[["bestTune"]][["lambda"]])
stargazer(type = "text",glmnet_metric_train,out = "outputs/glmnet_metric_train.txt",summary = F)

saveRDS(COLEGIO_ESTRATO,"predictions.rds")


saveRDS(Test_Colegio,"predictionsColegio.rds")


##### Regression Trees Bagging  #####
set.seed(1234)
registerDoParallel(cl)

tunegrid <- expand.grid(vars=c(3,4,5,6,8))
Bag <- caret::train(Train_Colegio_recipe,data=Train_Colegio,trControl = control, method = "bag",metric="RMSE")

#predict train cv for metrics 
Test_Colegio$p_Bag<-round(predict(Bag,Test_Colegio))
COLEGIO_ESTRATO$p_Bag<-round(predict(Bag,COLEGIO_ESTRATO))
Bag_metric_train<-data.frame(cv=max(Bag[["results"]][["RMSE"]]),Test=MSE(y_pred=Test_Colegio$p_Bag,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_Bag,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                             nrounds=Bag[["bestTune"]][["mfinal"]], tree_depth=Bag[["bestTune"]][["maxdepth"]])
stargazer(type = "text",Bag_metric_train,out = "outputs/Bag_metric_train.txt",summary = F)





##### Neural Net #####
set.seed(1234)
registerDoParallel(cl)
tunegrid <- expand.grid(size=seq(1:20),decay=seq(0,1,0.1))
N_NET <-caret::train(Train_Colegio_recipe,data=Train_Colegio,trControl = control, method = "nnet",metric='RMSE')

#predict train cv for metrics 
Test_Colegio$p_N_NET<-round(predict(N_NET,Test_Colegio))
COLEGIO_ESTRATO$p_N_NET<-round(predict(N_NET,COLEGIO_ESTRATO))
COLEGIO_ESTRATO3$p_N_NET<-round(predict(N_NET,COLEGIO_ESTRATO3))
COLEGIO$p_N_NET<-round(predict(N_NET,COLEGIO))

N_NET_metric_train<-data.frame(cv=max(N_NET[["results"]][["RMSE"]]),Test=Accuracy(y_pred=Test_Colegio$p_N_NET,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_N_NET,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                               decay=N_NET[["bestTune"]][["decay"]],size=N_NET[["bestTune"]][["size"]])
stargazer(type = "text",N_NET_metric_train,out = "outputs/N_NET_metric_train.txt",summary = F)

saveRDS(COLEGIO_ESTRATO,"predictions.rds")





##### Average Neural Net #####
set.seed(1234)
registerDoParallel(cl)
tunegrid <- expand.grid(bag=c(TRUE,FALSE),size=seq(1:20),decay=seq(0,1,0.1))
avNNet <-caret::train(Train_Colegio_recipe,data=Train_Colegio,trControl = control, tuneGrid=tunegrid, method = "avNNet",metric='RMSE')

#predict train cv for metrics 
Test_Colegio$p_avNNet<-round(predict(avNNet,Test_Colegio))
COLEGIO_ESTRATO$p_avNNet<-round(predict(avNNet,COLEGIO_ESTRATO))
COLEGIO_ESTRATO3$avNNet<-round(predict(avNNet,COLEGIO_ESTRATO3))
COLEGIO$avNNet<-round(predict(avNNet,COLEGIO))

p_avNNet_metric_train<-data.frame(cv=max(avNNet[["results"]][["RMSE"]]),Test=Accuracy(y_pred=Test_Colegio$p_avNNet,y_true=Test_Colegio$UNIANDES.LEAD),Pred=RMSE(y_pred=COLEGIO_ESTRATO$p_avNNet,y_true=COLEGIO_ESTRATO$UNIANDES.LEAD),
                               decay=avNNet[["bestTune"]][["decay"]],size=avNNet[["bestTune"]][["size"]],bag=avNNet[["bestTune"]][["bag"]])
stargazer(type = "text",p_avNNet_metric_train,out = "outputs/p_avNNet_metric_train.txt",summary = F)

saveRDS(COLEGIO_ESTRATO,"predictions.rds")


saveRDS(COLEGIO_ESTRATO3,"predictions_estrato3.rds")

saveRDS(predictions_colegio,"predictions_colegio.rds")

predictions_colegio_<- read_rds("predictions_colegio.rds")

saveRDS(Test_Colegio,"predictions_Test_Colegio.rds")

#neuralnet


#####
# revision de los resultados 

Resultados<-predictions_colegio%>%
  group_by(DEPTO,ESTRATO)%>%
  summarise(RandomForest = sum(p_RF),
            RandomForest_Ranger = sum(p_RF_ranger),
            xbgoost = sum(p_xgb),
            NeuralNet = sum(p_N_NET),
            ElasticNet = sum(p_glmnet)
            )%>% filter(ESTRATO<=3)


Resultados<-predictions_colegio%>%
  filter(ESTRATO<=3)%>%
  group_by(DEPTO,PERIODO)%>%
  summarise(Original = sum(UNIANDES.LEAD),
            RandomForest_Ranger = sum(p_RF_ranger),
            xbgoost = sum(p_xgb)
  )



Resultados<-predictions_colegio%>%
  filter(ESTRATO<=3)%>%
  group_by(DEPTO,PERIODO)%>%
  summarise(Original = sum(UNIANDES.LEAD),
            RandomForest_Ranger = sum(p_RF_ranger),
            xbgoost = sum(p_xgb)
  )%>%
  ungroup()
  
Resultados2<-Resultados%>%
  group_by(DEPTO)%>%
  summarise(Original = mean(Original),
            RandomForest_Ranger = mean(RandomForest_Ranger),
            xbgoost = mean(xbgoost)
  )



colSums(filter(Resultados2,DEPTO==19|DEPTO==27|DEPTO==52 |DEPTO==76))#avNNet

#qrnn

#brnn

#gamboost

#xgbDART

#xgbLinear

#gbm



# 