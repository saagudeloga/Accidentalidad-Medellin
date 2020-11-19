library(data.table)   # manejo de tablas
library(purrr)        # optimizacion de bucles
library(dplyr)        # manejo de tablas
library(plotly)       # graficos en html
library(tidyr)        # limpieza de datos
library(stringr)      # limpieza de texto
library(lubridate)    # limpieza de fechas
library(sf) 
# Lectura de datos completo
setwd("Dato_cod")
medellin <- fread(file = "Otros/accidentalidad_medellin_final.csv", sep = ",",
                  encoding = "UTF-8", colClasses = "character")
str(medellin)

# Estructuracion variables
medellin$FECHA <- as.Date(medellin$FECHA, "%Y-%m-%d")
medellin$LONGITUD <- as.numeric(medellin$LONGITUD)
medellin$LATITUD <- as.numeric(medellin$LATITUD)
medellin$DIA <- as.integer(medellin$DIA)
medellin$PERIODO <- as.integer(medellin$PERIODO)
medellin$CLASE <- as.factor(medellin$CLASE)
medellin$GRAVEDAD <- as.factor(medellin$GRAVEDAD)
medellin$DISENO <- as.factor(medellin$DISENO)
medellin$DIA_NOMBRE <- as.factor(medellin$DIA_NOMBRE)
medellin$MES <- as.integer(medellin$MES)
medellin$COMUNA <- as.factor(medellin$COMUNA)
medellin$BARRIO <- as.factor(medellin$BARRIO)
medellin$ESPECIAL <- as.factor(medellin$ESPECIAL)
medellin$HORA_PICO <- as.factor(medellin$HORA_PICO)
medellin$HUMEDAD_REL <- as.numeric(medellin$HUMEDAD_REL)
medellin$PLUVIO <- as.numeric(medellin$PLUVIO)

#=======================
#       Modelacion
#=======================
require(lubridate)
require(tidyverse)
require(fastDummies)
require(xgboost)
require(Matrix)
require(gamlss)
require(skimr)
require(ranger)
library(FNN)
require(neuralnet)

#==============================
#  Imputación datos faltantes
#===============================
apply(apply(medellin, 2, is.na),2,sum)
medellin$HUMEDAD_REL[is.na(medellin$HUMEDAD_REL)] <- mean(medellin$HUMEDAD_REL, na.rm = T)
medellin$PLUVIO[is.na(medellin$PLUVIO)] <- 0

#=======================================================
#   Elección conjunto de entrenamiento y validación
#=======================================================
Train <- medellin[medellin$PERIODO %in% c(2014,2015,2016,2017),]
Test <- medellin[medellin$PERIODO == 2018,]

dim(Train) #  accidentes en 1460 dias desde 2014 hasta 2017
dim(Test) # 40273 accidentes en 365 dias del 2018

#=========================
#    Puesta a Punto
#==========================

# Convertir ESPECIAL y HORA PICO en binarias
Train$ESPECIAL <- ifelse(Train$ESPECIAL=='SI',1,0)
Train$HORA_PICO <- ifelse(Train$HORA_PICO=='SI',1,0)

Test$ESPECIAL <- ifelse(Test$ESPECIAL=='SI',1,0)
Test$HORA_PICO <- ifelse(Test$HORA_PICO=='SI',1,0)

Train$CLASE <- as.factor(Train$CLASE)
levels(Train$CLASE) <- c('Atropello','Caida_ocupante', 'Choque','Choque',
                         'Incendio','Otro','Volcamiento')
Test$CLASE <- as.factor(Test$CLASE)
levels(Test$CLASE) <- c('Atropello','Caida_ocupante', 'Choque','Choque',
                        'Incendio','Otro','Volcamiento')

#====================================================
#  Agrupamiento variables temporales para los modelos
#====================================================
# Grupo1: Mensual	
Mensual <- Train %>% group_by(Mes = month(FECHA),CLASE) %>% 
  summarise(cantidad = length(FECHA),
            festivos = sum(ESPECIAL),
            Hora_Pico = sum(HORA_PICO),
            ValorHumR = mean(HUMEDAD_REL, na.rm = T),
            Pluvial  = max(PLUVIO)) %>% 
  dummy_cols(select_columns = c('CLASE'),
             remove_selected_columns = T)

MensualTs <- Test %>% group_by(Mes = month(FECHA),CLASE) %>% 
  summarise(cantidad = length(FECHA),
            festivos = sum(ESPECIAL),
            Hora_Pico = sum(HORA_PICO),
            ValorHumR = mean(HUMEDAD_REL, na.rm = T),
            Pluvial  = max(PLUVIO)) %>% 
  dummy_cols(select_columns = c('CLASE'),
             remove_selected_columns = T)	

# Grupo 2: Diario
Diario <- Train %>% group_by(Dia = day(FECHA),CLASE) %>% 
  summarise(cantidad = length(FECHA),
            festivos = sum(ESPECIAL),
            Hora_Pico = sum(HORA_PICO),
            ValorHumR = mean(HUMEDAD_REL, na.rm = T),
            Pluvial  = max(PLUVIO)) %>% 
  dummy_cols(select_columns = c('CLASE'),
             remove_selected_columns = T)

DiarioTs <- Test %>% group_by(Dia = day(FECHA),CLASE) %>% 
  summarise(cantidad = length(FECHA),
            festivos = sum(ESPECIAL),
            Hora_Pico = sum(HORA_PICO),
            ValorHumR = mean(HUMEDAD_REL, na.rm = T),
            Pluvial  = max(PLUVIO)) %>% 
  dummy_cols(select_columns = c('CLASE'),
             remove_selected_columns = T)


# Grupo 3: Semanal
Semanal <- Train %>% group_by(Semana = week(FECHA),CLASE) %>% 
  summarise(cantidad = length(FECHA),
            festivos = sum(ESPECIAL),
            Hora_Pico = sum(HORA_PICO),
            ValorHumR = mean(HUMEDAD_REL, na.rm = T),
            Pluvial  = max(PLUVIO)) %>% 
  dummy_cols(select_columns = c('CLASE'),
             remove_selected_columns = T)


SemanalTs <- Test %>% group_by(Semana = week(FECHA),CLASE) %>% 
  summarise(cantidad = length(FECHA),
            festivos = sum(ESPECIAL),
            Hora_Pico = sum(HORA_PICO),
            ValorHumR = mean(HUMEDAD_REL, na.rm = T),
            Pluvial  = max(PLUVIO)) %>% 
  dummy_cols(select_columns = c('CLASE'),
             remove_selected_columns = T)

#=======================================
#               Modelacion
#=======================================

#============================
#          XGBoost
#============================
param1 <- list(booster = "gblinear", nthread = 100, eta = 0.1,
               gamma = 0, max = 10, min_child_weight = 1,
               max_delta_step = 2, lambda = 1,
               alpha = 0, three_method = "exact",
               objective = "reg:squarederror",
               eval_metric = "rmse")  # Parametros del Boosting

#-----------------
# XGboost: Mensual
#-----------------
dtrain <- xgb.DMatrix(as.matrix(Mensual[,-c(2)]),label = Mensual$cantidad)
dtest <- xgb.DMatrix(as.matrix(MensualTs[,-c(2)]),label = MensualTs$cantidad)

watchlist <- list(train=dtrain,test=dtest)

ModMesXGboost <- xgb.train(data = dtrain,nrounds = 200,
                           watchlist = watchlist,params = param1,
                           max.depth=2)

plot(ModMesXGboost$evaluation_log$iter,ModMesXGboost$evaluation_log$train_rmse,
     col = 'red', type = 'l', xlab = 'Iteraciones',ylab = 'RMSE', 
     main = 'Entrenamiento & validacion \n con XGboost',
     lwd =3, sub = 'Nivel Mensual'); grid()
points(ModMesXGboost$evaluation_log$iter, ModMesXGboost$evaluation_log$test_rmse,
       type = 'l', col = 'blue',lwd = 3)


#------------------
# XGboost: Semanal
#------------------
dtrain <- xgb.DMatrix(as.matrix(Semanal[,-c(2)]),label = Semanal$cantidad)
dtest <- xgb.DMatrix(as.matrix(SemanalTs[,-c(2)]),label = SemanalTs$cantidad)

watchlist <- list(train=dtrain,test=dtest)

ModSemXGboost <- xgb.train(data = dtrain,nrounds = 100,
                           watchlist = watchlist,params = param1,
                           max.depth=2)

plot(ModSemXGboost$evaluation_log$iter,ModSemXGboost$evaluation_log$train_rmse,
     col = 'red', type = 'l', xlab = 'Iteraciones',ylab = 'RMSE', 
     main = 'Entrenamiento & validacion \n con XGboost',
     sub = 'Nivel Semanal',lwd =3); grid()
points(ModSemXGboost$evaluation_log$iter, ModSemXGboost$evaluation_log$test_rmse,
       type = 'l', col = 'blue',lwd = 3)

#-----------------
# XGboost: Diario
#-----------------
dtrain <- xgb.DMatrix(as.matrix(Diario[,-c(2)]),label = Diario$cantidad)
dtest <- xgb.DMatrix(as.matrix(DiarioTs[,-c(2)]),label = DiarioTs$cantidad)

watchlist <- list(train=dtrain,test=dtest)

ModDiaXGboost <- xgb.train(data = dtrain,nrounds = 100,
                           watchlist = watchlist,params = param1,
                           max.depth=2)

plot(ModDiaXGboost$evaluation_log$iter,ModDiaXGboost$evaluation_log$train_rmse,
     col = 'red', type = 'l', xlab = 'Iteraciones',ylab = 'MSE', 
     main = 'Entrenamiento & validacion \n con XGboost',
     sub = 'Nivel Semanal',lwd =3); grid()
points(ModDiaXGboost$evaluation_log$iter, ModDiaXGboost$evaluation_log$test_rmse,
       type = 'l', col = 'blue',lwd = 3)

#------------------------
# Validacion MSE XGboost
#------------------------
require(Matrix)

MSEXGboostMes <- mean((predict(ModMesXGboost,
                               as(as.matrix(MensualTs[,-2]),"dgCMatrix"), type = "response")-MensualTs$cantidad)^2)

MSEXGboostSem <- mean((predict(ModSemXGboost,
                               as(as.matrix(SemanalTs[,-2]),"dgCMatrix"))-SemanalTs$cantidad)^2)

MSEXGboostDia <- mean((predict(ModDiaXGboost,
                               as(as.matrix(DiarioTs[,-2]),"dgCMatrix"))-DiarioTs$cantidad)^2)


#=========================================================================
#               Regresión lineal generalizada usando GAMLSS
#==========================================================================

#------------------
# RLGamlss: Mensual
#-------------------
ModMenGam <- gamlss(formula = cantidad~.,
                    sigma.formula = ~.,
                    data = Mensual,
                    family = EXP)
#-------------------
# RLGamlss: Semanal
#--------------------
ModSemGam <- gamlss(formula = cantidad~.,
                    sigma.formula = ~.,
                    data = na.omit(Semanal),
                    family = EXP)
#--------------------
# RLGamlss: Diario
#--------------------
ModDiaGam <- gamlss(formula = cantidad~.,
                    sigma.formula = ~.,
                    data = na.omit(Diario),
                    family = EXP)

#-------------------------------
# Validacion MSE para RLGamlss
#-------------------------------
MSEgamlssMes <- mean((predictAll(ModMenGam, MensualTs)$mu-MensualTs$cantidad)^2)

MSEgamlssSem <- mean((predictAll(ModSemGam, SemanalTs)$mu-SemanalTs$cantidad)^2)

MSEgamlssDia <- mean((predictAll(ModDiaGam, DiarioTs)$mu-DiarioTs$cantidad)^2)


#====================================
#         Random Forest
#====================================

#------------------------
# Random Forest: Mensual
#------------------------
ModMenRanger <- ranger(cantidad~., 
                       data = na.omit(Mensual),
                       num.trees = 350, mtry = 4)

predRFMes <- predict(ModMenRanger, na.omit(MensualTs))


#-----------------------
# Random Forest: Semanal
#-----------------------
ModMenRanger <- ranger(cantidad~., 
                       data = na.omit(Semanal),
                       num.trees = 350,mtry = 4)

predRFSem <- predict(ModMenRanger, na.omit(SemanalTs))

#-----------------------
# Random Forest: Diario
#-----------------------
ModMenRanger <- ranger(cantidad~., 
                       data = na.omit(Diario),
                       num.trees = 350,mtry = 4)

predRFDia <- predict(ModMenRanger, DiarioTs)


#---------------------------------
#  Validacion MSE Random Forest
#---------------------------------
MSERangerMes <- mean(((predRFMes$predictions)-MensualTs$cantidad)^2)

MSERangerSem <- mean(((predRFSem$predictions)-SemanalTs$cantidad)^2)

MSERangerDia <- mean(((predRFDia$predictions)-DiarioTs$cantidad)^2)

#========================================
#       K-Nearest Neighbor Regression
#========================================

#-----------------
#   KNN: Mensual
#-----------------
MSEKnnMes <- vector()
for(i in 1:50){
  ModMesKnn <- knn.reg(train = na.omit(Mensual[,-c(2,3)]),
                       test = na.omit(MensualTs[,-c(2,3)]),
                       y = Mensual$cantidad,k=i)
  
  MSEKnnMes[i] <- mean(((ModMesKnn$pred)-na.omit(MensualTs)$cantidad)^2)
}
plot(1:50,MSEKnnMes, type = 'l')
paste("el valor de K que minimisa el MSE es:",which.min(MSEKnnMes))

#-------------
# KNN: Semanal
#-------------
MSEKnnSem <- vector()
for(i in 1:50){
  ModSemKnn <- knn.reg(train = na.omit(Semanal[,-c(2,3)]),
                       test = na.omit(SemanalTs[,-c(2,3)]),
                       y = Semanal$cantidad,k=i)
  
  MSEKnnSem[i] <- mean(((ModSemKnn$pred)-na.omit(SemanalTs)$cantidad)^2)
}
plot(1:50,MSEKnnSem, type = 'l')
paste("el valor de K que minimisa el MSE es:",which.min(MSEKnnSem))

#-------------
# KNN: Diario
#-------------
MSEKnnDia <- vector()
for(i in 1:50){
  ModDiaKnn <- knn.reg(train = na.omit(Diario[,-c(2,3)]),
                       test = na.omit(DiarioTs[,-c(2,3)]),
                       y = Diario$cantidad,k=i)
  
  MSEKnnDia[i] <- mean(((ModDiaKnn$pred)-na.omit(DiarioTs)$cantidad)^2)
}
plot(1:50,MSEKnnDia, type = 'l')
paste("el valor de K que minimisa el MSE es:",which.min(MSEKnnDia))

#=====================================
#         Redes Neuronales
#=====================================

#------------
# RN: Mensual
#-------------
ModMenNN <- neuralnet(cantidad~.,data = na.omit(Mensual),
                      hidden = c(3,2))

#--------------
# RN: Semanal
#--------------
ModSemNN <- neuralnet(cantidad~.,data = na.omit(Semanal),
                      hidden = c(3,2))

#-------------
# RN: Diaria
#-------------
ModDiaNN <- neuralnet(cantidad~.,data = na.omit(Diario),
                      hidden = c(2),stepmax = 5000000)


#---------------------------------
# Validacion MSE Redes Neuronales
#---------------------------------
MSENNMes <- mean((predict(ModMenNN, na.omit(MensualTs))-na.omit(MensualTs)$cantidad)^2)
MSENNSem <- mean((predict(ModSemNN, na.omit(SemanalTs))-na.omit(SemanalTs)$cantidad)^2)
MSENNDia <- mean((predict(ModDiaNN, na.omit(DiarioTs))-na.omit(DiarioTs)$cantidad)^2)


plot.nnet(ModMenNN,  pos.col = "blue", neg.col = "orange")
plot.nnet(ModSemNN,  pos.col = "blue", neg.col = "orange")
plot.nnet(ModDiaNN,  pos.col = "blue", neg.col = "orange")

#======================================================
#                     CONCLUSIONES
#======================================================
MSE <- c(MSEXGboostMes,MSEXGboostSem,MSEXGboostDia,MSEgamlssMes,MSEgamlssSem,MSEgamlssDia,
         MSERangerMes,MSERangerSem,MSERangerDia,min(MSEKnnMes),min(MSEKnnSem),min(MSEKnnDia),
         MSENNMes,MSENNSem,MSENNDia)
Modelos <- c(rep('XGboost',3),rep('GAMLSS',3),rep('RandomForest',3),rep('Knn',3),rep('Red Neuronal',3))
Periodo <- c(rep(c('Mensual','Semanal','Diario'),5))

ggplot(data.frame(RMSE=sqrt(MSE),Modelos,Periodo), aes(fill=Periodo,y = RMSE,x=Modelos))+
  geom_bar(position = 'dodge',stat= 'identity')