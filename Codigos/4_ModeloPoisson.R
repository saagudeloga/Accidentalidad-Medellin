library(data.table)   # manejo de tablas
library(purrr)        # optimizacion de bucles
library(dplyr)        # manejo de tablas
library(plotly)       # graficos en html
library(tidyr)        # limpieza de datos
library(stringr)      # limpieza de texto
library(lubridate)    # limpieza de fechas
library(sf) 
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

#==============================
#  Imputación datos faltantes
#===============================
apply(apply(medellin, 2, is.na),2,sum)
medellin$HUMEDAD_REL[is.na(medellin$HUMEDAD_REL)] <- mean(medellin$HUMEDAD_REL, na.rm = T)
medellin$PLUVIO[is.na(medellin$PLUVIO)] <- 0

#================
# Puesta a punto
#=================
medellin$CLASE <- as.factor(medellin$CLASE)
levels(medellin$CLASE) <- c('Atropello','Caida_ocupante', 'Choque','Choque',
                         'Incendio','Otro','Volcamiento')

medellin$CLASE <- toupper(medellin$CLASE)

medellin$ESPECIAL <- ifelse(medellin$ESPECIAL == "SI",1,0)
#=======================================================
#   Elección conjunto de entrenamiento y validación
#=======================================================
Train <- medellin[medellin$PERIODO %in% c(2014,2015,2016,2017),]
Test <- medellin[medellin$PERIODO == 2018,]

dim(Train) #  accidentes en 1460 dias desde 2014 hasta 2017
dim(Test) # 40273 accidentes en 365 dias del 2018

#==============================
# Modelo Generalizado Poisson
#==============================
grupo_1 <- Train %>% group_by(FECHA, CLASE, DIA_NOMBRE, PERIODO, ESPECIAL) %>%
  summarise(Accidentes = n()) %>%
  arrange(FECHA)


# Modelo Poisson
modelo_poisson <- glm(Accidentes~FECHA+CLASE+DIA_NOMBRE+PERIODO+ESPECIAL,
                      data = grupo_1, family = "poisson")
summary(modelo_poisson)

# MSE para entrenamiento
prediccion_Train <- predict(modelo_poisson, type = "response")


MSE_Train <- mean((prediccion_Train - grupo_1$Accidentes)^2)

# MSE Validacion
grupo_validacion <- Test %>% group_by(FECHA, CLASE, DIA_NOMBRE,
                              PERIODO, ESPECIAL) %>%
  summarise(Accidentes = n()) %>%
  arrange(FECHA)

prediccion_Test <- predict(object = modelo_poisson, newdata = grupo_validacion[,-6], 
                           type = "response")
MSE_Test <- mean((prediccion_Test - grupo_validacion$Accidentes)^2)


print(c(MSE_Test,MSE_Train))

Variacion <- ((MSE_Train - MSE_Test)/(MSE_Test*2))*100


#===========================
# Prediccion 2019
#==========================
Datos_2019 <- read.csv("Otros/Predecir_2019.csv", sep = ",", encoding = "UTF-8")
Datos_2019 <- Datos_2019[,-1]
Datos_2019$CLASE <- gsub(pattern = "CAIDA OCUPANTE", replacement = "CAIDA_OCUPANTE", x = Datos_2019$CLASE)

Datos_2019$FECHA <- as.Date(Datos_2019$FECHA, format = "%Y-%m-%d")
Datos_2019$CLASE <- as.factor(Datos_2019$CLASE)
Datos_2019$DIA_NOMBRE <- as.factor(Datos_2019$DIA_NOMBRE)
Datos_2019$PERIODO <- as.integer(Datos_2019$PERIODO)

prediccion2019 <- predict(object = modelo_poisson, newdata = Datos_2019,
                          type = "response")
PREDICCIONES_DIARIAS_2019 <- Datos_2019 %>% 
  mutate(ACCIDENTES = round(prediccion2019,0))


write.csv(PREDICCIONES_DIARIAS_2019, "Predicciones_diarias_2019.csv")

