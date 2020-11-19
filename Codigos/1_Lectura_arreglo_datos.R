library(data.table)   # manejo de tablas
library(purrr)        # optimizacion de bucles
library(dplyr)        # manejo de tablas
library(plotly)       # graficos en html
library(tidyr)        # limpieza de datos
library(stringr)      # limpieza de texto
library(lubridate)    # limpieza de fechas
library(sf) 

## lectura de archivos

# lista archivos
setwd("Dato_cod")
archivos_datos <- list.files(pattern = ".csv", include.dirs = T, recursive = F)

# leer todos los archivos
datos <- map(archivos_datos, fread, sep = ",", encoding = "UTF-8", colClasses = "c")

# agregar archivos del 2014 a 2018
medellin <- bind_rows(datos)

# ver cabecera del archivo
head(medellin)

## Limpieza de datos

# Organizar fecha
medellin$FECHA <- as.Date(medellin$FECHA) # Formato YYYY-MM-DD

# Eliminar datos vacios de la variable CLASE y correccion de niveles
medellin <- medellin[-which(medellin$CLASE == ""),]
medellin$CLASE <- iconv(medellin$CLASE, from = "UTF-8", to = "ASCII//TRANSLIT") # Correcion tildes
medellin$CLASE <- gsub(pattern = "Caida de Ocupante", replacement = "Caida Ocupante", x = medellin$CLASE) # Quitar DE

# Cambiar datos vacios de la variable DISENO y correccion de niveles
medellin$DISENO <- ifelse(medellin$DISENO == "","Otro",medellin$DISENO) #Cambiar nivel vacio por otro
medellin$DISENO <- iconv(medellin$DISENO, from = "UTF-8",to="ASCII//TRANSLIT") #Quitar tildes

# Quitar tildes a nombres de dias
medellin$DIA_NOMBRE <- iconv(medellin$DIA_NOMBRE, from = "UTF-8", to = "ASCII//TRANSLIT")

#==========================
# ARREGLOS COMUNA Y BARRIO
#===========================
catastro <- read_sf("Limite_Barrio_Vereda_Catastral.shp")

# Crear un codigo desde CBML que deje solo comuna y barrio
medellin <- mutate(medellin, CB = str_sub(CBML,1,4))

#===================================================================================================================
### El CB trae el codigo de comuna y barrio, en los datos hay unos que no tienen este codigo o que
### lo tienen mal, si se detalla en ellas nos damos cuenta que tampoco tienen ni el nombre del barrio ni
### la comuna, la mayoria de ellos tienen las coordenadas de epm, con mas tiempo se puede detallar mas
### en las coordenadas y el mapa para registrar la ubicacion excacta de la comuna y el barrio
### con esto se pierden 19942 registros de accidentes repartidos en los 5 a?os de registros
sinCB <- medellin[which(medellin$CB %in% c("","10","0","11","16","50","60","AUC1","AUC2","Inst","SN01","7005")),]
conCB <- medellin[-which(medellin$CB %in% c("","10","0","11","16","50","60","AUC1","AUC2","Inst","SN01","7005")),]
conCB <- inner_join(conCB, select(catastro,CODIGO,NOMBRE_COM,NOMBRE_BAR),
                  by = c("CB" = "CODIGO"))
conCB <- conCB[!duplicated(conCB[,c("OBJECTID")])]
table(sinCB$PERIODO)
#========================================================================================================================

# ingresar las varaibles de identificacion de lugar dadas por catastro en el archivo de datos
medellin <- inner_join(medellin, select(catastro, CODIGO, NOMBRE_COM, NOMBRE_BAR),
                        by = c("CB" = "CODIGO"))

# Quitar duplicados por el inner_join. Pasamos de 228687 registros a 208745 registros
medellin <- medellin[!duplicated(medellin[,c("OBJECTID")])]

# Quitar tildes y volver a mayuscula los nombres de barrios
medellin$NOMBRE_BAR <- iconv(toupper(medellin$NOMBRE_BAR), from = "UTF-8", to = "ASCII//TRANSLIT")

# Elegimos las columnas que son necesarias
medellin <- medellin[,c("X","Y","OBJECTID","FECHA","HORA","DIA","PERIODO","CLASE","GRAVEDAD",
                        "DISENO","DIA_NOMBRE","MES","CB","NOMBRE_COM","NOMBRE_BAR")]

# Cambiemos algunos nombres de las variables
names(medellin)[c(1,2,14,15)] <- c("LONGITUD","LATITUD","COMUNA","BARRIO")

# Asi quedan los datos
head(medellin)
str(medellin)

#==============================
# Ingreso de fechas especiales
#==============================
# Lectura registros de fechas especiales desde el 2014 a 2019
especiales <- read.csv("Otros/Fechas_especiales.csv", sep = ";", header = T)

# Convertir en formato YYYY-MM -DD
especiales$FECHA <- as.Date(especiales$FECHA, format = "%d/%m/%Y")

# unir las fechas especiales a medellin, los que nos son fechas especiales los pone como NA
medellin <- merge(x = medellin, y = especiales, by = "FECHA", all.x = T)
medellin$ESPECIAL <- ifelse(is.na(medellin$ESPECIAL),"NO","SI")

# Variable HORA_PICO
medellin$HORA <- hms(medellin$HORA)
medellin$HORA_PICO <- ifelse(hour(medellin$HORA) %in% c(5,6,7), "SI","NO")

#================================
#   Lectura de datos de clima
#================================
# Humedad Relativa
HUMEDAD <- read.csv('Otros/Datos IDEAM/HumedadRelativa.csv.csv')
HUMEDAD <- data.frame(FECHA=as.Date(HUMEDAD$Fecha),HUMEDAD_REL=HUMEDAD$Valor)

# Pluviometria
PLUV <- read.csv('Otros/Datos IDEAM/Pluviometria.csv.csv')
PLUV <- data.frame(FECHA=as.Date(PLUV$Fecha),PLUVIO = PLUV$Valor)
PLUV <- PLUV[!duplicated(PLUV[,c("FECHA")]),]

medellin <- merge(x = medellin, y = HUMEDAD, by = "FECHA", all.x = TRUE)
medellin <- merge(x = medellin, y = PLUV, by = "FECHA", all.x = TRUE)

# Tipo Variable
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


# Archivo definitivo
write.csv(medellin,"Otros/accidentalidad_medellin_final.csv",fileEncoding = "UTF-8")


