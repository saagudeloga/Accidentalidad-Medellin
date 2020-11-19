library(data.table)   # manejo de tablas
library(purrr)        # optimizacion de bucles
library(dplyr)        # manejo de tablas
library(plotly)       # graficos en html
library(tidyr)        # limpieza de datos
library(stringr)      # limpieza de texto
library(lubridate)    # limpieza de fechas
library(sf) 
library(leaflet, quietly = T)
library(data.table, quietly = T)
library(dplyr, quietly = T)
library(shiny, quietly = T)
library(lubridate, quietly = T)
library(plotly, quietly = T)

setwd("Dato_cod")
medellin <- fread(file = "Otros/accidentalidad_medellin_final.csv", sep = ",",
                  encoding = "UTF-8", colClasses = "character")
str(medellin)
# Estructuracion variables
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
medellin$FECHA <- as.Date(medellin$FECHA)



agrupamiento <- medellin[,c("FECHA","BARRIO","COMUNA","PERIODO","CB","ESPECIAL","HORA_PICO",
                            "GRAVEDAD")]

data_agrupamiento <- agrupamiento %>% filter(PERIODO >= 2014 & PERIODO <= 2018) %>% 
  group_by(CB) %>%
  summarise(total_accidentes = n(), 
            Acc_Especiales = sum(ESPECIAL == "SI"),
            Acc_Hpico = sum(HORA_PICO == "SI"),
            Acc_Muerto = sum(GRAVEDAD == "MUERTO")) %>%
  ungroup()

#row.names(data_agrupamiento) <- nueva_base$CB


wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}
wssplot(data_agrupamiento[,-1], nc = 10)

# Kmeans
set.seed(1017239703)
barrios <- data_agrupamiento$CB
k.means.fit <- kmeans(data_agrupamiento[,-1], 5)
d <- k.means.fit$cluster



require(ggplot2)
require(tidyverse)
require(factoextra)

agrup_grafica <- data_agrupamiento[,-1]
row.names(agrup_grafica) <- barrios
fviz_cluster(object = k.means.fit, data = agrup_grafica,
             ellipse.type = "convex", repel = F, show.clust.cent = FALSE,
             labelsize = 8) +
  labs(title = "Clasificación Jerárquica Barrios Medellín",
       subtitle = "K Nearest Neighbor") +
  theme_bw() +
  theme(legend.position = "bottom")

data_agrupamiento$Grupos <- d


names(data_agrupamiento)

data_agrupamiento$Grupos <- ifelse(data_agrupamiento$Grupos == 1, "Grupo 1",
                                   ifelse(data_agrupamiento$Grupos == 2, "Grupo 2",
                                          ifelse(data_agrupamiento$Grupos == 3, "Grupo 3",
                                                 ifelse(data_agrupamiento$Grupos == 4, "Grupo 4",
                                                        ifelse(data_agrupamiento$Grupos == 5, "Grupo 5",0)))))


#=======================
#       Mapa
#=========================
shape <- read_sf("Limite_Barrio_Vereda_Catastral.shp")
# Mapa para todos los barrios
dato_mapa <- inner_join(shape, data_agrupamiento, by = c("CODIGO" = "CB"))


factpal <- colorFactor(rainbow(5), d)
write.csv(data_agrupamiento, "Agrupamiento_barrios.csv", fileEncoding = "UTF-8")


#================================
# Ensayo otro mapa
#=================================
thalia <- c("#FF0000", "#CCFF00", "#00FF66", "#0066FF", "#CC00FF")
dato_mapa$colores <- ifelse(dato_mapa$Grupos == "Grupo 1", "#FF0000",
                            ifelse(dato_mapa$Grupos == "Grupo 2", "#CCFF00",
                                   ifelse(dato_mapa$Grupos == "Grupo 3", "#00FF66",
                                          ifelse(dato_mapa$Grupos == "Grupo 4", "#0066FF",
                                                 ifelse(dato_mapa$Grupos == "Grupo 5","#CC00FF",0)))))



# Crear mapa
leaflet() %>% addPolygons(data = dato_mapa, opacity = 0.4, color = "#545454",weight = 1, fillColor = dato_mapa$colores,
                          fillOpacity = 0.4, label = ~NOMBRE_BAR,
                          highlightOptions = highlightOptions(color = "#262626", weight = 3, bringToFront = T, opacity = 1),
                          popup = paste("Barrio: ", dato_mapa$NOMBRE_BAR, "<br>", "Grupo: ", dato_mapa$Grupos)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addLegend(position = "bottomright", colors = thalia, labels = c("Grupo 1: ajadfklsd", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5"))


