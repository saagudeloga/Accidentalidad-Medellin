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

# Lectura de datos completo
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


# ===================================
# Graficos y estadistica descriptiva
#=====================================
require(ggplot2)

#===============
#     Dias
#===============
# Registros por d?a
medellin_dia <- medellin %>% group_by(DIA_NOMBRE) %>% summarize(total_accidentes = n())

# Ordenar de Lunes a Domingo
medellin_dia$DIA_NOMBRE <- ordered(medellin_dia$DIA_NOMBRE, 
                                   levels = c("LUNES","MARTES","MIERCOLES","JUEVES","VIERNES",
                                              "SABADO","DOMINGO"))

grafico_1 <- ggplot(data = medellin_dia, aes(x = DIA_NOMBRE, y = total_accidentes)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", color = "black") + 
  geom_text(aes(y = total_accidentes, label = total_accidentes),
            position = position_dodge(width = 0.8), size = 4, vjust = 1.2, hjust = 0.5, col = "gray10") +
  xlab("D?as") + 
  ylab("Total Accidentes") + 
  ggtitle("Total Accidentes por d?a de la semana") +
  theme_minimal()

#=================
#     Mes
#=================
nombre_mes = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sept","Oct","Nov","Dic")
med_mes <- table(medellin$MES)
names(med_mes) <- nombre_mes
med_mes <- as.data.frame(med_mes)

grafico_2 <- ggplot(data = med_mes, aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", col = "black") +
  geom_text(aes(y = Freq, label = Freq),
            position = position_dodge(width = 0.7), size = 3.5, vjust = 1.2, hjust = 0.5, col = "gray10") +
  xlab("Mes") +
  ylab("Total Accidentes") +
  ggtitle("Total Accidentes por mes") +
  theme_minimal()


#=================
#     Anio
#=================
med_Anio <- table(medellin$PERIODO) %>% as.data.frame()

grafico_3 <- ggplot(data = med_Anio, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", col = "black") + 
  geom_text(aes(y = Freq, label = Freq),
            position = position_dodge(width = 0.7), size = 3.5, vjust = 1.2, hjust = 0.5, col = "gray10") +
  xlab("A?o") +
  ylab("Total Accidentes") +
  ggtitle("Total Accidentes por a?o") + 
  theme_minimal()

#===================
#     Semana
#===================
med_sem <- medellin %>% group_by(semana=week(medellin$FECHA)) %>% 
  summarise(Total_Accidentes = n())

grafico_4 <- ggplot(data = med_sem, aes(x = semana, y = Total_Accidentes)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", col = "black") + 
  xlab("A?o") +
  ylab("Total Accidentes") +
  ggtitle("Total Accidentes por Semana") + 
  theme_minimal()

require(ggpubr)
ggarrange(grafico_3, grafico_2, grafico_4,grafico_1, ncol = 2, nrow = 2)

#===============
# Serie Temporal
#===============

# Serie temporal por dia

med_diaria <- medellin %>% group_by(FECHA) %>% summarize(total_accidentes = n())
med_diaria$Anio <- year(med_diaria$FECHA)
med_diaria$dia <- day(med_diaria$FECHA)
med_diaria$mes <- month(med_diaria$FECHA)

plot_ly(data = med_diaria, x = ~FECHA, y = ~total_accidentes,
        type = "scatter", mode = "lines", split = ~Anio,
        line = list(width = 1)) %>%
  layout(title = 'Registros de accidentalidad diarios 2014-2018',
        xaxis = list(title = "D?a"),
        yaxis = list(title = "N?mero de accidentes"))


# Promedio por mes
aggregate(total_accidentes~Anio*mes, data = med_diaria, FUN = mean) %>%
  plot_ly(x = ~mes,
          y = ~total_accidentes, type = "scatter", mode = "lines",
          split = ~Anio, line = list(width = 1.5)) %>%
  layout(title = 'Promedio diario mensual',
         xaxis = list(title = "Mes"),
         yaxis = list(title = "N?mero de accidentes"))


# Boxplot por dia de la semana
med_diaria$semana <- weekdays(med_diaria$FECHA)
med_diaria$semana <- factor(med_diaria$semana,
                             levels = c("lunes","martes","mi?rcoles","jueves","viernes","s?bado","domingo"))

plot_ly(data = med_diaria, x = ~semana, y = ~total_accidentes,
        type = "box")


#========================================
# Graficos para la variables categoricas
#========================================

# Accidentes por comuna
medellin_comuna <- medellin %>% group_by(COMUNA) %>%summarize(accidentes = n())

ggplot(data = medellin_comuna, aes(x = reorder(COMUNA,+accidentes), y = accidentes)) +
  geom_bar(stat = "identity", position = "dodge", fill = "grey24", color = "black", alpha = 0.6) +
  geom_text(aes(y = accidentes, label = accidentes),
                position = position_dodge(width = 0.7), size = 3.5, vjust = 0.5, hjust = -0.1, col = "black") +
  xlab("Comuna") + 
  ylab("Total accidentes") +
  ggtitle("Total accidentes por comuna 2014-2018") +
  ylim(c(0,50000)) +
  theme_minimal() +
  coord_flip()



# ------------ Gravedad del accidente
med_gravedad <- table(medellin$GRAVEDAD) %>% prop.table() %>% as.data.frame
med_gravedad$Var1 <- ordered(med_gravedad$Var1, levels = c("HERIDO","SOLO DAÑOS","MUERTO"))

grafico_5 <- ggplot(data = med_gravedad, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", color = "black") + 
  geom_text(aes(y = Freq, label = paste(round(Freq*100,1),"%")),
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5, hjust = 0.5, col = "gray10") +
  xlab("Gravedad") + 
  ylab("Frecuencia relativa (Accidentes)") + 
  ggtitle("Total Accidentes por gravedad") +
  theme_minimal()

# -------- Dise?o de la via en el accidente
med_diseno <- table(medellin$DISENO) %>% prop.table() %>% as.data.frame
med_diseno$Var1 <- ordered(med_diseno$Var1, levels = c("Via peatonal","Tunel","Ponton","Paso a Nivel","Paso Inferior","Puente","Ciclo Ruta",
                                                       "Paso Elevado","Otro","Glorieta","Lote o Predio","Interseccion","Tramo de via"))

grafico_6 <- ggplot(data = med_diseno, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", color = "black") + 
  geom_text(aes(y = Freq, label = paste(round(Freq*100,2),"%")),
            position = position_dodge(width = 0.8), size = 4, vjust = 0.5, hjust = -0.2, col = "gray10") +
  xlab("Diseño") + 
  ylab("Frecuencia relativa (Accidentes)") + 
  ggtitle("Total Accidentes por dise?o de la via") +
  theme_minimal() + 
  coord_flip() +
  ylim(0,0.8)

#------------ Clase Accidente
med_clase <- table(medellin$CLASE) %>% prop.table() %>% as.data.frame
med_clase$Var1 <- ordered(med_clase$Var1, levels = c("Choque","Otro","Atropello","Caida Ocupante","Volcamiento","Incendio","Choque y Atropello"))

grafico_7 <- ggplot(data = med_clase, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "firebrick", color = "black") + 
  geom_text(aes(y = Freq, label = paste(round(Freq*100,3),"%")),
            position = position_dodge(width = 0.8), size = 4, vjust = -0.5, hjust = 0.5, col = "gray10") +
  xlab("Clase") + 
  ylab("Frecuencia relativa (Accidentes)") + 
  ggtitle("Total Accidentes por Clase") +
  theme_minimal() + 
  ylim(0,0.8)



#------------- Fecha Especial
medellin$ESPECIAL_YN <- ifelse(medellin$ESPECIAL == "NO", "NO", "SI")
med_especial <- table(medellin$ESPECIAL_YN) %>% prop.table() %>% as.data.frame


# Gravedad y tipo de accidente en fechas especiales
especiales <- subset(medellin, ESPECIAL_YN == "SI")
esp_clase_gravedad <- table(especiales$CLASE, especiales$GRAVEDAD) %>% prop.table() %>% as.data.frame()
esp_clase_gravedad$Var1 <- ordered(esp_clase_gravedad$Var1, levels = c("Choque y Atropello","Incendio","Volcamiento","Caida Ocupante","Atropello","Otro","Choque"))


ggplot(data = esp_clase_gravedad, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = Freq, label = paste(round(Freq*100,1),"%")),
            position = position_dodge(width = 0.8), size = 3, vjust = 0.5, hjust = -0.2, col = "gray10") +
  theme_minimal() +
  ylim(0,0.5) +
  coord_flip()

#-------------- hORA PICO
med_pico <- table(medellin$ESTADO_HORA) %>% prop.table()

# Gravedad y tipo de accidentes en hora pico
hora_pico <- table(medellin$GRAVEDAD, medellin$ESTADO_HORA) %>% prop.table()


#=========== Bivariados

# Clase vs gravedad
med_clase_gravedad <- table(medellin$CLASE, medellin$GRAVEDAD) %>% prop.table() %>% as.data.frame()
med_clase_gravedad$Var1 <- ordered(med_clase_gravedad$Var1, levels = c("Choque y Atropello","Incendio","Volcamiento","Caida Ocupante","Atropello","Otro","Choque"))
names(med_clase_gravedad) <- c("Var1","Gravedad","Freq")

ggplot(data = med_clase_gravedad, aes(x = Var1, y = Freq, fill = Gravedad)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = Freq, label = paste(round(Freq*100,1),"%")),
            position = position_dodge(width = 0.8), size = 3, vjust = 0.5, hjust = -0.2, col = "gray10") +
  theme_minimal() +
  ylim(0,0.5) +
  xlab("Frecuencia %") +
  ylab("Clase de Accidente") +
  coord_flip()

# Gravedad por a?o
table(medellin$PERIODO, medellin$GRAVEDAD)



#================================================
#                     MAPA
#================================================
shape <- read_sf("Limite_Barrio_Vereda_Catastral.shp")
# Mapa para todos los barrios
nueva_base <- medellin %>% filter(PERIODO >= 2014 & PERIODO <= 2018) %>% 
  group_by(CB) %>%
  summarise(accidentes = n()) %>%
  ungroup()

nuevo_mapa <- inner_join(shape, nueva_base, by = c("CODIGO" = "CB"))

# Paleta de color
mypal <- colorNumeric(palette = c("#000000","#280100","#3D0201","#630201","#890100","#B00100","#DD0100","#F50201",
                                   "#FF5F5E","#FF7A79","#FF9796","#FEB1B0","#FDC9C8", "#FFE5E4"), domain = nuevo_mapa$accidentes, reverse = T)

# Crear mapa
leaflet() %>% addPolygons(data = nuevo_mapa, color = "#0A0A0A", opacity = 0.6, weight = 1, fillColor = ~mypal(nuevo_mapa$accidentes),
                          fillOpacity = 0.6, label = ~NOMBRE_BAR,
                          highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = T, opacity = 1),
                          popup = paste("Barrio: ", nuevo_mapa$NOMBRE_BAR, "<br>", "Accidentes: ", nuevo_mapa$accidentes, "<br>")) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addLegend(position = "bottomright", pal = mypal, values = nuevo_mapa$accidentes, title = "Accidentes", opacity = 0.6)
