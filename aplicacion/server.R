library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    library(leaflet, quietly = T)
    library(sf, quietly = T)
    library(data.table, quietly = T)
    library(dplyr, quietly = T)
    library(shiny, quietly = T)
    library(lubridate, quietly = T)
    library(plotly, quietly = T)
    library(ggplot2, quietly = T)
    library(ggpubr, quietly = T)
    library(crosstalk, quietly = T)
    
    # Carga de datos
    medellin <- fread("Datos/accidentalidad_medellin_final.csv", sep = ",", encoding = "UTF-8", colClasses = "character")
    medellin$FECHA <- as.Date(medellin$FECHA, "%Y-%m-%d")
    medellin$CLASE <- toupper(medellin$CLASE)
    totales <- group_by(medellin, FECHA, PERIODO, DIA, MES, DIA_NOMBRE, ESPECIAL, CLASE) %>%
        summarize(total_accidentes = n()) %>%
        mutate(SEMANA = week(FECHA))
    
    # Shape barrios y comunas medellin
    shape <- read_sf("Limite_Barrio_Vereda_Catastral.shp")
    
    # Carga Datos predicciones
    Pronostico_2019 <- read.csv("Datos/Pronostico_2019.csv", encoding = "UTF-8")
    Pronostico_2019$Clase <- ifelse(Pronostico_2019$Clase == "atropello", "ATROPELLO",
                                   ifelse(Pronostico_2019$Clase == "caida_ocupante", "CAIDA OCUPANTE",
                                          ifelse(Pronostico_2019$Clase == "choque", "CHOQUE",
                                                 ifelse(Pronostico_2019$Clase == "incendio","INCENDIO",
                                                        ifelse(Pronostico_2019$Clase == "otro","OTRO",
                                                               ifelse(Pronostico_2019$Clase == "volcamiento","VOLCAMIENTO",0))))))

    # Ingresando visualizaciones en la aplicacion
    
    # Mapa Agrupamiento
    output$Historico <- renderLeaflet({
        # Variables que se tienen en cuenta
        tipo_accidente_agrup <- input$tipo_accidente_Agrup
        comuna_agrup <- input$comuna_Agrup
        
        # Se crea el mapa partiendo de las opciones que el usuario elija
        if(tipo_accidente_agrup == "TODOS" & comuna_agrup == "TODAS"){
            # Total accidentes por periodo y elegindo el tipo de accidente
            datos_mapa <- medellin %>% filter(PERIODO >= input$anio_Agrup[1] & PERIODO <= input$anio_Agrup[2]) %>%
                group_by(CB) %>%
                summarize(accidentes = n()) %>%
                ungroup()
            # Se une con el shape
            final_mapa <- inner_join(shape, datos_mapa, by = c("CODIGO" = "CB"))
        } else if(comuna_agrup == "TODAS"){
            datos_mapa <- medellin %>% filter(PERIODO >= input$anio_Agrup[1] & PERIODO <= input$anio_Agrup[2], CLASE == tipo_accidente_agrup) %>%
                group_by(CB) %>%
                summarize(accidente = n()) %>%
                ungroup()
            # Se une con el shape
            final_mapa <- inner_join(shape, datos_mapa, by = c("CODIGO" = "CB"))
        } else if(tipo_accidente_agrup == "TODOS"){
            # Total de accidentes por periodo y eligiendo la comuna
            datos_mapa <- medellin %>% filter(PERIODO >= input$anio_Agrup[1] & PERIODO <= input$anio_Agrup[2]) %>%
                group_by(CB) %>%
                summarize(accidentes = n()) %>%
                ungroup()
            # Se une con el shape
            final_mapa <- inner_join(shape, datos_mapa, by = c("CODIGO" = "CB")) %>%
                filter(NOMBRE_COM == comuna_agrup)
        } else{
            # Se filtra por el periodo, se agrupa por comuna y por tipo de accidente
            datos_mapa <- medellin %>% filter(PERIODO >= input$anio_Agrup[1] & PERIODO <= input$anio_Agrup[2], CLASE == tipo_accidente_agrup) %>%
                group_by(CB) %>%
                summarize(accidentes = n()) %>%
                ungroup()
            # Se une con shape
            final_mapa <- inner_join(shape, datos_mapa, by = c("CODIGO" = "CB")) %>%
                filter(NOMBRE_COM == comuna_agrup)
        }
        paleta <- colorNumeric(palette = c("#000000","#280100","#3D0201","#630201","#890100","#B00100","#DD0100","#F50201",
                                           "#FF5F5E","#FF7A79","#FF9796","#FEB1B0","#FDC9C8", "#FFE5E4"), domain = final_mapa$accidentes, reverse = T)
        
        # Dibujo del mapa
        leaflet() %>% addPolygons(data = final_mapa, color = "#0A0A0A", opacity = 0.5, weight = 1, 
                                  fillColor = ~paleta(final_mapa$accidentes), fillOpacity = 0.6,
                                  label = ~NOMBRE_BAR, highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = T, opacity = 1),
                                  popup = paste("Barrio: ", final_mapa$NOMBRE_BAR, "<br>", "Accidentes: ", final_mapa$accidentes, "<br>")) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addLegend(position = "bottomright", pal = paleta, values = final_mapa$accidentes, title = "Accidentes", opacity = 0.9)
    })
    
    # Prediccion
    output$Prediccion1 <- renderPlotly({
        # Variables que se tienen en cuenta
        tipo_accidente_agrup <- input$tipo_accidente
        resolucion_agrup <- input$Resolucion
        
        # GRAFICO 2014 - 2018
        if(tipo_accidente_agrup == "TODOS" & resolucion_agrup == "Diario"){
            med_Diario <- medellin %>% group_by(DIA) %>% summarize(total = n())
            
            G2 <- plot_ly(x = factor(med_Diario$DIA, levels = c("1":"31")), y = med_Diario$total,type = "bar",marker = list(color = rgb(0, 0, 0, 0.5))) %>%
                layout(title = "HISTORICO ACCIDENTES ENTRE 2014 Y 2018", xaxis = list(title = "Dia"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(tipo_accidente_agrup == "TODOS" & resolucion_agrup == "Semanal"){
            med_Semanal <- medellin %>% group_by(semana = week(FECHA)) %>% summarize(total = n())
            
            G2 <- plot_ly(x = factor(med_Semanal$semana), y = med_Semanal$total,type = "bar",marker = list(color = rgb(0, 0, 0, 0.5))) %>%
                layout(title = "HISTORICO ACCIDENTES ENTRE 2014 Y 2018", xaxis = list(title = "Semana"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(tipo_accidente_agrup == "TODOS" & resolucion_agrup == "Mensual"){
            med_Mensual <- medellin %>% group_by(MES) %>% summarize(total = n())
            med_Mensual$MES <- ifelse(med_Mensual$MES == 1, "Ene",
                                      ifelse(med_Mensual$MES == 2, "Feb",
                                             ifelse(med_Mensual$MES == 3, "Mar",
                                                    ifelse(med_Mensual$MES == 4, "Abr",
                                                           ifelse(med_Mensual$MES == 5,"May",
                                                                  ifelse(med_Mensual$MES == 6,"Jun",
                                                                         ifelse(med_Mensual$MES == 7,"Jul",
                                                                                ifelse(med_Mensual$MES == 8,"Ago",
                                                                                       ifelse(med_Mensual$MES == 9, "Sep",
                                                                                              ifelse(med_Mensual$MES == 10,"Oct",
                                                                                                     ifelse(med_Mensual$MES == 11,"Nov",
                                                                                                            ifelse(med_Mensual$MES == 12,"Dic",0))))))))))))
            
            med_Mensual$MES <- factor(med_Mensual$MES, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
            
            G2 <- plot_ly(x = med_Mensual$MES, y = med_Mensual$total,type = "bar", marker = list(color = rgb(0, 0, 0, 0.5))) %>%
                layout(title = "HISTORICO ACCIDENTES ENTRE 2014 Y 2018", xaxis = list(title = "Mes"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(resolucion_agrup == "Diario"){
            medp_ultimo <- subset(medellin, CLASE == tipo_accidente_agrup)
            med_ultimo <- medp_ultimo %>% group_by(DIA) %>% summarize(total = n())
            
            G2 <- plot_ly(x = med_ultimo$Dia, y = med_ultimo$total,type = "bar", marker = list(color = rgb(0, 0, 0, 0.5))) %>%
                layout(title = "HISTORICO ACCIDENTES ENTRE 2014 Y 2018", xaxis = list(title = "Dia"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(resolucion_agrup == "Semanal"){
            medp_ultimo <- subset(medellin, CLASE == tipo_accidente_agrup)
            med_ultimo <- medp_ultimo %>% group_by(semana = week(FECHA)) %>% summarize(total = n())
            
            G2 <- plot_ly(x = factor(med_ultimo$semana), y = med_ultimo$total, type = "bar", marker = list(color = rgb(0, 0, 0, 0.5))) %>%
                layout(title = "HISTORICO ACCIDENTES ENTRE 2014 Y 2018", xaxis = list(title = "Semana"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else {
            medp_ultimo <- subset(medellin, CLASE == tipo_accidente_agrup)
            med_ultimo <- medp_ultimo %>% group_by(MES) %>% summarize(total = n())
            med_ultimo$MES <- ifelse(med_ultimo$MES == 1, "Ene",
                                     ifelse(med_ultimo$MES == 2, "Feb",
                                            ifelse(med_ultimo$MES == 3, "Mar",
                                                   ifelse(med_ultimo$MES == 4, "Abr",
                                                          ifelse(med_ultimo$MES == 5,"May",
                                                                 ifelse(med_ultimo$MES == 6,"Jun",
                                                                        ifelse(med_ultimo$MES == 7,"Jul",
                                                                               ifelse(med_ultimo$MES == 8,"Ago",
                                                                                      ifelse(med_ultimo$MES == 9, "Sep",
                                                                                             ifelse(med_ultimo$MES == 10,"Oct",
                                                                                                    ifelse(med_ultimo$MES == 11,"Nov",
                                                                                                           ifelse(med_ultimo$MES == 12,"Dic",0))))))))))))
            
            med_ultimo$MES <- factor(med_ultimo$MES, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
            
            G2 <- plot_ly(x = med_ultimo$MES, y = med_ultimo$total, type = "bar", marker = list(color = rgb(0, 0, 0, 0.5))) %>%
                layout(title = "HISTORICO ACCIDENTES ENTRE 2014 Y 2018", xaxis = list(title = "Mes"), yaxis = list(title = "Cantidad Accidentes"))
        }
        G2
    })
    output$Prediccion2 <- renderPlotly({
        # Variables que se tienen en cuenta
        tipo_accidente_agrup <- input$tipo_accidente
        resolucion_agrup <- input$Resolucion
        
        # Se crea el mapa partiendo de las opciones que el usuario elija
        if(tipo_accidente_agrup == "TODOS" & resolucion_agrup == "Diario"){
            Pron_Diario <- subset(Pronostico_2019, Grupo_pro == "Diario")
            Acci_Diario <- Pron_Diario %>% group_by(Dia) %>% summarize(total = sum(Cantidad_Predicha))
            
            G1 <- plot_ly(x = factor(Acci_Diario$Dia), y = Acci_Diario$total,type = "bar",marker = list(color = rgb(1, 0, 0, 0.5))) %>%
                layout(title = "PREDICCION ACCIDENTES 2019", xaxis = list(title = "Dia"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(tipo_accidente_agrup == "TODOS" & resolucion_agrup == "Semanal"){
            Pron_Semanal <- subset(Pronostico2019, Grupo_pro == "Semanal")
            Acci_Semanal <- Pron_Semanal %>% group_by(Semana) %>% summarize(total = sum(Cantidad_Predicha))
            
            G1 <- plot_ly(x = factor(Acci_Semanal$Semana), y = Acci_Semanal$total,type = "bar",marker = list(color = rgb(1, 0, 0, 0.5))) %>%
                layout(title = "PREDICCION ACCIDENTES 2019", xaxis = list(title = "Semana"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(tipo_accidente_agrup == "TODOS" & resolucion_agrup == "Mensual"){
            Pron_Mensual <- subset(Pronostico2019, Grupo_pro == "Mensual")
            Acci_Mensual <- Pron_Mensual %>% group_by(Mes) %>% summarize(total = sum(Cantidad_Predicha))
            Acci_Mensual$Mes <- ifelse(Acci_Mensual$Mes == 1, "Ene",
                                       ifelse(Acci_Mensual$Mes == 2, "Feb",
                                              ifelse(Acci_Mensual$Mes == 3, "Mar",
                                                     ifelse(Acci_Mensual$Mes == 4, "Abr",
                                                            ifelse(Acci_Mensual$Mes == 5,"May",
                                                                   ifelse(Acci_Mensual$Mes == 6,"Jun",
                                                                          ifelse(Acci_Mensual$Mes == 7,"Jul",
                                                                                 ifelse(Acci_Mensual$Mes == 8,"Ago",
                                                                                        ifelse(Acci_Mensual$Mes == 9, "Sep",
                                                                                               ifelse(Acci_Mensual$Mes == 10,"Oct",
                                                                                                      ifelse(Acci_Mensual$Mes == 11,"Nov",
                                                                                                             ifelse(Acci_Mensual$Mes == 12,"Dic",0))))))))))))
            
            Acci_Mensual$Mes <- factor(Acci_Mensual$Mes, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
            
            G1 <- plot_ly(x = Acci_Mensual$Mes, y = Acci_Mensual$total,type = "bar", marker = list(color = rgb(1, 0, 0, 0.5))) %>%
                layout(title = "PREDICCION ACCIDENTES 2019", xaxis = list(title = "Mes"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(resolucion_agrup == "Diario"){
            Pron_ultimo <- subset(Pronostico_2019, Grupo_pro == resolucion_agrup & Clase == tipo_accidente_agrup)
            Acci_ultimo <- Pron_ultimo %>% group_by(Dia) %>% summarize(total = sum(Cantidad_Predicha))
            
            G1 <- plot_ly(x = Acci_ultimo$Dia, y = Acci_ultimo$total,type = "bar", marker = list(color = rgb(1, 0, 0, 0.5))) %>%
                layout(title = "PREDICCION ACCIDENTES 2019", xaxis = list(title = "Dia"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else if(resolucion_agrup == "Semanal"){
            Pron_ultimo <- subset(Pronostico_2019, Grupo_pro == resolucion_agrup & Clase == tipo_accidente_agrup)
            Acci_ultimo <- Pron_ultimo %>% group_by(Semana) %>% summarize(total = sum(Cantidad_Predicha))
            
            G1 <- plot_ly(x = Acci_ultimo$Semana, y = Acci_ultimo$total, type = "bar", marker = list(color = rgb(1, 0, 0, 0.5))) %>%
                layout(title = "PREDICCION ACCIDENTES 2019", xaxis = list(title = "Semana"), yaxis = list(title = "Cantidad Accidentes"))
            
        } else {
            Pron_ultimo <- subset(Pronostico_2019, Grupo_pro == resolucion_agrup & Clase == tipo_accidente_agrup)
            Acci_ultimo <- Pron_ultimo %>% group_by(Mes) %>% summarize(total = sum(Cantidad_Predicha))
            Acci_ultimo$Mes <- ifelse(Acci_ultimo$Mes == 1, "Ene",
                                       ifelse(Acci_ultimo$Mes == 2, "Feb",
                                              ifelse(Acci_ultimo$Mes == 3, "Mar",
                                                     ifelse(Acci_ultimo$Mes == 4, "Abr",
                                                            ifelse(Acci_ultimo$Mes == 5,"May",
                                                                   ifelse(Acci_ultimo$Mes == 6,"Jun",
                                                                          ifelse(Acci_ultimo$Mes == 7,"Jul",
                                                                                 ifelse(Acci_ultimo$Mes == 8,"Ago",
                                                                                        ifelse(Acci_ultimo$Mes == 9, "Sep",
                                                                                               ifelse(Acci_ultimo$Mes == 10,"Oct",
                                                                                                      ifelse(Acci_ultimo$Mes == 11,"Nov",
                                                                                                             ifelse(Acci_ultimo$Mes == 12,"Dic",0))))))))))))
            
            Acci_ultimo$Mes <- factor(Acci_ultimo$Mes, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
            
            G1 <- plot_ly(x = Acci_ultimo$Mes, y = Acci_ultimo$total, type = "bar", marker = list(color = rgb(1, 0, 0, 0.5))) %>%
                layout(title = "PREDICCION ACCIDENTES 2019", xaxis = list(title = "Mes"), yaxis = list(title = "Cantidad Accidentes"))
        }
        G1
    })
})
