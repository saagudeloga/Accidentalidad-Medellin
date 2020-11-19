# Pronostico 2019
require(dplyr)
setwd("Dato_cod")

Diario2019 <- read.csv("DiarioApp.csv",sep = ";", encoding = "UTF-8")
Semanal2019 <- read.csv("SemanalApp.csv",sep = ";", encoding = "UTF-8")
Mensual2019 <- read.csv("MensualApp.csv",sep = ";", encoding = "UTF-8")

Pronostico2019 <- bind_rows(Diario2019,Semanal2019,Mensual2019)
Pronostico2019 <- Pronostico2019[,c("Dia","Semana","Mes","Clase","Cantidad_real",
                                    "Cantidad_Predicha","Grupo_pro")]

Pronostico2019$Cantidad_Predicha <- ifelse(Pronostico2019$Cantidad_Predicha < 0, 
                                           (Pronostico2019$Cantidad_Predicha)*(-1), 
                                           Pronostico2019$Cantidad_Predicha)

Pronostico2019$Clase <- ifelse(Pronostico2019$Clase == "atropello", "ATROPELLO",
                               ifelse(Pronostico2019$Clase == "caida_ocupante", "CAIDA OCUPANTE",
                                      ifelse(Pronostico2019$Clase == "choque", "CHOQUE",
                                             ifelse(Pronostico2019$Clase == "incendio","INCENDIO",
                                                    ifelse(Pronostico2019$Clase == "otro","OTRO",
                                                           ifelse(Pronostico2019$Clase == "volcamiento","VOLCAMIENTO",0))))))


#write.csv(Pronostico2019, file = "Pronostico_2019.csv", fileEncoding = "UTF-8")
require(ggplot2)
#===================
#       Diario
#===================
Pron_Diario <- subset(Pronostico2019, Grupo_pro == "Diario")
Acci_Diario <- Pron_Diario %>% group_by(Dia) %>% summarize(total = sum(Cantidad_Predicha))

Pron_ultimo <- subset(Pronostico2019, Grupo_pro == "Diario" & Clase == "ATROPELLO")
Acci_ultimo <- Pron_ultimo %>% group_by(Dia) %>% summarize(total = sum(Cantidad_Predicha))

plot_ly(x = Acci_ultimo$Dia, y = Acci_Mensual$total,type = "bar", marker = list(color = c("blue")))


#======================
#     Semanal
#======================
Pron_Semanal <- subset(Pronostico2019, Grupo_pro == "Semanal")
Acci_Semanal <- Pron_Semanal %>% group_by(Semana) %>% summarize(total = sum(Cantidad_Predicha))

ggplot(data = Acci_Semanal, aes(x = Semana, y = total)) + 
  geom_bar(stat = "identity", alpha = 0.7, fill = "grey40", color = "black") +
  xlab("Dia") +
  ylab("Total Accidentes") +
  ggtitle("Accidentes semanales 2019") +
  theme_minimal()

#=======================
#     Mensual
#=======================
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


plot_ly(
  x = Acci_Mensual$Mes,
  y = Acci_Mensual$total,
  type = "bar",
  marker = list(color = c("red"))
)
