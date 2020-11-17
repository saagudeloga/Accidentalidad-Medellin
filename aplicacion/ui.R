require(golem)
require(shinycustomloader)
require(bs4Dash)
require(shinyjs)
require(shiny)
require(shinyWidgets)
require(markdown)
# cargar libreria de mapas
library(leaflet, quietly = T)
library(sf, quietly = T)

### ---- carga de archivos ----
# vector de posibles accidentes
accidentes <- c("TODOS", "ATROPELLO", "CAIDA OCUPANTE", "CHOQUE", "INCENDIO", 
                "VOLCAMIENTO", "OTRO")

# mapa de barrios y comunas
barrios <- read_sf("Limite_Barrio_Vereda_Catastral.shp")
comunas <- c("TODAS", levels(factor(barrios$NOMBRE_COM)))
rm(barrios)


bs4DashPage(title = "VEHAPP", sidebar_collapsed = T,
            navbar = bs4DashNavbar(skin = "light", status = "dark"),
            sidebar = bs4DashSidebar(skin = "dark", status = "danger", title = "VEHAPP", brandColor = "danger", src = "Sest.png", elevation = 3, opacity = 0.8,
                                     bs4SidebarMenu(bs4SidebarMenuItem("Inicio",tabName = "Inicio",icon = "home"),
                                                    bs4SidebarMenuItem("Resumen",tabName = "Resumen",icon = "book-open"),
                                                    bs4SidebarMenuItem("Historico",tabName = "Historico",icon = "car-crash"),
                                                    bs4SidebarMenuItem("Agrupamiento",tabName = "Agrupamiento",icon = "sitemap"),
                                                    bs4SidebarMenuItem("Prediccion", tabName = "Prediccion", icon = "line-chart"),
                                                    bs4SidebarMenuItem("Video explicativo", tabName = "Video", icon = "video"))),
            body = bs4Dash::bs4DashBody(useShinyjs(), chooseSliderSkin("HTML5"),
                                        bs4TabItems(bs4TabItem(tabName = "Inicio", includeMarkdown("opening.md"),
                                                               fluidRow(column(4,bs4UserCard(title = "Carolina Vergara",
                                                                                             subtitle = "Estadistica",
                                                                                             status = "danger",
                                                                                             width = 12,
                                                                                             src = "Carolina.jpg",
                                                                                             bs4ListGroup(
                                                                                               width = 36,
                                                                                               height = 8,
                                                                                               bs4ListGroupItem(
                                                                                                 HTML(text = "<i class = 'fa fa-university'> Universidad Nacional de Colombia</i>"),
                                                                                                 type = NULL,
                                                                                                 src = "https://r-podcast.org"
                                                                                               ),
                                                                                               bs4ListGroupItem(
                                                                                                 HTML(text = "<i class = 'fa fa-github'>  Carolina </i>"),
                                                                                                 type = "action",
                                                                                                 src = "https://github.com/rpodcast"
                                                                                               )
                                                                                             ))),
                                                                        column(4,bs4UserCard(title = "Samuel Agudelo",
                                                                                             subtitle = "Estadistica",
                                                                                             status = "danger",
                                                                                             width = 12,
                                                                                             src = "Samuel3.jpg")),
                                                                        column(4, bs4UserCard(title = "Marlon Gaviria",
                                                                                              subtitle = "Estadistica",
                                                                                              status = "danger",
                                                                                              width = 12,
                                                                                              src = "Marlon.jpg")),
                                                                        column(4, bs4UserCard(title = "Nicolas Sarmiento",
                                                                                              subtitle = "Estadistica",
                                                                                              status = "danger",
                                                                                              width = 12,
                                                                                              src = "Nico.jpg")),
                                                                        column(4, bs4UserCard(title = "Federico Milotta",
                                                                                              subtitle = "Ingenieria de Sistemas",
                                                                                              status = "danger",
                                                                                              width = 12,
                                                                                              src = "Federico.jpg"))
                                                               )),
                                                    bs4TabItem(tabName = "Resumen", includeMarkdown("resumen.md")),
                                                    bs4TabItem(tabName = "Historico", sidebarLayout(sidebarPanel(width = 3,
                                                                                                      
                                                                                                      # prueba
                                                                                                      #tags$style(".well {background-color: #b3cde0;}"),
                                                                                                      
                                                                                                      # imagen de la app
                                                                                                      #img(src = 'Sest.png', height = "165px"),
                                                                                                      
                                                                                                      # titulo
                                                                                                      #titlePanel("Filtros:", windowTitle = "VehApp"),
                                                                                                      
                                                                                                      # explicacion del mapa
                                                                                                      textOutput("texto_mapa"),
                                                                                                      
                                                                                                      # seleccion de anio
                                                                                                      sliderInput("anio_Agrup",
                                                                                                                  "Periodos:",
                                                                                                                  min = 2014,
                                                                                                                  max = 2018,
                                                                                                                  value = c(2017, 2018)),
                                                                                                      
                                                                                                      # seleccion de tipo de accidentes
                                                                                                      selectInput(inputId = "comuna_Agrup",
                                                                                                                  label = "Comuna:",
                                                                                                                  choices = comunas),
                                                                                                      
                                                                                                      # seleccion de tipo de accidentes
                                                                                                      selectInput(inputId = "tipo_accidente_Agrup",
                                                                                                                  label = "Tipo de accidente:",
                                                                                                                  choices = accidentes),
                                                                                                      
                                                                                                      # version
                                                                                                      textOutput("version_mapa")
                                                                                                      
                                                    ), mainPanel(titlePanel(HTML("<center> Historico de Accidentes en Medellín </center>")),
                                                                 leafletOutput("Historico")))
                                                    ),
                                                    bs4TabItem(tabName = "Agrupamiento", sidebarLayout(sidebarPanel(width = 3,
                                                                                                                     #seleccion anio
                                                                                                                     sliderInput("anio_Agrup",
                                                                                                                                 "Peridos:",
                                                                                                                                 min = 2014,
                                                                                                                                 max = 2018,
                                                                                                                                 value = c(2017,2018)),
                                                                                                                     #Seccion comuna
                                                                                                                     selectInput(inputId = "comuna_Agrup",
                                                                                                                                 label = "Comuna:",
                                                                                                                                 choices = comunas),
                                                                                                                     selectInput(inputId = "tipo_accidente_Agrup",
                                                                                                                                 label = "Tipo de accidente:",
                                                                                                                                 choices = accidentes)),
                                                                                                        mainPanel(titlePanel("Agrupamiento"),
                                                                                                                  leafletOutput("Agrupamiento")))),
                                                    bs4TabItem(tabName = "Prediccion", sidebarLayout(sidebarPanel(width = 3,
                                                                                                                   selectInput(inputId = "Resolucion",
                                                                                                                               label = "Tipo Prediccion:",
                                                                                                                               choices = c("DIARIO" = "Diario",
                                                                                                                                           "SEMANAL" = "Semanal",
                                                                                                                                           "MENSUAL" = "Mensual")),
                                                                                                                   selectInput(inputId = "tipo_accidente",
                                                                                                                               label = "Tipo de accidente:",
                                                                                                                               choices = accidentes)),
                                                                                                      mainPanel(titlePanel("Pronosticos 2019 y Observados historicos"),
                                                                                                                plotlyOutput("Prediccion2", height = "240px", width = "820px"),
                                                                                                                div(align = "height",
                                                                                                                    class = "width",
                                                                                                                    plotlyOutput("Prediccion1", height = "240px", width = "820px"))))),
                                                    bs4TabItem(tabName = "Video",
                                                               HTML('<iframe width="813" height="457" src="https://www.youtube.com/embed/yK3SiymGsHs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')))))