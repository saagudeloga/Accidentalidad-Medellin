require(golem)
require(shinycustomloader)
require(bs4Dash)
require(shinyjs)
require(shiny)
require(shinyWidgets)
require(markdown)
require(plotly)
require(htmltools)
require(htmlwidgets)
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
                                                    bs4SidebarMenuItem("Prediccion 2019", tabName = "Prediccion", icon = "bar-chart"),
                                                    bs4SidebarMenuItem("Prediccion Dia a Dia",tabName = "Prediccion1",icon = "line-chart"),
                                                    bs4SidebarMenuItem("Video explicativo", tabName = "Video", icon = "video"))),
            body = bs4Dash::bs4DashBody(useShinyjs(), chooseSliderSkin("HTML5"),
                                        bs4TabItems(bs4TabItem(tabName = "Inicio", includeMarkdown("opening.md"),
                                                               fluidRow(column(4,bs4UserCard(title = "Carolina Vergara Clavijo",
                                                                                             subtitle = "Estadistica",
                                                                                             status = "danger",
                                                                                             width = 12,
                                                                                             src = "C1.jpeg",
                                                                                             bs4ListGroup(
                                                                                               width = 36,
                                                                                               height = 8,
                                                                                               bs4ListGroupItem(
                                                                                                 HTML(text = "<i class = 'fa fa-university'> Universidad Nacional de Colombia</i>"),
                                                                                                 type = NULL,
                                                                                                 src = "https://r-podcast.org"
                                                                                               ),
                                                                                               bs4ListGroupItem(
                                                                                                 HTML(text = "<i class = 'fa fa-github'> carolina2808 </i>"),
                                                                                                 type = "action",
                                                                                                 src = "https://github.com/carolina2808"
                                                                                               )
                                                                                             ))),
                                                                        column(4,bs4UserCard(title = "Samuel Agudelo Gamboa",
                                                                                             subtitle = "Estadistica",
                                                                                             status = "danger",
                                                                                             width = 12,
                                                                                             src = "S1.jpeg",
                                                                                             bs4ListGroup(
                                                                                               width = 36,
                                                                                               height = 8,
                                                                                               bs4ListGroupItem(
                                                                                                 HTML(text = "<i class = 'fa fa-university'> Universidad Nacional de Colombia</i>"),
                                                                                                 type = NULL,
                                                                                                 src = "https://r-podcast.org"
                                                                                               ),
                                                                                               bs4ListGroupItem(
                                                                                                 HTML(text = "<i class = 'fa fa-github'>  saagudeloga </i>"),
                                                                                                 type = "action",
                                                                                                 src = "https://github.com/saagudeloga"
                                                                                               )
                                                                                               ))),
                                                                        column(4, bs4UserCard(title = "Marlon Gaviria Perez",
                                                                                              subtitle = "Estadistica",
                                                                                              status = "danger",
                                                                                              width = 12,
                                                                                              src = "M1.jpeg",
                                                                                              bs4ListGroup(
                                                                                                width = 36,
                                                                                                height = 8,
                                                                                                bs4ListGroupItem(
                                                                                                  HTML(text = "<i class = 'fa fa-university'> Universidad Nacional de Colombia</i>"),
                                                                                                  type = NULL,
                                                                                                  src = "https://r-podcast.org"
                                                                                                ),
                                                                                                bs4ListGroupItem(
                                                                                                  HTML(text = "<i class = 'fa fa-github'>  MarlonGaviria </i>"),
                                                                                                  type = "action",
                                                                                                  src = "https://github.com/MarlonGaviria"
                                                                                                )
                                                                                                ))),
                                                                        column(4, bs4UserCard(title = "Nicolas Sarmiento Garcia",
                                                                                              subtitle = "Estadistica",
                                                                                              status = "danger",
                                                                                              width = 12,
                                                                                              src = "N1.jpeg",
                                                                                              bs4ListGroup(
                                                                                                width = 36,
                                                                                                height = 8,
                                                                                                bs4ListGroupItem(
                                                                                                  HTML(text = "<i class = 'fa fa-university'> Universidad Nacional de Colombia</i>"),
                                                                                                  type = NULL,
                                                                                                  src = ""
                                                                                                ),
                                                                                                bs4ListGroupItem(
                                                                                                  HTML(text = "<i class = 'fa fa-github'>  nsarmientog </i>"),
                                                                                                  type = "action",
                                                                                                  src = "https://github.com/nsarmientog"
                                                                                                )
                                                                                                ))),
                                                                        column(4, bs4UserCard(title = "Federico Milotta",
                                                                                              subtitle = "Ingenieria de Sistemas",
                                                                                              status = "danger",
                                                                                              width = 12,
                                                                                              src = "F1.jpeg",
                                                                                              bs4ListGroup(
                                                                                                width = 36,
                                                                                                height = 8,
                                                                                                bs4ListGroupItem(
                                                                                                  HTML(text = "<i class = 'fa fa-university'> University of Milan, Italy</i>"),
                                                                                                  type = NULL,
                                                                                                  src = ""
                                                                                                ),
                                                                                                bs4ListGroupItem(
                                                                                                  HTML(text = "<i class = 'fa fa-github'> FedericoMilotta </i>"),
                                                                                                  type = "action",
                                                                                                  src = "https://github.com/FedericoMilotta"
                                                                                                )
                                                                                                )))
                                                               )),
                                                    bs4TabItem(tabName = "Resumen", includeMarkdown("resumen.md")),
                                                    bs4TabItem(tabName = "Historico", sidebarLayout(sidebarPanel(width = 3,
                                                                                                      
                                                                                                      # titulo
                                                                                                      titlePanel("Filtros:", windowTitle = "VehApp"),
                                                                                                      
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
                                                                                                      textOutput("version_historico")
                                                                                                      
                                                    ), mainPanel(titlePanel(HTML("<center> Historico de Accidentes en Medellín </center>")),
                                                                 leafletOutput("Historico")))
                                                    ),
                                                    bs4TabItem(tabName = "Agrupamiento", sidebarLayout(sidebarPanel(width = 3,
                                                                                                                    
                                                                                                                    # titulo
                                                                                                                    titlePanel("Filtros:", windowTitle = "VehApp"),
                                                                                                                    
                                                                                                                     #Seccion comuna
                                                                                                                     selectInput(inputId = "Juanita",
                                                                                                                                 label = "Comuna:",
                                                                                                                                 choices = comunas),
                                                                                                                    
                                                                                                                    # explicacion del mapa
                                                                                                                    textOutput("texto_Agrupamiento")),
                                                                                                        mainPanel(titlePanel(HTML("<center> Agrupamiento de barrios</center>")),
                                                                                                                  leafletOutput("Agrupamiento"),
                                                                                                                  textOutput("Texto_explicacion")))),
                                                    bs4TabItem(tabName = "Prediccion", sidebarLayout(sidebarPanel(width = 3,
                                                                                                                  # titulo
                                                                                                                  titlePanel("Filtros:", windowTitle = "VehApp"),
                                                                                                                  
                                                                                                                   selectInput(inputId = "Resolucion",
                                                                                                                               label = "Tipo Prediccion:",
                                                                                                                               choices = c("DIARIO" = "Diario",
                                                                                                                                           "SEMANAL" = "Semanal",
                                                                                                                                           "MENSUAL" = "Mensual")),
                                                                                                                   selectInput(inputId = "tipo_accidente",
                                                                                                                               label = "Tipo de accidente:",
                                                                                                                               choices = accidentes),
                                                                                                                  # explicacion del mapa
                                                                                                                  textOutput("texto_Prediccion")),
                                                                                                      mainPanel(titlePanel("Pronosticos 2019 y Observados historicos"),
                                                                                                                plotlyOutput("Prediccion2", height = "240px", width = "820px"),
                                                                                                                div(align = "height",
                                                                                                                    class = "width",
                                                                                                                    plotlyOutput("Prediccion1", height = "240px", width = "820px"))))),
                                                    bs4TabItem(tabName = "Prediccion1", sidebarLayout(sidebarPanel(width = 3,
                                                                                                                   # titulo
                                                                                                                   titlePanel("Filtros:", windowTitle = "VehApp"),
                                                                                                                   dateRangeInput("Fechas",
                                                                                                                                  label = "Ingrese rango de fecha:",
                                                                                                                                  start = as.Date("2019-01-01"),
                                                                                                                                  end = as.Date("2019-12-31")),
                                                                                                                   selectInput(inputId = "clase_acci",
                                                                                                                               label = "Tipo de accidente:",
                                                                                                                               choices = accidentes),
                                                                                                                   # explicacion del mapa
                                                                                                                   textOutput("texto_Prediccion1")),
                                                                                                      mainPanel(titlePanel("Pronosticos Dia a Dia 2019"),
                                                                                                                plotlyOutput("Prediccion3", height = "270px",width = "820px"),
                                                                                                                div(align = "height",
                                                                                                                    plotlyOutput("Prediccion4", height = "240px",width = "820px"))))),
                                                    bs4TabItem(tabName = "Video",
                                                               HTML('<iframe width="870" height="470" src="https://www.youtube.com/embed/zthqqr8NfsQ?feature=oembed&amp" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')))))