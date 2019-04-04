#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme="cuanti.css",
                  
  
  # Application title
  titlePanel("Panel de análsis de datos de CUANTI"),
  fluidRow(
    column(4,
           wellPanel("Datos",
                     selectInput("selectorDatos","Elegir base de datos",
                                 c("SERCE"="serce","Tríada oscura"="triada","Latinobarómetro"="latinoBaro1","Censo Nacional de Psicólogos"="censo","Encuesta estudiantes de cuanti"="encuestaCuanti"),
                                 selected = "")
                     )
           ),
    column(4, #panel de RESUMEN - Nombre de variables, tipo, y niveles (min-max)
           wellPanel("Resumen",
                     conditionalPanel(
                       condition = "output.dataSelected",
                       tableOutput("summaryTable")
                     )
           )
    )
  ),
  fluidRow(
    column(4,wellPanel("Análisis",
                       selectInput("analisis","Elegir análisis",
                                   c("Ver datos"="ver",
                                     "Descriptivo"="descriptivo",
                                     "Histograma"="histograma",
                                     "Tabla de frecuencias univariada"="tablaF1",
                                     "Tabla de frecuencias bivariada"="tablaF2",
                                     "Gráfico de barras"="gbar",
                                     "Diagrama de caja"="boxplot",
                                     "Gráfico de dispersión"="dispersion"))
    )),
    column(4,wellPanel("Variables",
                      conditionalPanel(
                        condition = "output.dataSelected",
                        # placeholders will be replaced from the server
                        selectInput("var1", "Elegir variable 1", "placeholder 1"),
                        selectInput("var2", "Elegir variable 2", NULL)
                      )
              ),
          actionButton("goButton","Analizar!")
    )
    
    
    ),
  sidebarLayout(
    sidebarPanel(
      # Sidebar with some panels
      conditionalPanel(condition="output.anali == 'histograma'",
        sliderInput("bins",
                       "Número de bins:",
                       min = 1,
                       max = 50,
                       value = 30)
      ), 
      conditionalPanel(condition="output.anali == 'dispersion'",
                       sliderInput("pointSize",
                                   "Tamaño de puntos:",
                                   min = 1,
                                   max = 10,
                                   value = 3)
      ),
      conditionalPanel("output.anali == 'gbar'",
                       radioButtons("stacked",
                                    "Barras:",
                                    c("apiladas","no-apiladas")),
                       radioButtons("leyenda",
                                    "Leyenda:",
                                    c("izquierda","derecha"))
      ),
      conditionalPanel("output.anali == 'tablaF2' || output.anali=='gbar'",
                       radioButtons("freq",
                                    "Frecuencias:",
                                    c("absolutas","porcentuales"))
      ),
      conditionalPanel(" output.anali == 'tablaF2'  && input.freq == 'porcentuales'",
                       radioButtons("margin",
                                    "Porcentajes:",
                                    c("por filas","por columnas"))
      )
    ), 
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition="output.anali == 'ver'",
                       dataTableOutput("dataframe")),
      
      conditionalPanel(condition="output.anali == 'tablaF1' || output.anali == 'tablaF2' || output.anali == 'descriptivo' ",
                       htmlOutput("laTabla")),
      
      
      conditionalPanel("output.anali == 'histograma' || output.anali == 'gbar' || output.anali == 'boxplot' || output.anali == 'dispersion'",
                       plotOutput("elPlot")
#                       textOutput("mensaje")
      )
      #panel absoluto fijo al fondo
    )

  ),
#  fluidRow(class="barraPie",
                                div(      style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE; height=250px",
                                          img(src='logoCSE.png', align = "left",width="10%"),
                                          HTML("Proyecto CSE 2018 Innovaciones educativas - Desarrollo: Á. Cabana - <a href='https:/github.com/almadana/shinyCuanti'>Repositorio Github</a> - Licencia GPL v4."),
                                          img(src='logoFPsi.png', align = "right",height="15%",width="15%")
                                )
                  
                  
 #               )
  )
)

