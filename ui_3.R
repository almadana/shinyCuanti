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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Panel de análsis de datos de CUANTI"),
  fluidRow(
    column(4,
           wellPanel("Datos",
                     selectInput("selectorDatos","Elegir base de datos",
                                 c("SERCE"="serce","Tríada oscura"="triada","Latinobarómetro"="latinoBaro1","Censo Nacional de Psicólogos"="censo"),
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
                                   c("Ver datos"="ver","Descriptivo"="descriptivo","Histograma"="histograma",
                                     "Tabla de frecuencias univariada"="tablaF1","Tabla de frecuencias bivariada"="tablaF2",
                                     "Gráfico de barras"="gbar","Diagrama de caja"="boxplot","Gráfico de dispersión"="dispersion"))
    ),
    column(4,wellPanel("Variables",
                      conditionalPanel(
                        # use a server side condition
                        
                        condition = "output.dataSelected",
                        # placeholders will be replaced from the server
                        selectInput("var1", "Elegir variable 1", "placeholder 1"),
                        selectInput("var2", "Elegir variable 2", "placeholder 2")
                      )
              ),
          actionButton("goButton","Analizar!")
    )
    
    
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Tablas",
              conditionalPanel("output.anali == 'ver'",
                 dataTableOutput("dataframe"))
               ),
      tabPanel("Gráficas",
      # Sidebar with a slider input for number of bins 
      # sidebarLayout(
      conditionalPanel("output.anali == 'histograma'",
        sidebarPanel(
           sliderInput("bins",
                       "Número de bins:",
                       min = 1,
                       max = 50,
                       value = 30)
        )
      ), 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
      #)
      ,
      tabPanel("Inferencia")
    )
  )
))
)
