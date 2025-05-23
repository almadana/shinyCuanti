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
  shinyjs::useShinyjs(),
                  

  # Application title
  tags$h1("Panel de análisis de datos de CUANTI"),
  tabsetPanel(
    tabPanel("Análisis",
      fluidRow(
        column(3,
               wellPanel("Datos",
                         selectInput("selectorDatos","Elegir base de datos",
                                     c("ENDIS"="endis","SERCE"="serce","Encuesta mundial de valores"="wvs","Tríada oscura"="triada","Latinobarómetro"="latinoBaro1",
                                       "Censo Nacional de Psicólogos"="censo","Encuesta estudiantes de cuanti"="encuestaCuanti",
                                       "Mini-base de estudiantes de seminarios"="miniBase","Experimento: bebés y melodías"="expeCuna",
                                       "Experimento: audio vs. transcripción"="inteligencia",
                                       "Percepción de desigualdad y riqueza"="riqueza"),
                                     selected = "")
                         )
               ),
        column(9, #panel de RESUMEN - Nombre de variables, tipo, y niveles (min-max)
               
               wellPanel(
                    #mainPanel(
                      actionButton("toggleSidebar", "Mostrar resumen"),
                        div(id="tabla_resumen",style = "display: none;",
                          conditionalPanel(
                             condition = "output.dataSelected",
                            tableOutput("summaryTable")
                          )
                     #   )
                    )
               )
               
        )
      ),
      fluidRow(
        column(3,wellPanel("Análisis",
                           selectInput("analisis","Elegir análisis",
                                       c("Ver datos"="ver",
                                         "Descriptivo"="descriptivo",
                                         "Histograma"="histograma",
                                         "Tabla de frecuencias univariada"="tablaF1",
                                         "Tabla de frecuencias bivariada"="tablaF2",
                                         "Gráfico de barras"="gbar",
                                         "Diagrama de caja"="boxplot",
                                         "Intervalos de confianza para la media"="intconf",
                                         "Gráfico de dispersión"="dispersion")),
                           checkboxInput("muestrear",
                                         label="Tomar muestra aleatoria"
                                         ),
                           conditionalPanel(condition="output.condition",
                             sliderInput("sliderMuestra",
                                         "Tamaño de la muestra",
                                         min=1,
                                         max=100,
                                         value=10),
                            actionButton("remuestrear","Remuestrear")
                           )
            )),
        column(9,wellPanel("Variables",
                          conditionalPanel(
                            condition = "output.dataSelected",
                            # placeholders will be replaced from the server
                            selectInput("var1", "Elegir variable 1", "placeholder 1"),
                            selectInput("var2", "Elegir variable 2", NULL)
                          ),
                  actionButton("goButton","Analizar!")
            )
        )
      ),
      fluidRow(
      sidebarLayout(
        sidebarPanel(
          # Sidebar with some panels
          conditionalPanel(condition="output.anali == 'histograma'",
            sliderInput("bins",
                           "Número de bins:",
                           min = 1,
                           max = 50,
                           value = 30),
            checkboxInput("ajusteNormal",label="Ajustar curva normal")
          ), 
          conditionalPanel(condition="output.anali == 'dispersion'",
                           sliderInput("pointSize",
                                       "Tamaño de puntos:",
                                       min = 1,
                                       max = 10,
                                       value = 3),
                           checkboxInput("tendencia","Línea de tendencia y coef. de correlación"),
                           conditionalPanel(condition="input.tendencia",
              
                             tags$p(textOutput("coefCorrel1")),
                             tags$p(textOutput("pCorrel1"))
                           )
          ),
          conditionalPanel("output.anali == 'gbar'",
                           radioButtons("stacked",
                                        "Barras:",
                                        c("apiladas","no-apiladas")),
                           radioButtons("leyenda",
                                        "Leyenda:",
                                        c("izquierda","derecha"))
                           #conditionalPanel(condition="leyenda",
                            #                )
          ),
          conditionalPanel("output.anali == 'tablaF2'",
                           checkboxInput("chisq2",
                                        "Prueba Chi cuadrado de independencia:"),
                           conditionalPanel(condition="input.chisq2",
                                            tags$p(textOutput("chisq2Text"))
                           )
          ),
          conditionalPanel("output.anali == 'tablaF2' || output.anali=='gbar' ",
                           radioButtons("freq",
                                        "Frecuencias:",
                                        c("absolutas","porcentuales"))
          ),
          conditionalPanel(" output.anali == 'tablaF2'  && input.freq == 'porcentuales'",
                           radioButtons("margin",
                                        "Porcentajes:",
                                        c("por filas","por columnas"))
          ),      
          conditionalPanel("output.anali == 'boxplot'",
                                   checkboxInput("student",
                                                 "Prueba T de Student para las medias."),
                                   conditionalPanel(condition="input.student",
                                                    checkboxInput("pairedStudent","Muestras pareadas"),
                                                    tags$p(verbatimTextOutput("studentText"))
                                   )
          )
        ), 
        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel(condition="output.anali == 'ver'",
                           DT::DTOutput("dataframe")),
          
          conditionalPanel(condition="output.anali == 'tablaF1' || output.anali == 'tablaF2' || output.anali == 'descriptivo' ",
                           htmlOutput("laTabla")),
          
          
          conditionalPanel("output.anali == 'histograma' || output.anali == 'gbar' || output.anali == 'boxplot' || output.anali == 'intconf'|| output.anali == 'dispersion'",
                           plotOutput("elPlot")
    #                       textOutput("mensaje")
          )
          #panel absoluto fijo al fondo
        )
    
      ),
              div(class="barraPie",
                  
                  span(class="spanner",img(src='educacion-permanente.jpg', align = "left",class="logo"),
                       "Proyecto CSE 2018 Innovaciones educativas - Desarrollo: Á. Cabana - ",
                        a(href='https:/github.com/almadana/shinyCuanti',"Repositorio Github"," - Licencia GPL v4.")
                  )
              )
        )
      ),
      tabPanel("Distribuciones",
         fluidRow(
           column(8,
              wellPanel("Distribución",
                selectInput("selectorDist","Elegir distribución",
                  c("Binomial"="binomial","Normal"="normal","t de Student"="student","Chi cuadrado"="chisq"),
                  selected = "binomial"
                )
              )
           )
         ),
         fluidRow(
           sidebarLayout(
             sidebarPanel(
               conditionalPanel(condition='input.selectorDist == "binomial"',
                  sliderInput("pBin","Elija un valor de p",min=0.01,max=0.99,value=0.5,step = 0.01,round=FALSE),
                  sliderInput("nBin","Elija el número de intentos",min=2,max=100,value=10,step=1)
                  
               ),
               conditionalPanel(condition='input.selectorDist == "student" || input.selectorDist == "chisq"',
                                sliderInput("gLibertad","Elija los grados de libertad",min=2,max=100,value=10,step = 1,round=T)
                                
               ),
               checkboxInput("pvalor","Calcular p-valor"),
               conditionalPanel(condition='input.pvalor',
                                conditionalPanel(condition='input.selectorDist != "chisq"',
                                  radioButtons("colas","",choices = c("Una cola","Dos colas"))
                                ),
                                conditionalPanel(condition='input.selectorDist == "binomial"', 
                                  sliderInput("kBin","Número de intentos positivos",min=0,max=10,value=5,step=1)
                                ),
                                conditionalPanel(condition='input.selectorDist == "student"', 
                                                 sliderInput("tStudent","Valor del estadístico t",min=-5,max=5,value=0,step=.01)
                                ),
                                conditionalPanel(condition='input.selectorDist == "chisq"', 
                                                 sliderInput("chisq","Valor del estadístico chi cuadrado",min=0.1,max=50,value=0.1,step=.01)
                                ),
                                conditionalPanel(condition='input.selectorDist == "normal"', 
                                                 sliderInput("zNormal","Valor del estadístico z",min=-5,max=5,value=0,step=.01)
                                )
               )
             ),
             mainPanel(
               plotOutput("distriPlot"),
               htmlOutput("distriTable")
             )
           )
         )
      )
    )
  )
)

