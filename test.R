library(shiny)

ui <- fluidPage(
  selectInput("analisis", "Elegir análisis", choices = c("ver", "histograma")),
  sliderInput("bins", "Número de bins:", min = 1, max = 50, value = 30)
)

server <- function(input, output, session) {}

shinyApp(ui, server)
