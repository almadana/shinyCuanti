library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Tabla iris"),
  DTOutput("tablaIris")
)

server <- function(input, output, session) {
  output$tablaIris <- renderDT({
    datatable(iris)
  })
}

shinyApp(ui, server)
