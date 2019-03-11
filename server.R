#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(htmlTable)
load("./data/censo.RData")
load("./data/darkTriad.RData")
load("./data/serce.RData")


#guambia
currentDataset = censoFil



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  #"serce","Tríada oscura"="triada","Latinobarómetro"="latinoBaro1","Censo Nacional de Psicólogos"="censo"),
  listaDeDatos = list("censo"=censoFil,"triada"=dt,"serce"=serce,"latinoBaro1"="dt")
  
    dataSet <- eventReactive(input$selectorDatos,{
      
        laData = input$selectorDatos
        listaDeDatos[[laData]]
    
  })
    
    # var1 <- eventReactive(input$var1, {
    #   x=dataSet()
    #   x[,input$var1]
    # })
    # 
    data <- eventReactive(input$goButton,{
      x=dataSet()
      if (!is.null(input$var1)) {
        v1=x[,input$var1]
        v1name=input$var1
      }
      else {
        v1=NULL
        v1name=NULL
      }
      if (input$var2!="") {
        #print(input$var2)
        v2=x[,input$var2]
        v2name=input$var2
      }
      else {
        v2=NULL
        v2name=NULL
      }
      anali=input$analisis
#      print(anali)
      # 1- var1, 2-var2, 3-var1name, 4-var2name, 5-anali, 6-data
      list(v1,v2,v1name,v2name,anali,x)
    }
    )
    
    output$anali <- eventReactive(input$goButton,{
      input$analisis
    })
    outputOptions(output, "anali", suspendWhenHidden = FALSE)
    
    struct <- eventReactive(dataSet(),{
      a=unlist(lapply(dataSet(),class))
      b=(lapply(dataSet(),levels))
      numericas=unlist(lapply(b,is.null))
      list(a,b,numericas)
    })
    
    
    
    # var2 <- eventReactive(input$var2, {
    #   x=dataSet()
    #   x[,input$var2]
    #   
    # })
    
    
    # actualizar valores de
    observeEvent(dataSet(), {
      #esto debería ser algo con dataSet
      varsCDs=colnames(dataSet())
      updateSelectInput(session, "var1", choices = varsCDs)
      #updateSelectInput(session, "var2", choices = c(varsCDs[!(varsCDs %in% input$var1)] ))
    })
    
    # observeEvent(input$var1,{
    #   varsCDs=colnames(dataSet())
    #   updateSelectInput(session, "var2", choices = c(varsCDs[!(varsCDs %in% input$var1)] ))
    # }) 
    # 
    #actualizar la lista de valores posibles de var2
#    c("Ver datos"="ver","Descriptivo"="descriptivo","Histograma"="histograma",
 #     "Tabla de frecuencias univariada"="tablaF1","Tabla de frecuencias bivariada"="tablaF2",
#    "Gráfico de barras"="gbar","Diagrama de caja"="boxplot","Gráfico de dispersión"="dispersion"))

    observeEvent(input$analisis,{
      updateVariableInputs()
    })
    
    updateVariableInputs <- function(){
      varsCDs=colnames(dataSet())
      nums=struct()[[3]] #variables numericas
      todas=!logical(length(nums))
      elegibles1=switch(input$analisis,"ver"=NULL,"descriptivo"=NULL,"histograma"=nums,
                        "tablaF1"=!nums,"tablaF2"=!nums,"gbar"=!nums,"boxplot"=nums,"dispersion"=nums)    
      elegibles2=switch(input$analisis,"ver"=NULL,"descriptivo"=NULL,"histograma"=NULL,
                        "tablaF1"=NULL,"tablaF2"=!nums,"gbar"=!nums,"boxplot"=todas,"dispersion"=nums)    
      
      updateSelectInput(session, "var1", choices = varsCDs[elegibles1])
      updateSelectInput(session, "var2", choices = c(varsCDs[elegibles2] ))
    }
    
    # actualizar tabla de sumario
    output$summaryTable <- renderTable({
      a=struct()[[1]]
      b=struct()[[2]]
      numericas=struct()[[3]]
      a[a=="numeric"] = "numérica"
      a[a=="factor"] = "categórica"
      mins = (lapply(dataSet()[,numericas],min,na.rm=T))
      max = (lapply(dataSet()[,numericas],max,na.rm=T))
      
      niveles=unlist(lapply(b,function(x) if (!is.null(x)) { paste(x,collapse = ", ") }))
      
      minMax = paste(mins,max,sep="-")
      
      sumario = data.frame(Variable=names(a),Tipo.de.variable=a)
      sumario[!numericas,"Valores"]=niveles
      sumario[numericas,"Valores"]=minMax
      sumario
    },spacing="xs")
  
    #
    #
    # poner visible segundo panel o no
    output$dataSelected <- eventReactive(dataSet,{
      #print(dataSet)
      return(!is.null(dataSet()))
    })
    outputOptions(output, 'dataSelected', suspendWhenHidden=FALSE)


    
    #esto determina la función de plote que se va a llamar en renderPlot()  
    funcionDePloteo <-eventReactive(input$goButton,{
      switch(input$analisis,
             "histograma"=hacerHistograma,
             "boxplot"=hacerBoxplot,
             "gbar"=hacerGrafBarras,
             "dispersion"=hacerDispersion,
             noPlot) 
    }) 

  #determina qué función de table se usa en renderTable()
  funcionDeTabla <-eventReactive(input$goButton,{
      switch(input$analisis,
             "tablaF1"=hacerTablaF1,
             "tablaF2"=hacerTablaF2,
             "descriptivo"=hacerTablaDescriptiva,
             noPlot)
    }) 
    
  hacerTablaF1 <- function() {
    x=data()[[1]]
    n=data()[[3]]
    a=table(x)
    b=prop.table(a)
    a=addmargins(a)
    b=addmargins(b)
    d=rbind(a,b)
    colnames(d)[ncol(d)]="Total"
    d[2,]=d[2,]*100
    d=round(d,2)
    names(attributes(d)$dimnames)<-c("",n)
    rownames(d)=c("Frecuencia absoluta","Frecuencia Porcentual")
    htmlTable(d)
  }
  
  hacerTablaF2 <- function() {
    x1=data()[[1]]
    n1=data()[[3]]
    x2=data()[[2]]
    n2=data()[[4]]
    a=table(x1,x2,dnn=c(n1,n2))
    if (input$freq == "porcentuales") {
      margin = switch(input$margin,"por filas"=1,"por columnas"=2)
      a=prop.table(a,margin)
      if (margin==1) {
        a=addmargins(a,2)
        colnames(a)[ncol(a)] = "Total"
      }
      else {
        a=addmargins(a,1)
        rownames(a)[nrow(a)] = "Total"
      }
      a=a*100
    }
    # colnames(a)[ncol(a)]="Total"
    # colnames(a)[ncol(a)]="Total"
    #names(attributes(d)$dimnames)<-c("",n)
    #rownames(d)=c("Frecuencia absoluta","Frecuencia Porcentual")
    htmlTable(round(a,2))
  }
  
  
  
          
    #determina qué gráfica, tabla o análisis a mostrar
  #funciones de gráficas
    hacerHistograma = function() {
      #print(data())
      x = data()[[1]]
      nombre = data()[[3]]
      # generate bins based on input$bins from ui.R
      bins <- seq(min(x,na.rm=T), max(x,na.rm=T), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      titulo = paste("Histograma de",input$var1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white',xlab = nombre,ylab="Frecuencia",main="Histograma")
      
    }

    hacerDispersion = function() {
      x = data()[[1]]
      y = data()[[2]]
      nombre.x = data()[[3]]
      nombre.y = data()[[4]]
      
      pointSize = input$pointSize
      
      # draw the histogram with the specified number of bins
      #titulo = paste("Histograma de",input$var1)
      plot(x, y,cex = pointSize,xlab = nombre.x,ylab=nombre.y,main="")
      
    }
    
    
        
    hacerGrafBarras = function() {
    if (input$var2==input$var1) {
      nombre=data()[[3]]
      barplot(table(data()[[1]]),main=nombre)
    }
    else {
      nombre1=data()[[3]]
      nombre2=data()[[4]]
      #print(nombre2)
      barplot(table(data()[[1]],data()[[2]]),main="Diagrama de caja",beside = input$stacked!="apiladas")
      position=switch(input$leyenda,"izquierda"="topleft","derecha"="topright")
      legend(position,levels(data()[[1]]),fill=gray.colors(length(levels(data()[[1]]))))
    }
  }
    
    hacerBoxplot= function() {
    if (input$var2==input$var1) {
      nombre=data()[[3]]
      boxplot(data()[[1]],main=paste("Diagrama de caja de",nombre))
    }
    else {
      datos=data()[[6]]
      nombre1=data()[[3]]
      nombre2=data()[[4]]
      #print(nombre2)
      boxplot(formula(paste(nombre1,nombre2,sep="~")),data=datos,main="Diagrama de caja",xlab=nombre2,ylab=nombre1)
      
    }
  }
  
  
  #verDataFrame
  output$dataframe <- renderDataTable({
    # print("Bingo!")
    #print(str(data()[[6]]))
    # print(data()[[6]])
    data()[[6]]
  },searchDelay = 600)
  
  
  output$laTabla <- renderUI({
     HTML(funcionDeTabla()())
  })
  
  
  noPlot <- function(){}
  
  output$elPlot <- renderPlot({
    funcionDePloteo()()
  })
  
  # output$mensaje <- renderText({
  #   paste("Que pasa: !",input$analisis)
  # })
  
})
