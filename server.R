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
library(dplyr)
load("./data/censo.RData")
load("./data/darkTriad.RData")
load("./data/serce.RData")
load("./data/latino.RData")
load("./data/encuesta.RData")


#guambia
currentDataset = censoFil



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  #"serce","Tríada oscura"="triada","Latinobarómetro"="latinoBaro1","Censo Nacional de Psicólogos"="censo"),
  listaDeDatos = list("censo"=censoFil,"triada"=dt,"serce"=serce,"latinoBaro1"=latino,"encuestaCuanti"=encuesta1)
  
    dataSet <- eventReactive(input$selectorDatos,{
      
        laData = input$selectorDatos
        listaDeDatos[[laData]]
    
  })
    

    #--- cargar data() ----
        data <- eventReactive(input$goButton,{
      x=dataSet()
      if (!is.null(input$var1)) {
        v1=pull(x,input$var1) #when data is tibble, this makes v1 a vector, not a 1-d tibble...
        v1name=input$var1
      }
      else {
        v1=NULL
        v1name=NULL
      }
      if (input$var2!="") {
        #print(input$var2)
        v2=pull(x,input$var2)
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
    

    #--- cargar output.anali ----
    output$anali <- eventReactive(input$goButton,{
      input$analisis
    })
    outputOptions(output, "anali", suspendWhenHidden = FALSE)
    
    
    
    #--- cargar struct() ----
    struct <- eventReactive(dataSet(),{
      a=unlist(lapply(dataSet(),class))
      b=(lapply(dataSet(),levels))
      numericas=unlist(lapply(b,is.null))
      list(a,b,numericas)
    })
    
    
    
     
    #--- actualizar valores de var-selector1 ---- fix: parece innecesario
    
    observeEvent(dataSet(), {
      varsCDs=colnames(dataSet())
      updateSelectInput(session, "var1", choices = varsCDs)
      updateSelectInput(session, "var2", choices = varsCDs)
    })
    
    #--- trigger de actualizador var-selector 1y2 ----
    observeEvent(input$analisis,{
      updateVariableInputs()
    })
    
    #--- actualizar valores de var-selector1y2 ----
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
    
    #---- actualizar tabla de sumario ----
    output$summaryTable <- renderTable({
      a=struct()[[1]]
      b=struct()[[2]]
      numericas=struct()[[3]]
      a[a=="numeric"] = "numérica"
      a[a=="factor"] = "categórica"
      mins = round((sapply(dataSet()[,numericas],min,na.rm=T)),2)
      max = round(sapply(dataSet()[,numericas],max,na.rm=T),2)
      
      niveles=unlist(lapply(b,function(x) if (!is.null(x)) { paste(x,collapse = ", ") }))
      niveles = sapply(niveles,function(x) {
        if (nchar(x)>50) {
          x=paste(substr(x,0,50),"...",sep="") }
        return(x)
      })
      
      etiquetas=unlist(lapply(dataSet(),function(x) {
        f=attributes(x)$label
        ifelse(is.null(f),"",f)}))
      minMax = paste(mins,max,sep="---")
      #print(etiquetas)
      sumario = data.frame(Variable=names(a),Descripcion=etiquetas,Tipo.de.variable=a)
      #print(sumario)
      #print(numericas)
      sumario[!numericas,"Valores"]=niveles
      sumario[numericas,"Valores"]=minMax
      sumario
    },spacing="xs")
  
    umbralRecorte = 50 #máximo número de caracteres para display a full de niveles de variables categóricas
    recortarCaracteres <- function(x) {
      if (nhcar(x)>umbralRecorte) {
        x=paste(substr(x,0,umbralRecorte),"...",sep="")
      }
      return(x)
    }
    
    
    
    
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
    d=round(d,1)
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
    htmlTable(round(a,1))
  }
  
  
  hacerTablaDescriptiva <- function(){
    datos = data()[[6]]
    numericas=struct()[[3]]
    medias=apply(datos[,numericas],2,mean,na.rm=T)
    desv=apply(datos[,numericas],2,sd,na.rm=T)
    qtiles= round(t(apply(datos[,numericas],2,quantile)),2)
    moda=apply(datos,2,getmode)
    sumario=data.frame(Variables=colnames(datos))
    sumario[numericas,"Media"] = round(medias,2)
    sumario[numericas,"Desvío"] = round(desv,2)
    sumario[numericas,"Cuartil 1"] = qtiles[,2]
    sumario[numericas,"Mediana"] = qtiles[,3]
    sumario[numericas,"Cuartil 3"] = qtiles[,4]
    sumario[,"Moda"] = moda
    htmlTable(sumario)
  }

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
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
      #print(x)
      #print(is.numeric(x))
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
      tab=table(data()[[1]])
      if (input$freq=="porcentuales") {
        tab=prop.table(tab)
        tab=tab*100
      } 
      barplot(tab,main=nombre)
    }
    else {
      nombre1=data()[[3]]
      nombre2=data()[[4]]
      #print(nombre2)
      tab=table(data()[[1]],data()[[2]])
      if (input$freq=="porcentuales") {
        tab=prop.table(tab,2)
        tab=tab*100
      } 
      barplot(tab,beside = input$stacked!="apiladas",xlab = nombre2)
      position=switch(input$leyenda,"izquierda"="topleft","derecha"="topright")
      legend(position,levels(data()[[1]]),fill=gray.colors(length(levels(data()[[1]]))))
      title(paste("Gráfico de",nombre1,"según",nombre2))
      
      #qplot(dt,aes_string(x=a,fill=b))+geom_bar(stat="identity",position = position_dodge())
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
     HTML(limpiaColspan(funcionDeTabla()()))
  })
  
  limpiaColspan <- function(x) {
    gsub("td colspan='[0-9]*'","td colspan='1'",x)
  }
  
  
  noPlot <- function(){}
  
  output$elPlot <- renderPlot({
    funcionDePloteo()()
  })
  
  # output$mensaje <- renderText({
  #   paste("Que pasa: !",input$analisis)
  # })
  
})
