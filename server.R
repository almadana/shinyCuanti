#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# colours

col1 = "#b4a5fa"
col2 = "#c8fab4"
col1f = "#6446fa"
col2f = "#9bfa82"

library(shiny)
library(ggplot2)
library(htmlTable)
library(dplyr)
load("./data/censo.RData")
load("./data/darkTriad.RData")
load("./data/serce.RData")
load("./data/latino.RData")
load("./data/encuesta.RData")
load("./data/descripciones.RData")
#load("./data/miniBase.RData")
#load("./data/expeCuna.RData")
#load("./data/inteligencia.RData")
#load("./data/wealth.RData")
load("./data/endis.RData")
load("./data/wvs.RData")
load('./data/Aristas.RData')

source("./cuanti_theme.r")


#GA logger settings
# ga_set_tracking_id("UA-136860877-2")
# ga_set_approval(consent = TRUE)



#guambia
currentDataset = censoFil


shinyServer(function(input, output,session) {
  tablaKey <- reactiveVal(0)
  
  #nombre_bases = c("endis"="ENDIS","serce"="SERCE","wvs"="Encuesta mundial de valores","triada"="Tríada oscura",
  #                 "aristas"="Aristas","latinoBaro1"="Latinobarómetro",
  #                 "censo"="Censo Nacional de Psicólogos","encuestaCuanti"="Encuesta estudiantes de cuanti")
  
  #ga_collect_pageview(page = "/panel", title = "Panel", hostname = "cuanti.psico.edu.uy")
  
  #"serce","Tríada oscura"="triada","Latinobarómetro"="latinoBaro1","Censo Nacional de Psicólogos"="censo"),
  listaDeDatos = list("endis"=endis,"wvs"=wvs,"censo"=censoFil,"triada"=dt,"serce"=serce,#"latinoBaro1"=latino,
                      "encuestaCuanti"=encuesta1,#"miniBase"=miniBase,"expeCuna"=music1,"inteligencia"=inteli,"riqueza"=wealth3
                      "aristas"=aristas)
  muestra <- NULL
  
  
  observeEvent(input$mostrar_desc,{
    nombre <- input$selectorDatos
    n_base = which(descripciones$base == nombre)
    texto_modal = descripciones$html_descripcion[n_base]
    
    showModal(modalDialog(
      title = "Acerca de la base de datos",
      HTML(texto_modal),
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
    
  })
  
  
  estado_visible <- reactiveVal(FALSE)
  
  observeEvent(input$toggleSidebar, {
    nuevo_estado <- !estado_visible()
    estado_visible(nuevo_estado)
    
    shinyjs::toggle(id = "tabla_resumen")
    
    texto <- if (nuevo_estado) "Ocultar resumen" else "Mostrar resumen"
    shinyjs::html("toggleSidebar", texto)
  })
  
  
    dataSet <- eventReactive(input$selectorDatos,{
      
        laData = input$selectorDatos
        listaDeDatos[[laData]]
    
  })
  
    #--- cargar data() ----
        data <- eventReactive(input$goButton,{
        df=dataSet()
        v1=NULL
        v1name=NULL
        v2=NULL
        v2name=NULL
        
        if (!is.null(muestra())) {
          df = df[muestra(),]
        }
        if (!is.null(input$var1)) {
          v1name=input$var1
          if (!is.null(input$var2)) { # if v2 can be not null only if v1 is
            #print(input$var2)
            v2name=input$var2
            df = df |> drop_na(all_of(c(v1name,v2name)))
            v2 = df |> pull(v2name)
          }
          else { # only v1 is not null
            df = df |> drop_na(all_of(v1name))
          
          }
          v1 = df |> pull(v1name)
        }
        anali=input$analisis
#      print(anali)
      ##########################################################
      # 1- var1, 2-var2, 3-var1name, 4-var2name, 5-anali, 6-data
      #########################################################
        list(v1,v2,v1name,v2name,anali,df)
      }
    )
    
    
    observeEvent(input$goButton, {
      tablaKey(tablaKey() + 1)
    })
    

    #--- cargar output.anali ----
    output$anali <- eventReactive(input$goButton,{
      #ga_collect_event(event_category = "Analizar",event_label = "Botón de análisis")
      input$analisis
    })
    outputOptions(output, "anali", suspendWhenHidden = FALSE)
    
    
    
    #--- cargar struct() ----
    # a: tipos de variable
    # b: lista de niveles de categóricas
    # numericas: TRUE con variables numéricas
    struct <- eventReactive(dataSet(),{
        a=unlist(lapply(dataSet(),class))
        b=(lapply(dataSet(),levels))
        numericas=unlist(lapply(b,is.null))
      list(a,b,numericas)
    })
    
    
    #--- Actualizar valor de muestreo: ------
    muestra <- eventReactive(c(input$muestrear,input$sliderMuestra,input$remuestrear),{
      remuestrear()
    })
    # 
    # muestra <- eventReactive(input$remuestrear,{
    #   remuestrear()
    # },ignoreInit = T)
    # # 
    # muestra <- eventReactive(input$muestrear,{
    #   if (input$muestrear) {
    #     remuestrear()
    #   }
    #   else {
    #     NULL
    #   }
    # })
    # 
    remuestrear <- function() {
      N = nrow(dataSet())
      if (input$muestrear) {
        updateCheckboxInput(session,"muestrear",label=paste0("Muestrear aleatoriamente un ",input$sliderMuestra,"% de los casos"))
        #actualizar dataSet
        nCasos = round(input$sliderMuestra/100*N)
        sample(N,nCasos)
      }
      else {
        updateCheckboxInput(session,"muestrear",label="Tomar muestra aleatoria")
        1:N
      }
    }
    
    
    output$condition <- eventReactive(input$muestrear,
      input$muestrear
    )
    outputOptions(output, "condition", suspendWhenHidden = FALSE)
    
     
    #------ actualizar valores de var-selector1 ---- fix: parece innecesario -----
    
    observeEvent(dataSet(), {
      varsCDs=colnames(dataSet())
      updatePickerInput(session, "var1", choices = varsCDs)
      updatePickerInput(session, "var2", choices = varsCDs)
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
      elegibles1=switch(input$analisis,
                        "ver"=NULL,
                        "descriptivo"=NULL,
                        "histograma"=nums,
                        "tablaF1"=!nums,
                        "tablaF2"=!nums,
                        "gbar"=!nums,
                        "boxplot"=nums,
                        "intconf"=nums,
                        "dispersion"=nums)    
      elegibles2=switch(input$analisis,
                        "ver"=NULL,
                        "descriptivo"=NULL,
                        "histograma"=NULL,
                        "tablaF1"=NULL,
                        "tablaF2"=!nums,
                        "gbar"=!nums,
                        "boxplot"=todas,
                        "intconf"=todas,
                        "dispersion"=nums)    
      
      updatePickerInput(session, "var1", choices = varsCDs[elegibles1])
      updatePickerInput(session, "var2", choices = c(varsCDs[elegibles2] ))
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
        print(x)
        if (nchar(x,type="chars")>50) {
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
      if (any(!numericas)) {
        sumario[!numericas,"Valores"]=niveles 
      }
      sumario[numericas,"Valores"]=minMax
      sumario
    },spacing="xs")
  
    umbralRecorte = 50 #máximo número de caracteres para display a full de niveles de variables categóricas
    recortarCaracteres <- function(x) {
      if (nchar(x)>umbralRecorte) {
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
             "intconf"=hacerIntervalo,
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
  #----------------- Tabla bivariada ---------------------
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
  
  output$chisq2Text <- renderText({
    x = data()[[1]]
    y = data()[[2]]
    a=chisq.test(x,y)
    paste("El valor de Chi cuadrado es: ",round(a$statistic,2),"con",a$parameter,"grados de libertad.\n El p-valor es",catch_pvalues(a$p.value))
  })

  
  hacerTablaDescriptiva <- function(){
    datos = data()[[6]]
    numericas=struct()[[3]]
    medias=apply(datos[,numericas],2,mean,na.rm=T)
    desv=apply(datos[,numericas],2,sd,na.rm=T)
    qtiles= round(t(apply(datos[,numericas],2,quantile,na.rm=T)),2)
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
      df = data()[[6]] # el data frame posta
      nombre = data()[[3]] # nombre de variable para hacer el histograma
      d = df[[nombre]] # variable elegida (datos)
      n_bins = input$bins
      # generate bins based on input$bins from ui.R
      bins <- seq(min(d,na.rm=T), max(d,na.rm=T), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      titulo = paste("Histograma de",input$var1)
      #print(d)
      #print(is.numeric(d))
      #print(df[nombre])
      p_h = ggplot(df,aes(x=.data[[nombre]])) + geom_histogram(aes(y = ..density..), bins = n_bins,fill=col1f,col=col2f) + labs(x = nombre,
                                                                            y = "frecuencia",
                                                                            title = titulo) + 
        theme_cuanti()
      
      if (input$ajusteNormal) {
        media_d = mean(d, na.rm = TRUE)
        sd_d = sd(d, na.rm = TRUE)
        
        # Crear datos para la curva normal
        x_seq = seq(min(d, na.rm = TRUE), max(d, na.rm = TRUE), length.out = 100)
        norm_data = tibble(x = x_seq, y = dnorm(x_seq, mean = media_d, sd = sd_d))
        
        # Agregar la curva normal
        p_h = p_h + geom_line(data = norm_data, aes(x = x, y = y), color = col2f, size = 1)
      }
      show(p_h)
    }

    hacerDispersion = function() {
      nombre.x = data()[[3]]
      nombre.y = data()[[4]]
      df = data()[[6]] # el data frame posta
      
      
      
      pointSize = input$pointSize
      
      # draw the histogram with the specified number of bins
      #titulo = paste("Histograma de",input$var1)
      #plot(x, y,cex = pointSize,xlab = nombre.x,ylab=nombre.y,main="")
      p_d = ggplot(df,aes(x=.data[[nombre.x]],y=.data[[nombre.y]])) + geom_point(size=pointSize,col=col1f) + theme_cuanti()
      if (input$tendencia) {
        #abline(lm(y~x))
        p_d = p_d + geom_smooth(method="lm",se=F,col=col2f,size=2)
      }
      show(p_d)
    }
    
    output$coefCorrel1 <- renderText({
      df = data()[6]
      x = data()[[1]]
      y = data()[[2]]
      paste("El coeficiente de correlación de Pearson es:",round(cor(x,y,use = "pairwise.complete.obs"),2))
    })
        
    output$pCorrel1 <- renderText({
      x = data()[[1]]
      y = data()[[2]]
      paste("El p-valor de la prueba de hipótesis (nula r=0) es:",catch_pvalues(cor.test(x,y)$p.value))
    })
    
    
    #  - GRÁFICOS DE BARRAS--------
    #  función para hacer los gráficos de barr
    hacerGrafBarras = function() {
      nombre.x = data()[[3]]
      nombre.y = data()[[4]]
      df = data()[[6]]
    

      if (input$var2==input$var1) {
        nombre=data()[[3]]
        tab=table(data()[[1]])
  
        if (input$freq == "porcentuales") {
          p_bar = ggplot(df, aes(x = .data[[nombre.x]])) +
            geom_bar(aes(y = after_stat(prop)),col=col2f,fill=col1f) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            labs(y = "Porcentaje", title = "Barras porcentuales")
        } else {
          p_bar = ggplot(df, aes(x = .data[[nombre.x]])) +
            geom_bar(col=col2f,fill=col1f) +
            labs(y = "Frecuencias absolutas", title = "Barras (frecuencias)")
        }
      }
        
      else {
      
        if (input$freq=="porcentuales") {
          
          if (input$stacked == "apiladas") { 
            p_bar = ggplot(df, aes(x = .data[[nombre.x]], fill = .data[[nombre.y]])) +
              geom_bar(position = "fill") +
              scale_y_continuous(labels = scales::percent_format()) +
              labs(y = "Porcentaje", title = "Barras apiladas porcentuales")
          }
          
          else {
            p_bar = df %>% 
              group_by(.data[[nombre.x]],.data[[nombre.y]]) %>% 
              summarise(n = n(), .groups = "drop") %>%
              group_by(.data[[nombre.x]]) %>% 
              mutate(porcentaje = n / sum(n)) %>% 
              ggplot(aes(x = .data[[nombre.x]], y=porcentaje,fill = .data[[nombre.y]])) +
              geom_bar(stat = "identity", position = position_dodge()) +
              scale_y_continuous(labels = scales::percent_format(scale = 100)) +
              labs(y = "Porcentaje", title = "Barras lado a lado porcentuales")
            
          }
        }
        else {
          if (input$stacked == "apiladas") { 

            p_bar = ggplot(df, aes(x = .data[[nombre.x]], fill = .data[[nombre.y]])) +
              geom_bar(position = "stack") +
              labs(y = "Frecuencias absolutas", title = "Barras apiladas")
          }
          else {
            p_bar = ggplot(df, aes(x = .data[[nombre.x]], fill = .data[[nombre.y]])) +
              geom_bar(position = position_dodge()) +
              labs(y = "Frecuencias absolutas", title = "Barras lado a lado")
            
          }
        }
        # 
        # barplot(tab,beside = input$stacked!="apiladas",xlab = nombre2)
        # legend(position,levels(data()[[1]]),fill=gray.colors(length(levels(data()[[1]]))))
        # title(paste("Gráfico de",nombre1,"según",nombre2))
        # 
      #qplot(dt,aes_string(x=a,fill=b))+geom_bar(stat="identity",position = position_dodge())
      }
      show(p_bar + theme_cuanti() + scale_fill_categorical() )
    }
    hacerBoxplot = function() {
      x = data()[[1]]
      y = data()[[2]]
      df = data()[[6]]
      nombre1 = data()[[3]]
      nombre2 = data()[[4]]
      nums = struct()[[3]]
      nombre_vars = c(nombre1,nombre2)
      
      if (input$var2 == input$var1) {
        # Univariado
        mx = mean(x, na.rm = TRUE)
        ggplot(df, aes(x = "", y = .data[[nombre1]])) +
          geom_boxplot(fill = col1) +
          geom_hline(yintercept = mx, color = col2f) +
          labs(x = "", y = nombre1, title = paste("Diagrama de caja de", nombre1)) +
          theme_cuanti()
      } else {
        if (nums[nombre2]) {
          # Ambos numéricos: mostramos como dos boxplots lado a lado
          #df2 = tibble(variable = c(nombre1, nombre2),
          #             valor = c(x, y))
          
          # df con las medias por variable, en long
          medias = df |> summarise(across(all_of(nombre_vars),mean)) |> 
            pivot_longer(nombre_vars)          
          
          #df en long
          df2 = df |> select(all_of(nombre_vars)) |> pivot_longer(nombre_vars)
          ggplot(df2, aes(x = name, y = value)) +
            geom_boxplot(fill = col1) +
            geom_point(data = medias, 
                       color = col2f, size = 3) +
            labs(x = "Variable", y = "Valores", title = "Diagrama de caja") +
            theme_cuanti()
        } else {
          # Y es categórica: boxplot clásico con medias por grupo
          medias = df |>
            group_by(.data[[nombre2]]) |>
            summarise(media = mean(.data[[nombre1]], na.rm = TRUE))
          
          ggplot(df, aes(x = .data[[nombre2]], y = .data[[nombre1]])) +
            geom_boxplot(fill = col1) +
            geom_point(data = medias, aes(x = .data[[nombre2]], y = media), 
                       color = col2f, size = 3) +
            labs(x = nombre2, y = nombre1, title = "Diagrama de caja") +
            theme_cuanti()
        }
      }
    }
    
    output$studentText <- renderText({
     # Prueba T de Student para las medias.
      x=data()[[1]]
      y=data()[[2]]
      if (input$var2==input$var1) {
        a=t.test(x)
        paste("El valor de t para una muestra es: ",round(a$statistic,2),"\n. El p-valor es: ",catch_pvalues(a$p.value))
      }
      else{
        nums=struct()[[3]]
        nombre2=data()[[4]]
          if (nums[nombre2]) {
              a=t.test(x,y,paired = input$pairedStudent)
              paste("El valor de t para dos muestras",ifelse(input$pairedStudent,"pareadas","independientes"), "es: ",round(a$statistic,5),"\n. El p-valor es: ",catch_pvalues(a$p.value))
          }
        else {
          etiquetas=levels(as.factor(y))
          niveles=length(etiquetas)
          a=pairwise.t.test(x,y)$p.value
          texto = "Los p-valores de los contraste de medias son: \n"
          for (i in 1:(niveles-1)) {
            for (j in (i):(niveles-1)) {
              texto = paste(texto,etiquetas[i]," --",etiquetas[j+1],":",catch_pvalues(a[j,i]),"\n")
            }  
          }
          print("\n")
          print(a)
          print("---")
          print(etiquetas)
          texto
        }
      }
    })
    
    catch_pvalues<-function(x){
      rp=round(x,5)
      if (rp==0) {
        return("<0.00001")
      }
      else{
        return(rp)
      }
    }
  
    # hacerIntervalo= function() {
    #   x=data()[[1]]
    #   y=data()[[2]]
    #   multiplicador=1.96
    #   if (input$var2==input$var1) {
    #     nombre=data()[[3]]
    #     mx=mean(x,na.rm=T)
    #     sdx = sd(x,na.rm = T)
    #     sex = sdx/sqrt(length(x))
    #     plot(mx,main=paste("Intervalo de confianza de 95% para la media de",nombre),ylim = c(mx-2*multiplicador*sex,mx+2*multiplicador*sex))
    #     segments(x0=1,y0=(mx-multiplicador*sex),x1=1,y1=(mx+multiplicador*sex),col="red")
    #   }
    #   else {
    #     nums=struct()[[3]]
    #     datos=data()[[6]]
    #     nombre1=data()[[3]]
    #     nombre2=data()[[4]]
    #     if (nums[nombre2]) {
    #       #print(nombre2)
    #       mx=mean(x,na.rm = T)
    #       my=mean(y,na.rm = T)
    #       sdx = sd(x,na.rm = T)
    #       sex = sdx/sqrt(length(x))
    #       sdy = sd(y,na.rm = T)
    #       sey = sdy/sqrt(length(y))
    #       lowery = min(mx-2*multiplicador*sex,my-2*multiplicador*sey)
    #       uppery = max(mx+2*multiplicador*sex,my+2*multiplicador*sey)
    #       plot(c(mx,my),main="Intervalos de confianza de 95% para la media",ylim=c(lowery,uppery),xlim=c(.5,2.5),xaxt="n")
    #       segments(x0=1,y0=(mx-multiplicador*sex),x1=1,y1=(mx+multiplicador*sex),col="red")
    #       segments(x0=2,y0=(my-multiplicador*sey),x1=2,y1=(my+multiplicador*sey),col="red")
    #       axis(side=1,at=c(1,2),labels=c(nombre1,nombre2))
    #     }
    #     else {
    #       #print(nombre2)
    #       mxy = tapply(x,y,function(x) mean(x,na.rm=T))
    #       sexy = tapply(x,y,function(x) sd(x,na.rm = T)/sqrt(length(x)))
    #       ycoords0 = mxy - multiplicador*sexy
    #       ycoords1 = mxy + multiplicador*sexy
    #       lowery = min(mxy-2*multiplicador*sexy)
    #       uppery = max(mxy+2*multiplicador*sexy)
    #       ycoords0
    #       ycoords1
    #       niveles=length(unique(y))
    #       xcoords=1:niveles
    #       plot(mxy,main="Intervalos de confianza de 95% para la media",xlab=nombre2,ylab=nombre1,ylim=c(lowery,uppery),xlim=c(.5,niveles+.5),xaxt="n")
    #       for (i in xcoords) {
    #         segments(i,ycoords0[i],i,ycoords1[i],col="red",lwd=2)
    #       }
    #       axis(side=1,at=xcoords,labels=levels(factor(y)))
    #     }
    #   }
    # }
    
    hacerIntervalo = function() {
      x = data()[[1]]
      y = data()[[2]]
      df = data()[[6]]
      nombre1 = data()[[3]]
      nombre2 = data()[[4]]
      nums = struct()[[3]]
      multiplicador = 1.96
      
      if (input$var2 == input$var1) {
        mx = mean(x, na.rm = TRUE)
        sex = sd(x, na.rm = TRUE) / sqrt(length(x))
        df_ic = tibble(grupo = nombre1, media = mx, li = mx - multiplicador * sex, ls = mx + multiplicador * sex)
        
        ggplot(df_ic, aes(x = grupo, y = media)) +
          geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2, color = col1f, size = 1.2) +
          geom_point(size = 5, fill=col2f,color = col1f,shape = 21,stroke=2) +
          labs(title = paste("Intervalo de confianza de 95% para la media de", nombre1),
               x = "", y = "Media") +
          theme_cuanti()
      } else if (nums[nombre2]) {
        # Ambos numéricos
        mx = mean(x, na.rm = TRUE)
        sex = sd(x, na.rm = TRUE) / sqrt(length(x))
        my = mean(y, na.rm = TRUE)
        sey = sd(y, na.rm = TRUE) / sqrt(length(y))
        
        df_ic = tibble(
          grupo = c(nombre1, nombre2),
          media = c(mx, my),
          li = c(mx - multiplicador * sex, my - multiplicador * sey),
          ls = c(mx + multiplicador * sex, my + multiplicador * sey)
        )
        
        ggplot(df_ic, aes(x = grupo, y = media)) +
          geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2, color = col1f, size = 1.2) +
          geom_point(size = 5, fill=col2f,color = col1f,shape = 21,stroke=2) +
          labs(title = "Intervalos de confianza de 95% para las medias",
               x = "Variable", y = "Media") +
          theme_cuanti()
      } else {
        # Y es categórica: medias por grupo
        df_ic = df |>
          group_by(.data[[nombre2]]) |>
          summarise(
            media = mean(.data[[nombre1]], na.rm = TRUE),
            li = media - multiplicador * sd(.data[[nombre1]], na.rm = TRUE) / sqrt(n()),
            ls = media + multiplicador * sd(.data[[nombre1]], na.rm = TRUE) / sqrt(n())
          )
        
        ggplot(df_ic, aes(x = .data[[nombre2]], y = media)) +
          geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2, color = col1f, size = 1.2) +
          geom_point(size = 5, fill=col2f,color = col1f,shape = 21,stroke=2) +
          labs(title = "Intervalos de confianza de 95% para la media",
               x = nombre2, y = nombre1) +
          theme_cuanti()
      }
    }
    
  
  #verDataFrame
  output$dataframe <- DT::renderDataTable({
    print(nrow(data()[[6]]))
    #print(str(data()[[6]]))
    # print(data()[[6]])
    #data()[[6]]
    DT::datatable(data()[[6]], rownames = FALSE, escape = FALSE, options = list(pageLength = 10))

  })
  # 
  # output$dataframe <- renderUI({
  #   req(data()[[6]])
  #   DT::renderDataTable({
  #     DT::datatable(data()[[6]])
  #   })
  # })
  
  
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
  
  
#--- Funciones de ploteo de distribuciones----  
  output$distriPlot <- renderPlot({
    funcionDePloteoDistri()()
  })
  
  funcionDePloteoDistri <- reactive({
    switch(input$selectorDist,
           "binomial"=hacerBinomial,
           "normal"=hacerNormal,
           "student"=hacerStudent,
           "chisq"=hacerChisq,
           noPlot)    
  })

  # en la Binomial, ajusta el máximo número posible de eventos positivos al número de intentos  
  observeEvent(input$nBin, {
    updateSliderInput(session,"kBin",max=input$nBin)
  })
  
  
  hacerBinomial <- function() {
    p=input$pBin
    n=input$nBin
    x=0:n
    valores = dbinom(x,size=n,prob = p)
    barplot(valores,names.arg = x,ylim = c(0,1.3*max(valores)))
    if (input$pvalor) {
      alphaValues = valores
      par(new=T)
      k=input$kBin
      k_1 = n-k
      big_k = max(k,k_1)
      if (input$colas=="Una cola") {
        if (big_k==k) {
          index_k = x >=k
        }
        else {
          index_k = x<=k
        }
        alphaValues[! (index_k) ] = 0
      }
      else {
        small_k = n-big_k
        index_big_k = x >= big_k
        index_small_k = x <= small_k
        alphaValues[ ! (index_big_k | index_small_k) ] = 0 
      }
      barplot(alphaValues,col = "red",ylim = c(0,1.3*max(valores)))
      mtext(paste("El p-valor es:",round(sum(alphaValues),4)),1)

    }
  }

  hacerStudent <- function() {
    gl=input$gLibertad
    x=seq(-5,5,length=1000)
    valores = dt(x,gl)
    plot(x,valores,lwd=2,type = "l",ylim = c(0,0.4))
    if (input$pvalor) {
      alphaValues = valores/sum(valores)
      par(new=T)
      t=input$tStudent
      if (t>0) {
        t_1 = -t
        big_t = t
      }
      else {
        t_1 = t
        big_t = -t
      }
      if (input$colas=="Una cola") {
        if (big_t==t) {
          index_t = x >=t
        }
        else {
          index_t = x<=t
        }
        alphaValues[!(index_t)]=0
        polygon(c(x[index_t],t),c(valores[index_t],0),col="red")
      }
      else {
        index_low_t = x<=t_1
        index_high_t = x>=big_t
        alphaValues[! (index_low_t | index_high_t)] = 0
        polygon(c(x[index_high_t],x[which(index_high_t)[1]]),c(valores[index_high_t],0),col="red")
        polygon(c(x[index_low_t],x[tail(which(index_low_t),1)]),c(valores[index_low_t],0),col="red")
      }
      mtext(paste("El p-valor es:",round(sum(alphaValues),4)),1)
      
    }
  }
  
  hacerChisq <- function() {
    gl=input$gLibertad
    x=seq(0,30+gl,length=1000)
    valores = dchisq(x,gl)
    plot(x,valores,lwd=2,type = "l",ylim = c(0,0.2))
    if (input$pvalor) {
      alphaValues = valores/sum(valores)
      par(new=T)
      chisq=input$chisq
      index_chisq = x >=chisq
      alphaValues[!(index_chisq)]=0
      polygon(c(x[index_chisq],chisq),c(valores[index_chisq],0),col="red")
      mtext(paste("El p-valor es:",round(sum(alphaValues),4)),1)
    }
  }
  
  
  
  
  
  hacerNormal <- function() {
    x=seq(-5,5,length=1000)
    valores = dnorm(x)
    plot(x,valores,lwd=2,type = "l",ylim = c(0,0.4))
    if (input$pvalor) {
      alphaValues = valores/sum(valores)
      par(new=T)
      z=input$zNormal
      if (z>0) {
        z_1 = -z
        big_z = z
      }
      else {
        z_1 = z
        big_z = -z
      }
      if (input$colas=="Una cola") {
        if (big_z==z) {
          index_z = x >=z
        }
        else {
          index_z = x<=z
        }
        alphaValues[!(index_z)]=0
        polygon(c(x[index_z],z),c(valores[index_z],0),col="red")
      }
      else {
        index_low_z = x<=z_1
        index_high_z = x>=big_z
        alphaValues[! (index_low_z | index_high_z)] = 0
        polygon(c(x[index_high_z],x[which(index_high_z)[1]]),c(valores[index_high_z],0),col="red")
        polygon(c(x[index_low_z],x[tail(which(index_low_z),1)]),c(valores[index_low_z],0),col="red")
      }
      mtext(paste("El p-valor es:",round(sum(alphaValues),4)),1)
    }
  }
    
  #--- Funciones de tabulado de distribuciones----
  output$distriTable <- renderUI({
    HTML(funcionDeTablaDistri()())
  })
  
  funcionDeTablaDistri <- reactive({
    switch(input$selectorDist,
           "binomial"=tablaBinomial,
           "normal"=tablaNormal,
           "student"=tablaStudent,
           "chisq"=tablaChisq,
           noPlot)    
  })
  
  tablaBinomial <- function() {
    p=input$pBin
    n=input$nBin
    k=0:n
    probabilidades = round(dbinom(k,size=n,prob = p),4)
    htmlTable(cbind(k,probabilidades))
  }
  
  tablaStudent <- function() {
    
  }
  
  tablaChisq <- function() {
    
  } 
  
  tablaNormal <- function() {
  } 
  
  # output$mensaje <- renderText({
  #   paste("Que pasa: !",input$analisis)
  # })
  
})
