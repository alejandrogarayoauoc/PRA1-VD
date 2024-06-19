library(shiny)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(shinythemes)
library(gridExtra)
library(ggthemes)
library(dplyr)
library(xtable)
library(plotly)
library(fpp3)
library(shinythemes)
library(corrplot)
library(cluster)
library(lares)
library(factoextra)
library(FactoMineR)
library(lattice)
library(GGally)

# Definicion de variables que voy a usar en la UI y en el servidor
vino <- read.csv("WineQT.csv")
nombresColumnas = c("Acidez fija", "Acidez volatil", "Acido citrico", "Azucar residual", "Cloruros",
                    "Dioxido de azufre libre", "Dioxido de azufre total", "Densidad", "pH", "Sulfatos",
                    "Alcohol", "Calidad", "id")
names(vino) = nombresColumnas


vino_apr <- vino %>% 
  mutate(
    aprobado = case_when( vino$Calidad == "5"| vino$Calidad == "6"|vino$Calidad == "7"|vino$Calidad == "8" ~ "Aprobado", vino$Calidad == "4"|vino$Calidad == "3"|vino$Calidad == "2"~ "Suspendido"
    )
  )

vino_numericas <- vino %>% select(-id)

vino_factor <- vino_numericas
vino_factor$Calidad <-as.factor(vino_factor$Calidad)

# Para el clustering
vino_numericas_esc<-scale(vino_numericas)


#####  UI

ui <- fluidPage(includeCSS("tema_tinto.css"),
                p(strong("Trabajo final visualización de datos: Análisis del vino")),theme = shinytheme("darkly"),
                tabsetPanel(

                  tabPanel(
                    title = "Analisis individual de los datos",
                    tabsetPanel(
                           
                           sidebarLayout(
                             sidebarPanel(
                                          sliderInput(inputId = "intervalos",
                                                      "División de intervalos:",
                                                      min = 0.001,
                                                      max = 3,
                                                      value = 1),
                                          varSelectInput("variables", "Variable:", vino), 
                                          selectInput(inputId = "color",
                                                      label = "Eleccion del color:",
                                                      choices = c("Color Vino tinto", "Color Rosado","Color Champan", "Color Vino blanco", "Color Manzanilla")),
                                          
                                          
                                          
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Datos",DT::dataTableOutput("tabla_vinos")),
                                 tabPanel("Histograma", plotOutput(outputId = "distPlotHist")),
                                 tabPanel("Diagrama de cajas", plotOutput(outputId = "distPlotCajas")),
                                 tabPanel("Diagrama de densidad", plotOutput(outputId = "distPlotDensidad")),
                                 tabPanel("Violines según aprobado/suspendido", plotOutput(outputId = "distPlotViolines")),
                                 
                                 tabPanel("Resumen",DT::dataTableOutput("tabla_vinos_resumen"))
                                 
                               ))
                           )
                    )
                  ),

                  tabPanel(
                    title = "Analisis multivariante de los datos",
                    tabsetPanel(
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(inputId = "variables_multiples",
                                      label = "Variables para correlación:",
                                      choices = names(vino),
                                      multiple = TRUE,
                                      selected = names(vino %>% select(-id))),

                          checkboxInput("reducido", "Gráfico de correlación reducido", FALSE),
                          numericInput(inputId = "ntop",
                                       label = "Número de mejores correlaciones a mostrar:",
                                       min = 1,
                                       max = 20,
                                       value = 10),
                          checkboxInput("division_calidad", "Dividir paralelos por calidad", FALSE),
                        ),
                        

                        
                        mainPanel(
                          width = 20,
                          height = 60,
                          tabsetPanel(
                            tabPanel("Correlación",plotOutput(outputId = "multi_cor")),
                            tabPanel("Correlación: Los mayores valores", plotOutput(outputId = "top_cor")),
                            tabPanel("Correlación para Calidad", plotOutput(outputId = "top_cor_focus")),
                            tabPanel("Gráficos de paralelos por calidad", plotOutput(outputId = "paralelo_calidad")),
                            
                            
                          ))
                      )
                    )
                  ),
                
                
                #CLUSTERING
                
                tabPanel("Clustering",
                         
                         mainPanel(
                           tabsetPanel(
                             tabPanel("No jerarquico",
                                      
                                      div(style="display: inline-block;",numericInput(inputId = "num",
                                                                                      "Numero de clusters", value = 6),radioButtons(
                                                                                        "clusType",
                                                                                        "Tipo de clustering:",
                                                                                        c("k-means"="kmeans",
                                                                                          "Clara"="clara"
                                                                                          
                                                                                        )
                                                                                      ),width = '100px'),
                                      
                                      div(style="display: inline-block;",
                                          
                                          checkboxInput("sil", "Analisis silhouette", FALSE),
                                          radioButtons(
                                            "distType",
                                            "Tipo de distancia:",
                                            c("Euclidea"="eu",
                                              "Manhattan"="ma"
                                              
                                            ))),
                                      plotOutput("plotC"),
                             ),
                             
                             
                             #Jerarquico
                             tabPanel("Jerarquico", 
                                      fluidRow(
                                        column(8,plotOutput("plotJ",width = "100%")),
                                        column(3,
                                               numericInput(inputId = "num2",
                                                            "Numero de clusters", value = 6),
                                               
                                               selectInput("linkage", "Funcion linkage:",
                                                           c("Complete" = "co",
                                                             "Average"="av",
                                                             "Diana" = "di"
                                                           )),
                                        ),
                                        column(1,checkboxInput("colo", "Colorear dendograma segun numero de clusters", TRUE))
                                      ),
                                      
                                      
                                      
                                      fluidRow(
                                        column(4,
                                               radioButtons(
                                                 "distType2",
                                                 "Tipo de distancia:",
                                                 c("Euclidea"="eu",
                                                   "Manhattan"="ma",
                                                   "Minkowski"="mi"
                                                   
                                                 )),
                                               
                                        ),
                                        column(4, radioButtons(
                                          "shape",
                                          "Forma del dendograma:",
                                          c("Rectangular"="re",
                                            "Circular"="ci",
                                            "Filogenica"="fi"
                                            
                                          )),),
                                        column(4, actionButton("do", "Generar dendograma"),)
                                        
                                      ),
                                      
                                      
                                      
                                      
                             ),
                             
                             
                             
                           ),
                           
                         )
                )
                
                ),)
                


  


#### SERVER

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  colorInput <- reactive({
    switch(input$color,
           "Color Vino tinto" = "#8B1A1A",
           "Color Rosado" = "#FFAEB9",
           "Color Champan" = "#FFDAB9", 
           "Color Vino blanco"="#CDBE70",
           "Color Manzanilla" = "#8B814C") 
    
  })
  
  resumendata <- reactive({
    vector_num <- data.frame(round(t(colMeans(vino)), 4), row.names = c("Media")) # Medias
    vector_num["Desviacion tipica",] <- round(apply(vino, 2, sd), 3) # desv
    vector_num[c("Minimo", "25%", "50%", "75%", "Maximo"), ] <- apply(vino, 2, quantile) # Cuartiles
    vector_num
  })
  
  
  
  
  ## GRAFICOS
  
  
  # Analisis individual
  output$distPlotHist <- renderPlot({
    
    ggplot(vino, aes(!!input$variables)) + geom_histogram( binwidth=input$intervalos, fill=colorInput(), color=colorInput(), alpha=0.9) + theme_pander()
    
  })
  output$distPlotCajas <- renderPlot({
    
    ggplot(vino, aes(!!input$variables)) + geom_boxplot( fill=colorInput(), color="#000000", alpha=0.9)+ theme_pander()
    
  })
  output$distPlotDensidad <- renderPlot({
    
    ggplot(vino, aes(!!input$variables)) + geom_density( fill=colorInput(), color=colorInput(), alpha=0.9) + theme_pander()
    
  })
  output$distPlotViolines <- renderPlot({
    
    ggplot(vino_apr,height = 850, aes(x=aprobado, y=!!input$variables, fill=aprobado)) + geom_violin(width=0.66, fill = c(colorInput()))+ geom_boxplot(width=0.1, color="grey", alpha=0.2)+ theme(legend.position="top") + theme_pander() + ggtitle("Violines Aprobado/Suspendido")# fill=name allow to automatically dedicate a color for each group
    
  })
  tabla_vinos <- reactive(
    DT::datatable(
      vino, options = list(scrollX = TRUE, sScrollY = '75vh', scrollCollapse = TRUE, lengthMenu = c(5, 15, 30)), extensions = list("Responsive")))
  
  
  
  
  output$tabla_vinos <- DT::renderDataTable({
    tabla_vinos()
  })
  
  
  
  tabla_resumen <- reactive(
    DT::datatable(
      resumendata(), options = list(scrollX = TRUE, sScrollY = '75vh', scrollCollapse = TRUE, lengthMenu = F), extensions = list("Responsive")))
  
  output$tabla_vinos_resumen <- DT::renderDataTable({
    tabla_resumen()
     
  })
  
  color2Input <- reactive({
    switch(input$color2,
           "Color Vino tinto" = "#8B1A1A",
           "Color Rosado" = "#FFAEB9",
           "Color Champan" = "#FFDAB9",
           "Color Vino blanco"="#CDBE70",
           "Color Manzanilla" = "#8B814C")
    
  })
  
  
  
  # Multivariante
  output$multi_cor <- renderPlot({
    var_sel <- input$variables_multiples
    correlation_matrix <- cor(vino[var_sel])
    type <- ifelse(input$reducido, 'upper', 'full')
    corrplot(correlation_matrix, type=type)
    
  })
  
  output$top_cor <- renderPlot({corr_cross(vino, rm.na = T, max_pvalue = 0.05, top = input$ntop, grid = T)})
  
  output$top_cor_focus <- renderPlot({corr_var(vino, "Calidad", input$ntop)})
  
  output$paralelo_calidad <- renderPlot({
    if (input$division_calidad) {
      ggparcoord(data = vino_factor,
                 columns = 1:11,
                 splineFactor = TRUE,
                 groupColumn = "Calidad") + facet_wrap(~ Calidad)
    } else {
      ggparcoord(data = vino_factor,
                 columns = 1:11,
                 splineFactor = TRUE,
                 groupColumn = "Calidad")
    }
  })
  
  
  
  #CLUSTERING
  
  #NO JERARQUICO
  output$plotC <- renderPlot({
    

    tipo<-switch(
      input$clusType,
      "kmeans"="kmeans",
      "clara"="clara")
    
    distancia<-switch(
      input$distType,
      "eu"="euclidean",
      "ma"="manhattan"
      
    )
    
    clusterK<-kmeans(vino_numericas_esc, centers=input$num)
    
    clusterClara<- clara(x = vino_numericas_esc, k = input$num, metric = distancia, stand = TRUE, samples = 50,
                         pamLike = TRUE)
    
    
    if(tipo=="kmeans"){
      g<-fviz_cluster(object = clusterK, data = vino_numericas_esc, palette = "Set2", ggtheme = theme_minimal(),ellipse.type = "norm",
                      geom = "point")+theme_pander() + labs(title = "Resultados clustering Kmeans") + theme(legend.position = "right")
    }
    else{
      g<-fviz_cluster(object = clusterClara, ellipse.type = "t", geom = "point", palette = "Set2",pointsize = 2.5) +theme_pander() + labs(title = "Resultados clustering CLARA") + theme(legend.position = "right")
      
    }
    
    if(input$sil==TRUE){
      sil_clusters <- eclust(x = vino_numericas_esc, FUNcluster = tipo, k = input$num, seed = 123, hc_metric = distancia,
                             graph = FALSE)
      s<-fviz_silhouette(sil.obj = sil_clusters, print.summary = FALSE, palette = "jco" )+theme_pander()
      g<-grid.arrange(g,s)
    }
    
    g
    
    
  })
  
  

  
  #JERARQUICO
  
  
  observeEvent(input$do,{
    
    showModal(modalDialog(
      title = "ADVERTENCIA",
      paste0("Por favor, sea paciente. R tardara alrededor de un minuto en mostrar el dendongrama pedido"),
      easyClose = TRUE,
      footer = modalButton("Aceptar")))
    
    
    
    distancia<-switch(
      input$distType2,
      "eu"="euclidean",
      "ma"="manhattan",
      "mi"="minkowski"
      
    )
    
    
    
    linkF<-switch(
      input$linkage,
      "co"="complete",
      "av"="average",
      "di"="di",
    )
    
    forma<-switch(
      input$shape,
      "re"="rectangle",
      "ci"="circular",
      "fi"="phylogenic"
    )
    
    dis<-dist(vino_numericas_esc, method = distancia, p = 2)
    
    if(input$colo==TRUE){
      nCl<-input$num2
    }
    else{
      nCl<-NULL
    }
    
    if(linkF=="di"){
      hc<-diana(dis)
    }
    else{
      hc<-hclust(d = dis, method = linkF)
    }
    
    gr<-fviz_dend(x = hc, k = nCl, cex = 0.6, type=forma) + geom_hline(yintercept = 5.5, linetype = "dashed") + labs(title = "Clustering jerárquico")+theme_pander()
    
  
    output$plotJ <- renderPlot({
      
      gr
      
      
    })
    
  })}
  

# Iniciamos la app llamando al UI y al servidor
shinyApp(ui = ui, server = server)