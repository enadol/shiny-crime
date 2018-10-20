# Load packages ----
library(shiny)
#library(readxl)
library(dplyr)
library(plotly)
library(readr)
library(magrittr)
library(mxmaps)
library(leaflet)
library(htmltools)
library(shinycssloaders)

# Source helper functions -----
source("helpers.R")

meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

#for select in shiny
unicoentidad <- distinct(delitos, ENTIDAD)
unicoyear <- distinct(delitos, AÑO)
unicodelito <- distinct(delitos, MODALIDAD)
unicotipo <- distinct(delitos, TIPO)
unicosubtipo <- distinct(delitos, SUBTIPO)

b <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 1,
  dtick = 1,
  ticklen = 1,
  tickwidth = 1,
  tickcolor = toRGB("blue"),
  zeroline = FALSE,
  showlegend = FALSE
)
prueba3 <- as.data.frame(df_mxstate)


# Define UI for application
ui <- fluidPage( theme= "crime.css",
                 
                 # Application title
                 titlePanel("EVOLUCIÓN DEL DELITO EN MÉXICO 1997-2017"),
                 
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarPanel(
                   h3("Una herramienta interactiva para consultar datos sobre delitos específicos a nivel estatal, por año. Navegue en los menús de abajo y seleccione los indicadores deseados. Obtendrá el desglose estatal del delito en el año seleccionado, el mapa interactivo nacional del delito en consulta, el desglose mensual por año seleccionado, y el número de casos en todos los años para el estado seleccionado."),     
                   selectInput("estados",
                               label = "Seleccione la entidad:",
                               choices = c(unicoentidad)
                   ),
                   helpText(h5("Clasificación de delitos según el Sistema Nacional de Seguridad Pública")),
                   selectInput("delitos",
                               label="Seleccione el delito a analizar:",
                               choices = c(unicodelito)
                   ),
                   selectInput("tipo",
                               label = "Seleccione el tipo de delito a analizar: ",
                               choices =c(unicotipo)
                   ),
                   selectInput("subtipo", 
                               label="Seleccione el subtipo: ",
                               choices = c(unicosubtipo)
                   ),
                   selectInput("anio",
                               label = "Seleccione el año: ",
                               choices= c(unicoyear)
                   ),
                   helpText("Oprima el botón para invocar los datos"),
                   
                   actionButton("send", "Obtener gráficas"),
                   helpText(h6(strong("Desarrollo:"), "Enrique A. López Magallón - ", strong("@EnriqueALopezM"))),
                   helpText(h6(strong("Fuente estadística:"), "Sistema Nacional de Seguridad Pública"))
                   
                   
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   htmlOutput("titulo2"),
                   withSpinner(plotlyOutput("yearly")),
                   htmlOutput("mapa"),
                   withSpinner(leafletOutput("states")),
                   htmlOutput("titulo3"),
                   withSpinner(plotlyOutput("tasaprueba")),
                   htmlOutput("text"),
                   htmlOutput("titulo1"),
                   withSpinner(plotlyOutput("plot"))


                   #withSpinner(plotlyOutput("statesTasas"))
                   #plotOutput("states")
                   
                 )
                 
                 
)


# Load data ----
year <- as.integer(delitos$AÑO)
delito <- delitos$MODALIDAD
state <- delitos$ENTIDAD
tipo <- delitos$TIPO
subtipo <- delitos$SUBTIPO

years <- unique(populationMX$AÑO)
claves <- unique(populationMX$CLAVE)


#Genéricas
query <-function(df, state, delito, tipo, subtipo, anio){filter(df, df$ENTIDAD==state, df$MODALIDAD==delito, df$TIPO==tipo, df$SUBTIPO==subtipo, df$AÑO==anio)}
query2 <-function(df, state, delito, tipo, subtipo){filter(df, df$ENTIDAD==state, df$MODALIDAD==delito, df$TIPO==tipo, df$SUBTIPO==subtipo)}
query3 <- function(df, delito, tipo, subtipo, anio){filter(df, df$MODALIDAD==delito, df$TIPO==tipo, df$SUBTIPO==subtipo, df$AÑO==anio)}


#Genérica
sacarFila <- function(filaenturno, numero){
  entrada <- filaenturno[numero, ]
  entrada
  
}
#Genérica
sacarYear<- function(anio, fila){
  indice <- match(anio, fila$AÑO)
  #extracto <- sacarFilas(fila, indice)
  indice
}

#Genérica para datos
assignFields <- function(entrada){
  salida <- c(entrada$ENERO, entrada$FEBRERO, entrada$MARZO, entrada$ABRIL, entrada$MAYO, entrada$JUNIO, entrada$JULIO, entrada$AGOSTO, entrada$SEPTIEMBRE, entrada$OCTUBRE, entrada$NOVIEMBRE, entrada$DICIEMBRE) %>% as.integer()
}

#Genérica para suma de valores de años
totalesYears <- function(filas){
  container <- c()
  for(i in 1:21){
    anio <- filas[i, ]
    valores <- anio[7:18] %>% sum()
    container[i] <- valores
    i <- i+1
  }
  
  container
}

getPoblacionEstados <- function(clave, anio){
  
  fila <- filter(populationMX, populationMX$CLAVE==clave, populationMX$AÑO==anio)
  
  poblacion <- fila$POBLACION
  poblacion
  
}

getPoblacionYears <- function(clave){
  fila <- filter(populationMX, populationMX$CLAVE==clave)
  poblacion <- fila$POBLACION
  poblacion
  
}

#Genérica para tasas x 100.000 han

getTasaPorEstado <- function(listaDelitos, clave, anio){
    population <- getPoblacionEstados(clave, anio)
    casos <- listaDelitos[clave,]
    tasa <- casos/population*100000
    tasa

  }

  #}
#TODO TERMINAR FUNCION
getTasaporYear <- function(listaDelitos, clave){
    population <- getPoblacionYears(clave)
    #TODO ajustar a solo una lista, quitando clave
    casos <- listaDelitos$ANUAL
    tasa <- casos/population*100000
    tasa
    
  #}
 }

#para la gráfica de las tasas de todos los estados, por año seleccionado (mapa)
getTasaTodosEstados <- function(listaDelitos, anio){
  for(i in claves){
    tasa <- getTasaPorEstado(listaDelitos, i, anio)
    #print(tasa)
  }
}

getRatesStates <- function(df, listaDelitos, anio){
  listaRates <- c()
  for(i in 1:32){
    filtrada <- filter(df, df$CLAVE==i & df$AÑO==anio)
    casos <- as.integer(listaDelitos[i])
    poblacion <- as.integer(filtrada[4])
    tasa <- casos/poblacion*100000
    listaRates[i] <- tasa
    
    
  }
  listaRates
}

getRatesYears <- function(df, listaDelitos, state){
  listaRates <- c()
  for(i in 1:21){
    filtrada <- filter(df, df$ESTADO==state)
    casos <- as.integer(listaDelitos[i])
    poblacion <- as.integer(filtrada[4][i,])
    tasa <- casos/poblacion*100000
    listaRates[i] <- tasa
    
    
  }
  listaRates
}
    
  getTasaTodosYears <- function(listaDelitos, clave){
    
    for(i in 1:21){
      tasa <- getTasaporYear(listaDelitos, clave)
      
      
    }
    print(tasa)
  }
  


totalesStates <- function(filas){
  container2 <- c()
  for(i in 1:32){
    estado <- filas[i, ]
    valores <- estado[7:18] %>% sum()
    container2[i] <- valores
    i <- i+1
  }
  
  container2
}

getTypes <- function(df, input){
  s <- filter(df, df$MODALIDAD == input)
  s2 <- unique(s$TIPO)
  s2
  
}

getSubtypes <- function(df, input1, input2){
  s <- filter(df, df$MODALIDAD==input1 & df$TIPO == input2)
  s2 <- unique(s$SUBTIPO)
  s2
  
}

meses <- list("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  outVar <- reactive({
    tipos <- getTypes(delitos, input$delitos)
    tipos
    
    
  })
  
  outVar2 <- reactive({
    subtipos <- getSubtypes(delitos, input$delitos, input$tipo)
    subtipos
    
  })
  
  observe({
    updateSelectInput(session, "tipo", label = "Seleccione el tipo: ", choices = outVar())
    
  })
  
  observe({
    updateSelectInput(session, "subtipo", label="Seleccione el subtipo: ", choices = outVar2())
  })
  
  
  output$tipo <- renderUI({
    entrada <- getTypes(input$delitos)
    selectInput("tipo", "Escoja el tipo: ", choices = entrada)
  })
  
  output$subtipo <- renderUI({
    entrada <- getSubtypes(input$delitos)
    selectInput("subtipo", "Escoja el subtipo: ", choices = entrada)
    
  })
  
  observeEvent(input$send, {
    fila <- query(delitos, input$estados, input$delitos, input$tipo, input$subtipo, input$anio) %>% as.data.frame()
    fila2 <- query2(delitos, input$estados, input$delitos, input$tipo, input$subtipo) %>% as.data.frame()
    fila3 <- query3(delitos, input$delitos, input$tipo, input$subtipo, input$anio) %>% as.data.frame()
    
    salida <- assignFields(fila)
    salida2 <- totalesYears(fila2)
    salida3 <- totalesStates(fila3)
    prueba3$value <- salida3
    
    
    #tasa1 <- getTasa(salida, input$estados, input$anio)
    tasa2 <- getRatesYears(populationMX, salida2, input$estados)
    tasa3 <- getRatesStates(populationMX, salida3, input$anio)
    prueba3$tasa <- tasa3
    
    hovertext <- paste("Delito: ", input$delitos,"<br>Tipo: ",input$tipo ,"<br>Subtipo:",input$subtipo,"<br>Estado: ", input$estados, "<br>Año: ",input$anio,"<br>Casos:", salida)
    hovertextsin <- ""
    hovertextyear <- paste(input$delitos, "<br>en", input$estados, "<br>en", fila2$AÑO,":", format(round(tasa2, 2)), "por cada 100.000 habitantes.")
    hovertextstates <- paste(fila3$MODALIDAD, fila3$TIPO,"<br>", fila3$SUBTIPO, "<br>en", fila3$AÑO, "por entidad federativa. <br>",fila2$ENTIDAD,":", salida3)
    hovertexttasa2 <- paste(fila3$MODALIDAD, fila3$TIPO,"<br>", fila3$SUBTIPO, "<br>en", fila3$AÑO, "por entidad federativa. <br>",fila3$ENTIDAD,":", format(round(tasa3, 2)), "por cada 100.000 habitantes.")    
    
    m <- leaflet() %>% setView(lng= -102.552, lat =  23.6345, zoom = 4) %>% addTiles()
    
    
    
    if(nrow(fila[7:18])==0){
      otra <- as.data.frame(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
      
      output$text <- renderUI(h3("No hay datos con esta clasificación. Ajuste su selección."))              
      output$plot <- renderPlotly(plot_ly(data=otra, type="bar", x=meses, y=0) %>% layout(showlegends= FALSE) %>% add_annotations(text="No se generó gráfica. Escoja otra combinación", x=6, y=3, showarrow=FALSE, zeroline=FALSE))
      output$yearly <- renderPlotly(plot_ly(data=otra, type="bar", x=0, y=0) %>% layout(showlegends= FALSE) %>% add_annotations(text="No se generó gráfica. Escoja otra combinación", x=6, y=3, showarrow=FALSE, zeroline=FALSE))
      output$mapa <- renderUI(h3("No hay datos con esa clasificación"))
      output$states <- renderLeaflet(m)
    }
    
    else {
      pal <- colorNumeric(palette="Oranges", domain = tasa3)
      
      labels <- sprintf("<strong>%s</strong><br/>%s<br/>%s<br/>No. de casos: %s ",
                        fila3$ENTIDAD, fila3$TIPO, fila3$AÑO, prueba3$value
      ) %>% lapply(htmltools::HTML)
      
      
      output$text <- renderUI(h3(""))
      output$titulo1 <- renderUI(h4(paste(fila3$MODALIDAD, " - ",fila3$TIPO," - ", fila3$SUBTIPO, " - EN ", fila2$ENTIDAD, " EN ",as.character(fila3$AÑO), "(NO. CASOS POR MES). Pase el ratón por cada barra y obtendrá la cifra correspondiente.")))
      output$plot <- renderPlotly(plot_ly(data=fila, type="bar", x=meses, y=salida, text=hovertext, hoverinfo="text") %>% layout(showlegend= FALSE) %>% add_lines(text=hovertextsin))
      output$titulo2 <- renderUI(h4(paste(fila3$MODALIDAD,fila3$TIPO," - ", fila3$SUBTIPO, " - EN ", fila2$ENTIDAD, " 1997-2017 - TASA x 100.000 HABITANTES. Pase el ratón por cada barra y obtendrá la cifra correspondiente.")))
      output$yearly <- renderPlotly(plot_ly(data=fila2, type="bar", x=~AÑO, y=tasa2, colors="Oranges", color=tasa2, text=hovertextyear, hoverinfo="text", showlegend=FALSE) %>% layout(xaxis=list(dtick=1), yaxis=list(b)) %>% add_lines(text=hovertextsin))
      output$titulo3 <- renderUI(h4(paste(fila3$MODALIDAD,fila3$TIPO," - ", fila3$SUBTIPO, " POR ESTADO EN ", fila3$AÑO, " - TASA POR CADA 100.000 HABS. Pase el ratón por cada barra y obtendrá la cifra correspondiente.")))
      output$tasaprueba <- renderPlotly(plot_ly(data=prueba3, type="bar", x=fila3$ENTIDAD, y=tasa3, text=hovertexttasa2, hoverinfo="text") %>% layout(showlegend= FALSE))
      output$mapa <- renderUI(h4(paste("HOTSPOTS DE ", fila3$MODALIDAD," - ",fila3$TIPO," - ", fila3$SUBTIPO, " EN ", fila3$AÑO, "- TASA x 100.000 HABITANTES. Haga clic en el estado seleccionado y obtendrá la tasa correspondiente.")))
      output$states <- renderLeaflet(
     
        mxstate_leaflet(prueba3, pal, ~pal(tasa3), popup = sprintf("Estado: %s<br/>Tasa x 100.000 hab.:  %s", fila3$ENTIDAD, format(round(tasa3, 2), nsmall = 2)))
        
        
      )
      
      

     
            
    }
  }
  
  
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

