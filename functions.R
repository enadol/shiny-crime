#Genérica
sacarFila <- function(filaenturno, numero){
  entrada <- filaenturno[numero, ]
  entrada
  
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
  #anio== input$anio ??
  #for(i in claves){
  #   llave <- claves[i]
  population <- getPoblacionEstados(clave, anio)
  
  #casos <- listaDelitos[llave,]$ANUAL
  casos <- listaDelitos[clave,]
  tasa <- casos/population*100000
  tasa
  #print(population)
  #print(tasa)
}

#}
#TODO TERMINAR FUNCION
getTasaporYear <- function(listaDelitos, clave){
  #for(i in years){
  #llave <- years[i]
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
    print(tasa)
  }
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

getTypes <- function(input){
  s <- filter(delitos, delitos$MODALIDAD == input)
  s2 <- unique(s$TIPO)
  s2
  
}

getSubtypes <- function(input1, input2){
  s <- filter(delitos, delitos$MODALIDAD==input1 & delitos$TIPO == input2)
  s2 <- unique(s$SUBTIPO)
  s2
  
}