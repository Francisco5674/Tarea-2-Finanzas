# import packages
library(zoo)

# Set up working directory

setwd("C:/Users/franc/OneDrive/Escritorio/registros UC/Finanzas/Tarea-2-Finanzas")

# Loading data

load('Nacciones.RData')
load('precios.RData')
load('sectores.RData')

# Funciones auxiliares

# Esta funcion calcula el retorno de un instrumento financiero, es necesario
# entregarle una matriz con los precios y cada fecha en la primera columna
retorno_de_instrumentos <- function(registro_precios){
  #Aislamos las fechas y quitamos la primera
  
  Dates = registro_precios[,1]
  Dates = Dates[-1]
  
  #sacamos los rendimientos con un loop
  
  rendimientos = c()
    
  for (i in 1:(length(registro_precios[,1])-1)){
    pos = i + 1
    P1 = as.numeric(registro_precios[pos,-1])
    P0 = as.numeric(registro_precios[pos - 1, -1])
    
    
    delta = (P1 - P0)/P0
    rendimientos = rbind(rendimientos,delta)
  }
  
  #Agregamos nombres y fechas
  
  rendimientos = cbind(Dates, rendimientos)
  colnames(rendimientos) <- colnames(registro_precios)
  
  return(rendimientos)
}

retorno_promedio <- function(valores){
  N = length(valores)
  valores = valores + 1
  producto = prod(valores)
  res = producto**(1/N)
  res = res - 1
  return(res)
}

# Esta funcion otorga los retornos de un portafolio de la categoria que le
# especifiques
portfolio <- function(categoria){
  
  retornos <- retorno_de_instrumentos(precios)
  
  # Ahora veremos que acciones pertenecen al sector 'categoria'
  
  Acciones = sectores[sectores[,6] == categoria,3]
  
  # Calculamos el largo del vector para saber cuantas acciones estamos tratando
  N = length(Acciones)
  
  # El vector total sumara los precios de cada accion
  total = rep(0,length(retornos[,1]))
  
  print(paste('La acciones del sector ', categoria, ' son'))
  print(Acciones)
  
  names_acciones = colnames(retornos)
  
  #El loop se encarga de filtrar y agregar
  
  for (name in names_acciones){
    
    if(toupper(name) %in% Acciones) {
      pos = which(colnames(retornos) == name)
      
      r = as.numeric(retornos[,pos]) + 1
      
      total = total + r
    }
  }
  
  #ya con el resultado, solo queda dividir el total y dar formato
  
  
  resultado = total/N
  
  resultado = resultado - 1
  
  resultado = cbind(retornos[,1],resultado)
  
  colnames(resultado) <- c('fecha', categoria)
  
  return(resultado)
}                       

# calcula el retotno de un portafolio hecho por las acciones que le digas
portfolio_ew_especifico <- function(data_precios, lista, n_port){
  # sacamos la cantidad de acciones
  N <- length(lista)
  # sacamos los retornos
  
  retornos <- retorno_de_instrumentos(data_precios)
  
  names_acciones = colnames(retornos)
  
  total = rep(0,length(retornos[,1]))
  
  for (name in names_acciones){
    
    if(name %in% lista) {
      pos = which(colnames(retornos) == name)
      
      r = na.omit(as.numeric(retornos[,pos]) + 1)
      
      total = total + r
    }
  }
  
  resultado = total/N
  
  resultado = resultado - 1
  
  resultado = cbind(retornos[,1],resultado)
  
  colnames(resultado) <- c('fecha', n_port)
  
  return(resultado)
}

# precios en formato mensual, (a fines de cada mes)

month_prices <- function(registro_precios, fechas, nombre_p){
  # lo registramos como dataframe
  data_f <- as.data.frame(cbind(fechas,registro_precios))
  
  # usamos yearmon
  data_f$fechas=as.yearmon(data_f$fechas)
  
  #sacamos los ultimos datos
  data_1 <- aggregate(data_f[,2], by=list(data_f$fechas), tail, 1)
  data_2 <- aggregate(data_f[,2], by=list(data_f$fechas), tail, 2)
  
  #revisamos <NA>
  for (pos in c(1: length(data_1[,1]))){
    if (is.na(data_1[pos,2])){
      if (!(pos == 1)){
        data_1[pos,2] <- data_2[pos,2][1]
      }
    }
  }
  
  #retornamos
  colnames(data_1) <- c('fecha', nombre_p)
  return(data_1)
}

productoria_f <- function(data){
  res <- rep(1, length(data[1,]))
  for (pos in c(1:length(data[,1]))){
    res <- res* data[pos,]
  }
  return(res)
}

source('effPortfolio.R')
source('GlobMin.R')
source('effFrontier.R')
source('plot.R')
source('tangency.portfolio.R')

