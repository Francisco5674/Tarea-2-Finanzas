# utilizando la funcio creada en el archico Data load podemos calcular el precio
# a fines de mes de cada accion

reg_precios <- precios
reg_precios <- reg_precios[,-1]


reg_precios_month <- c()
for (accion in colnames(reg_precios)){
  
  res <- month_prices(reg_precios[, colnames(reg_precios) == accion], 
                      precios[,1], accion)
  fechas <- res[,1]
  
  reg_precios_month <- cbind(reg_precios_month, res[,2])
}

reg_precios_month <- data.frame(fechas, reg_precios_month)
colnames(reg_precios_month) <- colnames(precios)

# sacamos los retornos correspondientes
retornos_month <- retorno_de_instrumentos(reg_precios_month)

################################################################################

# sintaxis de fechas: as.yearmon(as.Date.yearmon(retornos_month[,1]))

# ahora calculamos los retornos acumulados anuales
# tomamos las fechas de interes

fechas <- as.yearmon(as.Date.yearmon(retornos_month[,1]))
todas_fechas <- as.yearmon(as.Date.yearmon(retornos_month[,1]))
fechas <- fechas[fechas >= 'Dec 2017']
fechas <- fechas[fechas <= 'Dec 2019']
print(fechas)

#ahora calculamos los retornos con un loop
retornos_year <- retornos_month

# ahora sumaremos 1 a los retornos
retornos_month_b <- retornos_month + 1
retornos_month_b <- retornos_month_b[,-1]

for (fecha in fechas){
  fila <- which(todas_fechas == fecha)
  retornos_year[fila,-1] <- productoria_f(retornos_month_b[(fila - 11):(fila - 1),])
}

retornos_year <- retornos_year[12:length(retornos_year[,1]),]

print(fechas)
for (fecha in fechas){
  print(as.yearmon(as.Date.yearmon(fecha)))
}

################################################################################
# Los portafolios

# vamos a calcular el numero de acciones a fin de mes para sacar portafolios 
# value wighted

reg_nacciones <- Nacciones
reg_nacciones <- reg_nacciones[,-1]

reg_nacciones_month <- c()
for (accion in colnames(reg_nacciones)){
  
  res <- month_prices(reg_nacciones[, colnames(reg_nacciones) == accion], 
                      Nacciones[,1], accion)
  dates <- res[,1]
  
  reg_nacciones_month <- cbind(reg_nacciones_month, res[,2])
}

reg_nacciones_month <- data.frame(dates, reg_nacciones_month)
colnames(reg_nacciones_month) <- colnames(Nacciones)


# creamos una funcion para calcular los ponderadores de un value wighted

ponderadores <- function(reg_n_acc, reg_p_acc, lista, fecha){
  
  total <- 0
  value <- c()
  for (accion in lista){
    pos_r_nacciones <- which(reg_n_acc[,1] == fecha)
    pos_c_nacciones <- which(colnames(reg_n_acc) == accion)
    
    pos_r_precios <- which(reg_p_acc[,1] == fecha)
    pos_c_precios <- which(colnames(reg_p_acc) == accion)
    
    n_accion <- as.numeric(reg_n_acc[pos_r_nacciones, pos_c_nacciones])
    p_accion <- as.numeric(reg_p_acc[pos_r_precios, pos_c_precios])
    
    total <- total + n_accion*p_accion
    value <- c(value, n_accion*p_accion)
  }
  
  res <- value/total
  names(res) <- lista
  return(res)
}

# Tenemos que eliminar a mallplaza y cencoshop devido a su carencia de datos
# ademas, el ipsa tampoco es necesario

retornos_year <- retornos_year[,-11]
retornos_year <- retornos_year[,-26]
retornos_year <- retornos_year[,-2]

# ahora discriminaremos a los ganadores, perdedores y de medio retorno

port_ew <- c()
port_vw <- c()


for (fecha in fechas){
  
  perdedores <- (names(sort(retornos_year[retornos_year[,1] == fecha,][-1])))[
    1:9
  ]
  ret_medios <- (names(sort(retornos_year[retornos_year[,1] == fecha,][-1])))[
    10:19
  ]
  ganadores <- (names(sort(retornos_year[retornos_year[,1] == fecha,][-1])))[
    20:28
  ]
  
  ganadores <- na.omit(ganadores)
  ret_medios <- na.omit(ret_medios)
  perdedores <- na.omit(perdedores)
  
  # EQUALY weighted portfolio#################################################
  # retornos de los perdedores
  retornos_p <- retornos_month[,colnames(retornos_month) %in% c('fecha', perdedores)]
  retornos_p <- retornos_p[retornos_p[,1] == fecha,][-1]
  retornos_p <- mean(retornos_p)
  # retornos de los intermedios
  retornos_m <- retornos_month[,colnames(retornos_month) %in% c('fecha', ret_medios)]
  retornos_m <- retornos_m[retornos_m[,1] == fecha,][-1]
  retornos_m <- mean(retornos_m)
  #retornos ganadores
  retornos_g <- retornos_month[,colnames(retornos_month) %in% c('fecha', ganadores)]
  retornos_g <- retornos_g[retornos_g[,1] == fecha,][-1]
  retornos_g <- mean(retornos_g)
  
  portafolios <- c(fecha, retornos_p, retornos_m, retornos_g)
  port_ew <- rbind(port_ew, portafolios)
  
  # VALUE weighted portfolio#################################################
  # retornos de los perdedores
  pond <- ponderadores(reg_nacciones_month, reg_precios_month, perdedores, fecha)
  retornos_p <- retornos_month[,colnames(retornos_month) %in% c('fecha', perdedores)]
  retornos_p <- retornos_p[retornos_p[,1] == fecha,][-1]
  retornos_p <- retornos_p * pond
  retornos_p <- sum(retornos_p)
  # retornos de los intermedios
  pond <- ponderadores(reg_nacciones_month, reg_precios_month, ret_medios, fecha)
  retornos_m <- retornos_month[,colnames(retornos_month) %in% c('fecha', ret_medios)]
  retornos_m <- retornos_m[retornos_m[,1] == fecha,][-1]
  retornos_m <- retornos_m * pond
  retornos_m <- sum(retornos_m)
  #retornos 
  pond <- ponderadores(reg_nacciones_month, reg_precios_month, ganadores, fecha)
  retornos_g <- retornos_month[,colnames(retornos_month) %in% c('fecha', ganadores)]
  retornos_g <- retornos_g[retornos_g[,1] == fecha,][-1]
  retornos_g <- retornos_g * pond
  retornos_g <- sum(retornos_g)
  
  portafolios <- c(fecha, retornos_p, retornos_m, retornos_g)
  port_vw <- rbind(port_vw, portafolios)
}

colnames(port_ew) <- c('fecha', 'perdedores', 'intermedios', 'ganadores')
colnames(port_vw) <- c('fecha', 'perdedores', 'intermedios', 'ganadores')

###############################################################################
# hora de ver los momentum

port_ew <- cbind(port_ew, (port_ew[,4] - port_ew[,2]))
colnames(port_ew)[5] <- 'momentum'

port_vw <- cbind(port_vw, (port_vw[,4] - port_vw[,2]))
colnames(port_vw)[5] <- 'momentum'

###############################################################################
# calculando promedios y volatilidad

# primero los ew

medias <- c(mean(port_ew[,2]),mean(port_ew[,3]),
            mean(port_ew[,4]),mean(port_ew[,5]))
des_est <- c(sd(port_ew[,2]),sd(port_ew[,3]),
             sd(port_ew[,4]),sd(port_ew[,5])) 

sum_ew <- rbind(medias, des_est)
rownames(sum_ew) <- c('medias', 'sd')
colnames(sum_ew) <- c('perdedores', 'intermedios', 'ganadores', 'momentum')
sum_ew

# ahora los vw

medias <- c(mean(port_vw[,2]),mean(port_vw[,3]),
            mean(port_vw[,4]),mean(port_vw[,5]))
des_est <- c(sd(port_vw[,2]),sd(port_vw[,3]),
             sd(port_vw[,4]),sd(port_vw[,5])) 

sum_vw <- rbind(medias, des_est)
rownames(sum_vw) <- c('medias', 'sd')
colnames(sum_vw) <- c('perdedores', 'intermedios', 'ganadores', 'momentum')
sum_vw

###############################################################################
# ahora graficamos IPSA y momentum

momentum <- port_vw[,5]

IPSA_datos <- retornos_month[,2][retornos_month[,1] %in% fechas]

plot(momentum, col = 'red', lwd = 2, type = 'l', ylab = 'retorno',
     main = paste('momentum (azul, claro) vs Ipsa (rojo, oscuro)'),
     ylim = c(-0.15, 0.15))
lines(IPSA_datos, col = 'skyblue', lwd = 2)

##############################################################################
# ahora veremos los costos asociados a rablancear los portafolios

# se calculara de la siguiente manera:

# Nuestros portafolios ganadores y perdedores, tienen 9 acciones cada uno
# por lo tanto, si se sustituye una accion por otra, se debe pagar un 0.25% 
# en la venta de la accion antigua, y un 0.25% en la compra de la accion nueva
 
# Por ejemplo, si se modifica una accion del portafolio ganador, entonces,
# se le restara 1/9 * (0.25% + 0.25%) a la rentabilidad del portafolio. En caso
# de que sean dos acciones las que se deben reemplazar, entonces se le restara
# 2/9 * (0.25% + 0.25%)

# en caso de que se agrege una accion, como pasa al principio ya que smu no 
# tiene data para diciembre del 2017, entonces solo se restara un 0.25%, puesto 
# no esta sustituyendo una accion, mas bien solo se esta agregando

# construimos un loop que definira el costo para cada fecha

costos <- c()

for (fecha in fechas){
  
  perdedores <- (names(sort(retornos_year[retornos_year[,1] == fecha,][-1])))[
    1:9
  ]
  ganadores <- (names(sort(retornos_year[retornos_year[,1] == fecha,][-1])))[
    20:28
  ]
  
  ganadores <- na.omit(ganadores)
  ret_medios <- na.omit(ret_medios)
  perdedores <- na.omit(perdedores)
  
  if (as.yearmon(fecha) > "Dec 2017"){
    
    total_g <- length(ganadores_anteriores)
    total_p <- length(perdedores_anteriores)
    
    print(paste('#################', as.yearmon(fecha), '######'))
    
    print('Se agregaron a ganadores')
    print(length(na.omit(setdiff(ganadores, ganadores_anteriores))))
    g_add <- length(na.omit(setdiff(ganadores, ganadores_anteriores)))
    g_add <- g_add/total_g
    
    print('Se quitaron de ganadores')
    print(length(na.omit(setdiff(ganadores_anteriores, ganadores))))
    g_rem <- length(na.omit(setdiff(ganadores_anteriores, ganadores)))
    g_rem <- g_rem/total_g
    
    print('############################')
    print('Se agregron a perdedores')
    print(length(na.omit(setdiff(perdedores, perdedores_anteriores))))
    p_add <- length(na.omit(setdiff(perdedores, perdedores_anteriores)))
    p_add <- p_add/total_p
    
    print('Se quitaron de perdedores')
    print(length(na.omit(setdiff(perdedores_anteriores, perdedores))))
    p_rem <- length(na.omit(setdiff(perdedores_anteriores, perdedores)))
    p_rem <- p_rem/total_p
    
    costo <- 0.0025*(g_add + g_rem + p_add + p_rem)
    print(paste('el costo es de ', costo))
    
    costos <- c(costos, costo)
    
  }
  
  perdedores_anteriores <- perdedores
  ganadores_anteriores <- ganadores
  
}

costos <- c(0, costos)

# agregarremos una columna a port_ew para calcular el retorno del momentum neto

port_ew <- cbind(port_ew, costos)
colnames(port_ew)[6] <- 'costos'

port_ew <- cbind(port_ew, (port_ew[,5] - port_ew[,6]))
colnames(port_ew)[7] <- 'momentum neto'

mean(port_ew[,7])
mean(port_ew[,6])
