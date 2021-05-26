# el retorno fijo o sin riesgo
rf <- 0.001

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

# a) conseguimos los retornos de interes y sus estadisticas relevantes

sample_names = c('cencosud', 'bci', 'copec', 'colbun', 'cmpc', 'conchatoro', 'ccu', 'sqm-b')
sample_data = c()
sample_retornos = c()
for (accion in sample_names){
  data <- retornos_month[, colnames(retornos_month) == accion]
  retornos <- data
  media <- mean(data)
  des_est <- sd(data)
  
  sample_retornos <- cbind(sample_retornos, data)
  sample_data <- rbind(sample_data, c(media, des_est))
}

rownames(sample_data) <- sample_names
colnames(sample_data) <- c('mean', 'sd')
colnames(sample_retornos) <- sample_names

# b) conseguimos el sharpe_ratio

sharpe_ratio <- c()
for (accion in rownames(sample_data)){
  data <- sample_data[rownames(sample_data) == accion,]
  sharpe <- (data[1] - rf)/data[2]
  sharpe_ratio <- rbind(sharpe_ratio, sharpe)
}
colnames(sharpe_ratio) <- c('sharpe_ratio')

sample_data <- cbind(sample_data, sharpe_ratio)

# c) frontera minima varianza

# sacamos las variables de interes

sample_data_c <- sample_data[rownames(sample_data) 
                %in% c('cencosud', 'copec', 'colbun', 'cmpc', 'conchatoro'),]
sample_retornos_c <- sample_retornos[,colnames(sample_retornos) 
                %in% c('cencosud', 'copec', 'colbun', 'cmpc', 'conchatoro')]

min_var_port <- globalMin.portfolio()
min_var_port(sample_data_c[,1], var(sample_retornos_c))

eff_port_max <- efficient.portfolio()
eff_port_max(sample_data_c[,1], var(sample_retornos_c), target.return = 0.009129418)

data_plot <- c()
for (retorno in seq(1, 1000)){
  port <- eff_port_max(sample_data_c[,1], var(sample_retornos_c), target.return = retorno/10000)
  data_plot <- rbind(data_plot, c(port$sd, port$er))
}

plot(data_plot)

