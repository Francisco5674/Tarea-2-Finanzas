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

names_d = c('cencosud', 'copec', 'colbun', 'cmpc', 'conchatoro')

sample_data_c <- sample_data[rownames(sample_data) 
                %in% names_d,]
sample_retornos_c <- sample_retornos[,colnames(sample_retornos) 
                %in% names_d]

globalMin.portfolio(sample_data_c[,1], var(sample_retornos_c))

efficient.portfolio(sample_data_c[,1], var(sample_retornos_c), target.return = 0.0137)

front_eff <- efficient.frontier(sample_data_c[,1], var(sample_retornos_c), nport = 30)

points(0.0137, 0.0601, col= 'red')

plot.Markowitz(front_eff)

# sacamos un csv para el grafico

data_to_csv = cbind(front_eff$er, front_eff$sd)
colnames(data_to_csv) = c('er', 'sd')
write.csv(data_to_csv, 'csv/puntos_front_eff.csv')

# d)

# calculamos el portafolio tangente y lo añadimos al registro

tang_port_d = tangency.portfolio(sample_data_c[,1], var(sample_retornos_c), risk.free = rf)

tang_retornos = c()

tang_port_d

for (name in names_d){
  tang_retornos = cbind(tang_retornos, tang_port_d$weights[name]*
                          sample_retornos[,name])
}

sample_retornos = cbind(sample_retornos ,rowSums(tang_retornos))
colnames(sample_retornos)[9] = 'tang_port_d'

mean(sample_retornos[,colnames(sample_retornos)=='tang_port_d'])

write.csv(sample_retornos, 'csv/sample_retornos.csv')

# e)

# calculamos las caracteristicas de este nuevo portafolio y lo agregamos
# a los datos ya recopilados
sample_data = rbind(sample_data, c(tang_port_d$er,
                                   tang_port_d$sd,
                                   (tang_port_d$er - rf)/tang_port_d$sd))

rownames(sample_data)[9] <- 'tang_port_d'

# f)

var(sample_retornos)

# bci

# veamos la recompensa riesgo
rec_riesgo <- function(name){
  delta_er = sample_data[ name,'mean'] - rf
  corr = var(sample_retornos)[ name, 'tang_port_d']/
    sqrt(var(sample_retornos)['tang_port_d', 'tang_port_d'])
  print(delta_er/corr)
}

print('############## bci ######################')
rec_riesgo('bci')
sample_data['tang_port_d', 'sharpe_ratio']
print('############## ccu ######################')
rec_riesgo('ccu')
sample_data['tang_port_d', 'sharpe_ratio']
print('############## sqm-b ####################')
rec_riesgo('sqm-b')
sample_data['tang_port_d', 'sharpe_ratio']

# g)

# calculamos los betas y los agregamos a nuestra informacion

betas = c()

for (name in rownames(sample_data)){
  beta = (var(sample_retornos)[name,'tang_port_d'])/
    (var(sample_retornos)['tang_port_d', 'tang_port_d'])
  betas = rbind(betas, beta)
}

sample_data = cbind(sample_data, betas)
colnames(sample_data)[4] = 'betas'

plot(sample_data[,4], sample_data[,1], ylab = 'E(R)', xlab = 'Beta', 
     ylim = c(0, 0.025), xlim = c(-0.05, 1))
abline(a = rf, b = (tang_port_d$er - rf))

text(sample_data[,4], sample_data[,1], rownames(sample_data), cex= 1, pos= 1)

points(0, rf, col = 'blue')

text(0, rf, 'rf', cex= 1, pos= 3)  

write.csv(sample_data, 'csv/sample_data.csv')

# h)
 
sample_data_h = sample_data[1:8,]

sample_retornos_h = sample_retornos[,1:8]

eff_Front = efficient.frontier(sample_data_h[,1], var(sample_retornos_h), nport = 30)

plot.Markowitz(eff_Front)

# i)

tang_port_i = tangency.portfolio(sample_data_h[,1], var(sample_retornos_h), risk.free = rf)

# el nuevo portafolio tangente es 
tang_port_i

write.csv(tang_port_i$weights, 'csv/port_tang_i.csv')

# y tiene un sharp ratio
(tang_port_i$er - rf)/tang_port_i$sd



