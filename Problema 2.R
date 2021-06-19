# primero cargamos los datos

load('DatosTarea2.RData')

# a) calculamos la regresion

nombres <- colnames(datos)[2:26]
data_reg = list()
for (name in nombres){
  Portafolios = datos[, name] - datos[,'RF']
  Beta = datos[, 'MktRF']
  regresion <- lm(Portafolios~Beta)
  data_reg[[name]] = regresion
  print(c('############## ', name, '#############'))
  print(summary(regresion))
}

# b) 

# c) ahora veamos el modelo de corte transversal

# calculamos los datos necesarios

prima_riesgo = c()
coeficientes = c()
for (name in nombres){
  prima_riesgo = c(prima_riesgo, mean(datos[, name] - datos[,'RF']))
  beta = data_reg[[name]]$coefficients['Beta']
  coeficientes = c(coeficientes, beta)  
}

# ahora que tenemos los datos, calculamos la regresion

corte_transversal = lm(prima_riesgo~coeficientes)
summary(corte_transversal)
mean(datos[, 'MktRF'])

unname(coeficientes)

prima_riesgo

plot(coeficientes, prima_riesgo)
abline(lm(prima_riesgo~coeficientes))

corte_transversal

