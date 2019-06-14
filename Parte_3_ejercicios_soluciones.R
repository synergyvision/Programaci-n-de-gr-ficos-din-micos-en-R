##############
# EJERCICIOS #
##############


#################
# PAQUETE PLOTY #
#################
library( plotly )

#______________________________#
# EJERCICIO 1 - NUBE DE PUNTOS #
#______________________________#

# Crear un gráfico de una nube de puntos donde:
# a) la data del eje x sean 100 numeros aleatorios normales 
#de media 0 y varianza 1
# b) la data del eje y sean 100 numeros aleatorios normales
#de media 1 y varianza 2
# c) Usar color anaranjado
# d) Colocar título, nombre de eje x y nombre eje y, colocar 
#texto de su preferencia

plot_ly (
  x = rnorm(100,0,1),
  y = rnorm(100,0,1),
  type = 'scatter' , color = "orange",mode = 'markers') %>%
  layout(title = "Nube de puntos",
         xaxis = list(title = "números aleatorios"),
         yaxis = list(title = "alturas"
         ))


#_________________________________#
# EJERCICIO 2 - GRÁFICO DE BARRAS #
#_________________________________#

# Crear un gráfico de barras donde:
# a) la data del eje x sea una  secuencia de valores del 1 al 10
# b) la data del eje y sean 10 numeros aleatorios uniformes
#entre 10 y 30
# c) Colocar título, nombre de eje x y nombre eje y, colocar 
#texto de su preferencia

plot_ly (
  x = seq(from = 1,to = 10,by = 1),
  y = runif(n = 10,min = 10,max = 30),
  type = 'bar')  %>%
  layout(title = "Gráfico de barras",
         xaxis = list(title = "secuencia"),
         yaxis = list(title = "cantidades"
         ))


#________________________________#
# EJERCICIO 3 - GRÁFICO DE TORTA #
#________________________________#

# Crear un gráfico de torta donde:
# a) la data a usar sea un data frame de 10 filas y 2 columnas
# b) la primera columna (cat) debe ser una variable categórica, para ello 
# se puede crear un vector con estos caracteres,
# "Activo 1", "Activo 2", ... , "Activo 10"
# c) la segunda columna (valor) debe tener un valor entre $1 y $100
#para ello crear un vector de diez valores
# d) En la función plot_ly especificar la data y tipo de gráfico, 
#en labels colocar la  variable categórica, en values colocar 
#el valor numérico
# e) Colocar como título "Inversiones"

inversiones <- data.frame(cat=c("Activo 1","Activo 2","Activo 3","Activo 4",
                                "Activo 5","Activo 6","Activo 7","Activo 8",
                                "Activo 9","Activo 10"),valor=runif(10,1,100))
inversiones$cat <- as.factor(inversiones$cat)

plot_ly(data = inversiones, labels = ~cat, values = ~valor, type = 'pie') %>%
  layout(title = "Inversiones")


#__________________________#
# EJERCICIO 4 - HISTOGRAMA #
#__________________________#

# Crear un gráfico de un histograma donde:
# a) la data a usar sean 1000 números aleatorios de media 0 y 
#varianza 1 (asignar esto a la variable x)
# b) definir tipo de gráfico y agregar título de su preferencia


x <- rnorm(n=1000,mean= 0,sd=1) 

plot_ly ( x=x,
          type = 'histogram' ) %>%
  layout(title = "Histograma")

#_______________________#
# EJERCICIO 5 - BOXPLOT #
#_______________________#

# Crear un gráfico de caja (boxplot) donde:
# a) la data a usar sea la data diamonds del paquete ggplot2
# b) definir tipo de gráfico 
# c) en la variable y usar el precio (price)
# d) en la variable color usar el tipo de corte (cut)
# e) agregar título de su preferencia

plot_ly(data = ggplot2::diamonds, y = ~price, color = ~cut, type = 'box')  %>%
  layout(title = 'Gráfico de caja')


####################
# PAQUETE DYGRAPHS #
####################

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)


#_______________________________#
# EJERCICIO 6 - LINEAS Y PUNTOS #
#_______________________________#

# data
serie=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), value=runif(41))
str(serie$time)
serie=xts(x = serie$value, order.by = serie$time)

# Crear un gráfico de línea y puntos donde:
# a) la data a sea "serie" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar un ancho y alto de 500
# d) usar la función dyOptions para agregar los puntos (usar TRUE), 
#usar un tamaño de 3

# grafico
dygraph(data=serie,main = "Gráfico de líneas y puntos",xlab = "fechas",ylab = "valores",width = 500,height = 500) %>%
  dyOptions( drawPoints = TRUE, pointSize = 3 )

#_______________________________#
# EJERCICIO 7 - GRÁFICO DE ÁREA #
#_______________________________#

# Crear un gráfico de área donde:
# a) la data a sea "serie" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar la función dyOptions para agregar el tipo de grafico (usar TRUE), 
#usar un color morado


dygraph(data=serie,main = "Gráfico de área",xlab = "fechas",ylab = "valores") %>%
  dyOptions( fillGraph=TRUE ,colors="purple")

#________________________________#
# EJERCICIO 8 - GRÁFICO DE SALTO #
#________________________________#

# Crear un gráfico de salto donde:
# a) la data a sea "serie" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar la función dyOptions para agregar el tipo de grafico (usar TRUE, 
#considerar argumentos stepPlot y fillGraph), 
#usar un color azul

dygraph(data=serie,main = "Gráfico de salto",xlab = "fechas",ylab = "valores") %>%
  dyOptions( stepPlot=TRUE, fillGraph=TRUE,colors = "blue")

#________________________________#
# EJERCICIO 9 - GRÁFICO DE VELAS #
#________________________________#
# creo data
tendencia=sin(seq(1,41))+runif(41)
vela=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), valor1=tendencia, valor2=tendencia+rnorm(41), valor3=tendencia+rnorm(41), valor4=tendencia+rnorm(41) )
vela=xts(x = vela[,-1], order.by = vela$time)

# Crear un gráfico de vela donde:
# a) la data a sea "vela" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar la función dyCandlestick para agregar el tipo de grafico 

# grafico
dygraph(data=vela,main = "Gráfico de velas",xlab = "fechas",ylab = "valores") %>%
  dyCandlestick()

#_____________________________________#
# EJERCICIO 10 - GRÁFICO DE INTERVALO #
#_____________________________________#

# data
trend=sin(seq(1,41))+runif(41)
data=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), trend=trend, max=trend+abs(rnorm(41)), min=trend-abs(rnorm(41, sd=1)))
data=xts(x = data[,-1], order.by = data$time)

# Crear un gráfico de intervalo donde:
# a) la data a sea "data" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar la función dySeries para agregar el tipo de grafico y usar
# c("min", "trend", "max") como argumento de la misma

# Grafico
dygraph(data=data,main = "Gráfico de intervalo",xlab = "fechas",ylab = "valores") %>%
  dySeries(c("min", "trend", "max"))



