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
  x = rnorm(___,___,___),
  y = rnorm(___,___,___),
  type = ___ , color = ___,mode = 'markers') %>%
  layout(title = ___,
         xaxis = list(title = ___),
         yaxis = list(title = ___
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
  x = seq(from = ___,to = ___,by = ___),
  y = runif(n = ___,min = ___,max = ___),
  type = ___)  %>%
  layout(title = ___,
         xaxis = list(title = ___),
         yaxis = list(title = ___
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

inversiones <- data.frame(cat=___,valor=___)
inversiones$cat <- as.factor(inversiones$cat)

plot_ly(data = ___, labels = ~___, values = ~___, type = ___) %>%
  layout(title = ___)


#__________________________#
# EJERCICIO 4 - HISTOGRAMA #
#__________________________#

# Crear un gráfico de un histograma donde:
# a) la data a usar sean 1000 números aleatorios de media 0 y 
#varianza 1 (asignar esto a la variable x)
# b) definir tipo de gráfico y agregar título de su preferencia


x <- rnorm(n=___,mean= ___,sd=___) 

plot_ly ( x=___,
  type = ___ ) %>%
  layout(title = ___)

#_______________________#
# EJERCICIO 5 - BOXPLOT #
#_______________________#

# Crear un gráfico de caja (boxplot) donde:
# a) la data a usar sea la data diamonds del paquete ggplot2
# b) definir tipo de gráfico 
# c) en la variable y usar el precio (price)
# d) en la variable color usar el tipo de corte (cut)
# e) agregar título de su preferencia

plot_ly(data = ggplot2::___, y = ~___, color = ~___, type = ___)  %>%
  layout(title = ___)


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
dygraph(data=___,main = ___,xlab = ___,ylab = ___,width = ___,height = ___) %>%
  dyOptions( drawPoints = ___, pointSize = ___ )

#_______________________________#
# EJERCICIO 7 - GRÁFICO DE ÁREA #
#_______________________________#

# Crear un gráfico de área donde:
# a) la data a sea "serie" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar la función dyOptions para agregar el tipo de grafico (usar TRUE), 
#usar un color morado


dygraph(data=___,main = ___,xlab = ___,ylab = ___) %>%
  dyOptions( fillGraph=___ ,colors=___)

#________________________________#
# EJERCICIO 8 - GRÁFICO DE SALTO #
#________________________________#

# Crear un gráfico de salto donde:
# a) la data a sea "serie" definida anteriormente
# b) definir titulo y nombre de ejes de su preferencia
# c) usar la función dyOptions para agregar el tipo de grafico (usar TRUE, 
#considerar argumentos stepPlot y fillGraph), 
#usar un color azul

dygraph(data=___,main = ___,xlab = ___,ylab = ___) %>%
  dyOptions( stepPlot=___, fillGraph=___,colors = ___)

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
dygraph(data=___,main = ___,xlab = ___,ylab = ___) %>%
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
dygraph(data=___,main = ___,xlab = ___,ylab = ___) %>%
  dySeries(___)



