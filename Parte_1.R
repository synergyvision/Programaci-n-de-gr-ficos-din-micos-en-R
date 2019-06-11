######################################
# GRAFICOS USANDO LIBRERIA BASE DE R #
######################################

#___________________________________#
# Representación de nubes de puntos #
#___________________________________#
#Cargo data
data(trees)
trees

#realizo gráfico
plot(trees)


#Si sólo deseamos representar la altura del árbol 
#frente al diámetro del tronco, especificamos simplemente estas dos variables
with(trees,plot(Girth,Height))

#_________________________________________#
# Representación de funciones matemáticas #
#_________________________________________#
#defino valores en x
x=seq(0,2*pi,length=100)

#realizo grafica
plot(x,log(x),type="l",col="green",lwd=3)

#_______________________________________________#
# Representación simultánea de varias funciones #
#_______________________________________________#
plot(x,log(x),type="l",col="blue",lwd=3, main="Ln y Seno", xlab="", ylab="", las=1, col.axis="red")
lines(x,sin(x),col="green",lwd=3)

#añado leyenda
#Su primer argumento es una cadena de caracteres para fijar su posición: 
#"bottomright", "bottom", "bottomleft", "left", "topleft", "top", 
#"topright", "right" y "center"

plot(x,log(x),type="l",col="blue",lwd=3,main="Ln y Seno",xlab="",ylab="",las=1,col.axis="red")
lines(x,sin(x),col="green",lwd=3)
legend("bottomright",col=c("blue","green"),legend =c("Ln","Seno"), lwd=3, bty = "n")

#_________#
# Colores #
#_________#

#_____________________#
# 1) NOMBRE EN INGLES #
#_____________________#
#Usar función colors()
#rojo
plot(x,log(x),type="l",col="red",lwd=3)

#azul
plot(x,log(x),type="l",col="blue",lwd=3)

#marron
plot(x,log(x),type="l",col="brown",lwd=3)

#________#
# 2) RGB #
#________#

#rojo
plot(x,log(x),type="l",col=rgb(1,0,0),lwd=3)

plot(x,log(x),type="l",col=rgb(255,0,0,max=255),lwd=3)

#verde
plot(x,log(x),type="l",col=rgb(0,1,0),lwd=3)

#azul
plot(x,log(x),type="l",col=rgb(0,0,1),lwd=3)

#negro
plot(x,log(x),type="l",col=rgb(0,0,0),lwd=3)

#blanco
plot(x,log(x),type="l",col=rgb(1,1,1),lwd=3)

#rojo + azul
plot(x,log(x),type="l",col=rgb(1,0,1),lwd=3)

#azul
plot(x,log(x),type="l",col=rgb(58, 117, 196,max=255),lwd=3)

#_______________________#
# 3) CODIGO HEXADECIMAL #
#_______________________#

#AZUL DODGER
plot(x,log(x),type="l",col="#4169E1",lwd=3)

#verde
plot(x,log(x),type="l",col="#008000",lwd=3)

#granate
plot(x,log(x),type="l",col="#800000",lwd=3)

#_______________________________#
# 4) HSV (Hue-Saturation-Value) #
#_______________________________#
#ver función rgb2hsv()

rgb2hsv(58,117,196)

#azul
plot(x,log(x),type="l",col=hsv(rgb2hsv(58,117,196)),lwd=3)

#__________#
# 5) otros #
#__________#

palette()

rainbow(1)
heat.colors(1)
terrain.colors(1)
topo.colors(1)
cm.colors(1) 
grey(1)


#______________________#
# Uso de parámetro pch #
#______________________#

#defino data
light <- c(20, 20, 20, 20, 21, 24, 44, 60, 90,94, 101)

rate <- c(1.73, 1.65, 2.02, 1.89, 2.61, 1.36, 2.37,2.08, 2.69, 2.32, 3.67)

plot(light,rate)

#El parámetro pch que recibe como valor un entero 0:18 
#cambia el formato de los puntos de la nube.

plot(light,rate,pch=0)
plot(light,rate,pch=1)
plot(light,rate,pch=2)
plot(light,rate,pch=3)

#_______________#
# Uso de points #
#_______________#

rate1 <- c(2.13, 1.97, 1.82, 2.15, 2.91,1.76, 2.17, 1.98, 2.89, 2.12, 4.15)

points(light, rate1, pch=2,col="red")

#_____________#
# Histogramas #
#_____________#

#defino data
fec <- c(12.8, 21.6, 14.8, 23.1, 34.6, 19.7, 22.6, 29.6, 16.4,
                20.3, 29.3, 14.9, 27.3,22.4, 27.5, 20.3, 38.7, 26.4, 23.7, 26.1, 29.5,
                38.6, 44.4, 23.2, 23.6, 38.4, 32.9, 48.5, 20.9, 11.6, 22.3, 30.2, 33.4,
                26.7, 39, 12.8, 14.6, 12.2, 23.1, 29.4, 16, 20.1, 23.3, 22.9, 22.5, 15.1,
                31, 16.9, 16.1, 10.8, 35.4, 27.4, 19.3, 41.8, 20.3, 37.6, 36.9,37.3, 28.2,
                23.4, 33.7, 29.2, 41.7, 22.6, 40.4, 34.4, 30.4, 14.9, 51.8, 33.8, 37.9,
                29.5, 42.4, 36.6, 47.4)

#grafico
hist(fec)

#grafico 1
hist(fec, col="grey", xlab="Ejex x",
     ylab="Eje y",main="Histograma")


#COMANDO IMPORTANTE
par(mfrow=c(1,2))

hist(fec, breaks=7, col="grey", xlab="Ejex x",
     ylab="Eje y",main="Histograma")

hist(fec, breaks=seq(from=10,to=60,by=10), col="grey",
     xlab="Ejex x", ylab="Eje y",
     main="Histograma")

# Histograma con borde
#elimino graficos 
dev.off()
hist(fec, breaks=seq(from=10,to=60,by=10), col="grey",
     border="blue", xlab="Ejex x", ylab="Eje y",main="Histograma")

#__________#
# Boxplots #
#__________#

#data a usar
z <- iris[,"Sepal.Width"]

# boxplot simple
boxplot(z)

# boxplot horizontal
# uso comando para mostrar dos graficos en la misma ventana
par(mfrow = c(2,1))

boxplot(z, horizontal = TRUE, ylim = c(2, 4.5))
hist(z, xlim = c(2, 4.5))

#regreso ventana a la normalidad
par(mfrow = c(1,1))

# boxplot simple de las tres variables
boxplot(iris$Sepal.Length ~ iris$Species)

# boxplot horizontal simple de las tres variables
boxplot(iris$Sepal.Length ~ iris$Species,
        horizontal = TRUE)

# boxplot horizontal simple de las tres variables, con colores
boxplot(iris$Sepal.Length ~ iris$Species, horizontal = TRUE,
        border = c("blue", "green", "red"))
