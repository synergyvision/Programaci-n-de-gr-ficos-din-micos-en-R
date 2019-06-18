##############
# EJERCICIOS #
##############


################
# PAQUETE GRID #
################
library( grid )

#____________________________________#
# EJERCICIO 1 - MULTIPLES GRÁFICOS 1 #
#____________________________________#

#realizar dos gráficos en una ventana tal que:
# a) definir "data1" como un dataframe de dos columnas 
#donde la primera columna (x) sea una secuencia de valores entre 1 y 100,
#por su parte la segunda columna (y) debe ser la exponencial 
#de la columna anterior
# b) definir "data 2" como un dataframe de dos columnas 
#donde la primera columna (x) sea una secuencia de valores entre 1 y 100,
#por su parte la segunda columna (y) debe ser el logaritmo neperiano 
#de la columna anterior
# c) usar el geom_line para graficar la "data1" de color verde y guardarlo 
#en la variable graf1
# d) usar el geom_line para graficar la "data2" de color azul y guardarlo 
#en la variable graf2
# e) usar el comando "grid.arrange" con los gráficos anteriores como
#argumento y usar "nrow = 2" para obtener los gráficos en dos filas

data1 <- data.frame(x=___,y=___)
data2 <- data.frame(x=___,y=___)


graf1 <- ggplot(___, aes(x = ___, y = ___)) + 
  geom_line(col=___)
graf2 <- ggplot(___, aes(x = ___,y=___)) + 
  geom_line(col=___)

grid.arrange(___, ___, nrow = ___)


#____________________________________#
# EJERCICIO 2 - MULTIPLES GRÁFICOS 2 #
#____________________________________#

#realizar dos gráficos en una ventana tal que:
# a) usar los gráficos "graf1" y "graf2" definidos anteriormente
# en el comando "grid.arrange" para obtener los dos gráficos 
# ordenados en forma horizontal, usar argumento "ncol"

grid.arrange(___, ___, ___ = ___)

#________________________#
# EJERCICIO 3 -  GROBS 1 #
#________________________#

#realizar un gráfico de un Grob tal que:
# a) usar el comando "grid.rect" para obtener un rectángulo
# b) usar en x "unit(.2, "npc")" para especificar ubicación 
#con respecto al eje x
# c) usar en y "unit(100, "native")" para especificar ubicación 
#con respecto al eje y
# d) usar width=unit(1, "in") para especificar el ancho del rectángulo
# e) usar height=unit(3, "lines") para especificar el alto del rectángulo

grid.rect(x=___, y=___,width=___, height=___)

#________________________#
# EJERCICIO 4 -  GROBS 2 #
#________________________#

#realizar un gráfico de un Grob tal que:
# a) usar el comando "grid.circle" para obtener un círculo
# b) usar en x "unit(5/6, "npc")" para especificar ubicación 
#con respecto al eje x
# c) usar en y "unit(0.4, "npc")" para especificar ubicación 
#con respecto al eje y
# d) usar r=0.1 para indicar radio del círculo a trazar

grid.circle(x=___, y=___, r=___)

#_________________________#
# EJERCICIO 5 - VIEWPORTS #
#_________________________#

#realizar un gráfico de un viewport tal que:
# a) usar el comando "viewport" para crear un viewport usar 
# un ancho y un alto de .5 
# b) usar el comando "pushViewport" con el fin de añadir el viewport
# c) usar el comando "grid.rect" para obtener un rectángulo, usar color azul
#(fill)

vp <- viewport(width=___, height=___)
pushViewport(vp)
grid.rect(gp=gpar(col=NA, fill=___))


#________________________________________#
# EJERCICIO 6 - MODIFICAR TEMA EXISTENTE #
#________________________________________#

#crear un nuevo tema "mi_tema" tal que,
# a) asignar a la variable "mi_tema" el tema dark que tiene R (theme_dark())
# b) observar los valores a modificar en este caso serán el color del
#título principal (plot.title ) y el del nombre del eje x (axis.title.x)
#y del eje y (axis.title.y)
# c) usar comando "theme" para modificar color título principal, 
#usar color azul
# d) usar comando "theme" para modificar color eje x, usar color verde
# e) usar comando "theme" para modificar color eje y, usar color rojo
# f) asignar el nuevo tema creado ""mi_tema" para observar resultados

mi_tema <- ___

#valor a modificar
mi_tema$___
mi_tema$___
mi_tema$___

#modificar color titulo principal
mi_tema <- mi_tema + theme(plot.title = element_text(color = ___))

#modificar color eje x
mi_tema <- mi_tema + 
    theme(axis.title.x =   element_text(color = ___))

#modificar color eje y
mi_tema <- mi_tema + 
  theme(axis.title.y =   element_text(color =___))

#grafico
library(faraway)
ggplot(data = worldcup, aes(Time, Shots)) + 
  geom_point() + 
  ggtitle("Copa Mundial") + 
  ___

#_______________________________#
# EJEMPLO GEOM -  #
#_______________________________#

#estructura necesaria 1
GeomChristmasTree <- ggproto("GeomChristmasTree", Geom,
                             required_aes = c("x", "y"),
                             default_aes = aes(shape = 19, colour = "black", 
                                               fill = "green4", size = 3,
                                               linetype = 1, alpha = 1,
                                               fontsize = 1),
                             draw_key = draw_key_polygon,
                             
                             draw_panel = function(data, panel_scales, coord) {
                               coords <- coord$transform(data, panel_scales)
                               
                               # each tree has 4*branches + 3 points
                               if (length(coords$size) == 1) {
                                 tsize <- rep(pmax(1, round(coords$size)), length(coords$x))
                                 theight <- rep(pmax(0, round(coords$size)), length(coords$x))
                               } else {
                                 tsize <- pmax(1, round(coords$size))
                                 theight <- pmax(0, coords$size)
                               }
                               
                               # scale factors
                               r01x <- diff(range(coords$x))/100
                               r01y <- diff(range(coords$y))/100
                               
                               # coords
                               longx <- unlist(lapply(seq_along(coords$x), function(i) {
                                 if (tsize[i] == 1) {
                                   dx <- -c(0.3, 0.3, 1.2, 0, -1.2, -0.3, -0.3)
                                 } else {
                                   dx <- -c(0.3, 0.3, rep(c(1.2,0.3), tsize[i]-1), 1.2, 0, -1.2, rep(c(-0.3,-1.2), tsize[i]-1), -0.3, -0.3)
                                 }
                                 r01x*dx + coords$x[i]
                               }))
                               longy <- unlist(lapply(seq_along(coords$y), function(i) {
                                 if (tsize[i] == 1) {
                                   dy <- c(-0.5, 0, 0, theight[i], 0, 0, -0.5)
                                 } else {
                                   dy <- c(-0.5, 0, 0, rep(1:(tsize[i]-1), each=2), theight[i], rep((tsize[i]-1):1, each=2), 0, 0, -0.5)
                                 }
                                 r01y*dy + coords$y[i]
                               }))
                               longid <- unlist(sapply(seq_along(coords$y), function(i) {
                                 rep(i, each=4*tsize[i]+3)
                               }))
                               
                               grid::polygonGrob(
                                 longx, 
                                 longy,
                                 id = longid,
                                 gp = grid::gpar(col = coords[,"colour"],
                                                 fill = coords[,"fill"],
                                                 fontsize = 10)
                               )
                             }
)

#estructura necesaria 2
geom_christmas_tree <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE, ...) {
  layer(
    geom = GeomChristmasTree, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#grafico 1
ggplot(mpg, aes(displ, hwy, fill=manufacturer)) + 
  geom_christmas_tree(size=2)

#grafico 2
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + 
  stat_density_2d(aes(color=Species)) +
  geom_christmas_tree(aes(size=Petal.Length, fill=Species)) + 
  theme_void() + theme(legend.position="none")



