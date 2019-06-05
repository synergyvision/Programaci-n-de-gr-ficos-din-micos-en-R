#Ejercicios a incluir en libro interactivo
#1) Gráficos en R.

#grafico con librerias base
x <- seq(-5*pi,5*pi,length=100)
y <- cos(x)

#explicar argumentos funcion plot
# x
# y
# type
# "p" for points,
# "l" for lines,
# "b" for both,
# "c" for the lines part alone of "b",
# "o" for both ‘overplotted’,
# "h" for ‘histogram’ like (or ‘high-density’) vertical lines,
# "s" for stair steps,
# "S" for other steps, see ‘Details’ below,
# "n" for no plotting.
# main
# sub
# xlab
# ylab 
# asp : y/x ratio
# col
# lwd : grosor de linea

#lineas
plot(x,y,type="l",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

#puntos
plot(x,y,type="p",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

#lineas + puntos
#asp >1 eje y mayor a eje x
plot(x,y,type="b",col="blue",lwd=1,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y",asp = 5)

#asp <1 eje x mayor a eje y
plot(x,y,type="b",col="blue",lwd=1,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y",asp = 0.2)

#usando c, lineas sin puntos
plot(x,y,type="c",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

#usando o, overplotted
plot(x,y,type="o",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y",asp = 5)

#histograma
plot(rnorm(100,0,1),type="h",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

hist(rnorm(100,0,1))

#stairs step
plot(x,y,type="s",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

#other step
plot(x,y,type="S",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

#no plot
plot(x,y,type="n",col="blue",lwd=3,main = "Coseno",sub = "Gráfica 1",xlab = "eje x",
     ylab = "eje y")

#grafico usando ggplot2
#guiarse po cheat sheet
library(ggplot2)

#argumentos basicos
# data
# mapping = aestetics
# argumentos aestetics
# x
# y 
# colour
# size
# fg

#formas de definir un grafico con ggplot + uso de geom + titulo centrado

#forma 1
ggplot(data=data.frame(x,y), aes(x, y)) + geom_line(color="blue") + ggtitle("Coseno") +
  theme(plot.title = element_text(hjust = 0.5))

#forma 2
ggplot(data=data.frame(x,y)) + geom_line( aes(x, y),color="blue") + ggtitle("Coseno") +
  theme(plot.title = element_text(hjust = 0.5))

#forma 3
ggplot() + geom_line(data=data.frame(x,y),aes(x, y),color="blue") + ggtitle("Coseno") +
  theme(plot.title = element_text(hjust = 0.5))

# GEOMS
# una variable
# continua
#geom_area
#mpg: Fuel economy data from 1999 and 2008 for 38 popular models of car
# hwy: highway miles per gallon
a <- ggplot(mpg, aes(hwy))

a+geom_area(stat = "bin",color="green",fill="navy")

#geom_density
a + geom_density(kernel = "gaussian")

#geom_dotplot
a + geom_dotplot()

#geom_freqpoly
#linetype : 2,4,6 u 8
a + geom_freqpoly(linetype=8)

#geom_histogram
a + geom_histogram(binwidth = 5)


#discreta
#fl: fuel type
b <- ggplot(mpg, aes(fl))

#geom_bar
b + geom_bar()

# primitivas
#geom_polygon
ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

# Currently we need to manually merge the two together
datapoly <- merge(values, positions, by = c("id"))

p <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id))
p

#geom_path
d <- ggplot(economics,aes(date,unemploy))
d+geom_path(lineend = "butt",linejoin = "round",linemitre = 1)

#geom_ribbon
d+geom_ribbon(aes(ymin=unemploy-900,ymax=unemploy+900))

#geom_segment
e <- ggplot(seals,aes(x=long,y=lat))
e+geom_segment(aes(xend=long+delta_long,yend=lat + delta_lat))

#geom_rect
e+geom_rect(aes(xmin=long,ymin=lat,xmax=long+delta_long,
                ymax=lat + delta_lat))

# dos variables
# continua- continua
f <- ggplot(mpg, aes(cty, hwy))

#geom_blank
f+ geom_blank()

#geom_jitter
f+ geom_jitter()

#geom_point
f+ geom_point()

#geom_quantile
f+ geom_quantile()

#geom_rug
f+ geom_rug()

#geom_smooth
f+ geom_smooth()

#geom_text
f+ geom_text(aes(label = cty))

# discreta, continua
g <- ggplot(mpg,aes(class,hwy))

#geom_bar
g+geom_bar(stat = "identity")

#geom_boxplot
g+geom_boxplot()

#geom_dotplot
g+geom_dotplot(binaxis = "y",stackdir = "center")

#geom_violin
g+geom_violin(scale = "area")

# discreta- discreta
h <- ggplot(diamonds,aes(cut,color))

h+geom_jitter()

# distribucion continua bivariada
i <- ggplot()

#geom_bind2
d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
d + geom_bin2d()

#geom_density2d
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  xlim(0.5, 6) +
  ylim(40, 110)
m + geom_density_2d()

#geom_hex
d <- ggplot(diamonds, aes(carat, price))
d + geom_hex()

# funcion continua 
j <- ggplot(economics,aes(date, unemploy))

#geom_area
j+geom_area()

#geom_line
j+geom_line()

#geom_step
j+geom_step()

# visualizar el error
df <- data.frame(grp=c("A","B"),fit=4:5,se=1:2)
k <- ggplot(df,aes(grp,fit,ymin=fit-se,ymax=fit+se))

#geom_crossbar
k+geom_crossbar(fatten = 2)

#geom_errorbar
k+geom_errorbar()

#geom_linerange
k+geom_linerange()

#geom_pointrange
k+geom_pointrange()

# maps
data <- data.frame(murder=USArrests$Murder,state=tolower(rownames(USArrests)))
map=map_data("state")
l <- ggplot(data,aes(fill=murder))

#geom_map
l+geom_map(aes(map_id=state),map=map)+expand_limits(x=map$long,y=map$lat)

#tres variables
seals$z <- with(seals,sqrt(delta_long^2+delta_lat^2))
m <- ggplot(seals,aes(long,lat))
  
#geom_contour
m+geom_contour(aes(z=z))

#geom_raster
m + geom_raster(aes(fill = z), hjust=0.5, vjust=0.5, interpolate=FALSE)

#geom_tile
m + geom_tile(aes(fill = z))


#..............FIN PRIMERA HOJA........................#

# STATS
#stat_bin(geom="bar") es lo mismo que geom_bar(stat="bin")

#distribucion 1d
a <- ggplot(mpg, aes(hwy))

a+stat_bin(binwidth = 1,origin=10)

#distribuciones 2d
f <- ggplot(mpg, aes(cty, hwy))

f + stat_bin2d(bins = 30, drop = TRUE)


#3 variables
m + stat_contour(aes(z = z))

#comparaciones
g <- ggplot(mpg,aes(class,hwy))

g + stat_boxplot(coef = 1.5)

#funciones
f + stat_ecdf(n = 40)

#propositos generales
ggplot() + stat_function(aes(x = -3:3),
                         fun = dnorm, n = 101, args = list(sd=0.5))

# SCALES

#propositos generales
b <- ggplot(mpg, aes(fl))
n <- b + geom_bar(aes(fill = fl))

n

n + scale_fill_manual(
  values = c("skyblue", "royalblue", "blue", "navy"), limits = c("d", "e", "p", "r"), breaks =c("d", "e", "p", "r"), name = "fuel", labels = c("D", "E", "P", "R"))


# locaciones de x e y
last_month <- Sys.Date() - 0:29
df <- data.frame(
  date = last_month,
  price = runif(30)
)
base <- ggplot(df, aes(date, price)) +
  geom_line()

base + scale_x_date(date_labels = "%Y %m %d")
base + scale_x_date(date_labels = "%b %d")
base + scale_x_date(date_breaks = "1 week", date_labels = "%W")
base + scale_x_date(date_minor_breaks = "1 day")

# Set limits
base + scale_x_date(limits = c(Sys.Date() - 7, NA))

#color y relleno

#discretas
n <- b + geom_bar( aes(fill = fl))
n

n + scale_fill_brewer( palette = "Blues")

n + scale_fill_grey( start = 0.2, end = 0.8, na.value = "red")


#continuas
o <- a + geom_dotplot( aes(fill = ..x..))
o

o + scale_fill_gradient( low = "red",
                         high = "yellow")

o + scale_fill_gradientn( colours = terrain.colors(6))

#escalas de forma
p <- f + geom_point( aes(shape = fl))
p

p + scale_shape( solid = FALSE)

p + scale_shape_manual( values = c(3:7))

#escalas de tamaño
q <- f + geom_point( aes(size = cyl))
q

q + scale_size_area()

# SISTEMA DE COORDENADAS
r <- b + geom_bar()

r + coord_cartesian(xlim = c(0, 5))

r + coord_fixed(ratio = 1/2)

r + coord_flip()

r + coord_polar(theta = "x", direction=1 )

r + coord_trans(y = "sqrt")

r + coord_map(projection = "ortho",
              orientation=c(41, -74, 0))

# AJUSTE DE POSICION
s <- ggplot(mpg,aes(fl,fill=drv))

s + geom_bar(position = "dodge")

s + geom_bar(position = "fill")

s + geom_bar(position = "stack")

f + geom_point(position = "jitter")

s + geom_bar(position = position_dodge(width = 1))

# TEMAS
r +theme_bw()

r+theme_classic()

r+theme_grey()

r+theme_minimal()

r+theme_void()

# FACETING 
t <- ggplot(mpg,aes(cty,hwy)) + geom_point()

t + facet_grid(. ~ fl)

t + facet_grid(year ~ .)

t + facet_grid(year ~ fl)

t + facet_wrap(~ fl)

t + facet_grid(year ~ fl, scales = "free")

t + facet_grid(. ~ fl, labeller = label_both)

t + facet_grid(. ~ fl, labeller = label_bquote(alpha ^ .(x)))

t + facet_grid(. ~ fl, labeller = label_parsed)

# LABELS
t + ggtitle("New Plot Title")+xlab("New X label")+ylab("New Y label")

t + xlab("New X label")

t + ylab("New Y label")

t + labs(title =" New title", x = "New x", y = "New y")

# LEGEND
t + theme(legend.position = "bottom")

t + guides(color = "none")

t + scale_fill_discrete(name = "Title",
                        labels = c("A", "B", "C"))

# ZOOMING

t + coord_cartesian(
  xlim = c(0, 100), ylim = c(10, 20))

t + ylim(10, 20)

t + scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100))

#...................FINAL DE HOJA.............................

#dependiendo del geom usado se habilitan nuevas opciones de aestetics
#para geom_line
# x, y, alpha [0,1], color, linetype, size
ggplot() + geom_line(data=data.frame(x,y),aes(x, y),color="green",alpha=1,size=2) + ggtitle("Coseno") +
  theme(plot.title = element_text(hjust = 0.5))


#2) Construcción de gráficos usando el paquete ggplot2.
#3) Construcción de gráficos dinámicos usando el paquete ploty.

#informacion cheat sheet ploty
library( plotly )
p <- plot_ly (
  x = rnorm( 1000 ), y = rnorm( 1000 ), mode = 'markers' )
p

#graficos basicos
#graficos de lineas
plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7), type = 'scatter' , mode = 'lines' )

#nube de puntos
plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7 ),
  type = 'scatter' , mode = 'markers' )

#graficos de barra
plot_ly (
  x = c( 1, 2, 3),
  y = c( 5, 6, 7),
  type = 'bar' , mode = 'markers' )

#graficos de burbujas
plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7 ), type = 'scatter' , mode = 'markers' , size = c( 1, 5, 10 ), marker = list(
    color = c( 'red', 'blue' , 'green' )))

#mapas de calor
plot_ly (z=volcano,type='heatmap')

#graficos de area
plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7 ), type = 'scatter' , mode = 'lines' , fill = 'tozeroy' )

#layout (diseño)
#leyenda
set.seed( 123 )
x = 1 : 100
y1 = 2*x + rnorm ( 100 ) 
y2 = -2*x + rnorm ( 100 )
plot_ly ( x = x ,
          y = y1 ,
          type = 'scatter' ) %>%
  add_trace( x=x,
             y = y2 ) %>% layout(
               legend =
                 list( x = 0.5 ,
                       y=1,
                       bgcolor = '#F3F3F3' ))

#ejes
set.seed( 123 )
x = 1 : 100
y1 = 2*x + rnorm( 100 ) 
y2 = -2*x + rnorm( 100 )
axis_template <- list( showgrid = F , zeroline = F , nticks = 20 , showline = T ,
                       title = 'AXIS' , mirror = 'all' )
plot_ly ( x=x,
          y = y1 ,
          type = 'scatter' ) %>%
  layout(
    xaxis = axis_template , yaxis = axis_template )

#..................FINAL PRIMERA HOJA.....................

#graficos estadisticos
#Histogramas
x <- rchisq ( 100, 5, 0 ) 
plot_ly (
  x=x,
  type = 'histogram' )

#Box-plots
plot_ly (
  y = rnorm( 50 ) , type = 'box' ) %>%
  add_trace( y = rnorm( 50, 1 ))

#Histogramas 2D
plot_ly (
  x = rnorm( 1000, sd = 10 ) , y = rnorm( 1000, sd = 5 ) , type = 'histogram2d' )


#mapas
# grafico de burbuja
plot_ly (
  type = 'scattergeo' , lon = c( -73.5, 151.2 ) , lat = c( 45.5, -33.8 ) , marker = list (
    color = c( 'red' , 'blue' ) , size = c( 30, 50 ) , mode = 'markers' ))

# cloropleth map  
plot_ly (
  type = 'choropleth' ,
  locations = c( 'AZ', 'CA', 'VT' ) , locationmode = 'USA-states' , colorscale = 'Viridis' ,
  z = c( 10, 20, 40 )) %>%
  layout ( geo = list ( scope = 'usa' ))

# grafico de puntos
plot_ly (
  type = 'scattergeo' ,
  lon = c( 42, 39 ) ,
  lat = c( 12, 22 ) ,
  text = c( 'Rome' , 'Greece' ) , mode = 'markers' )

#graficos 3d
#Superficies 3D
# Using a dataframe: 
plot_ly (
type = 'surface' , z = ~volcano )

#graficos de linea 3D
plot_ly (
  type = 'scatter3d' , x = c( 9, 8, 5, 1 ) ,
  y = c( 1, 2, 4, 8 ) ,
  z = c( 11, 8, 15, 3 ) , mode = 'lines' )

# nube de puntos 3D
plot_ly (
  type = 'scatter3d' , x = c( 9, 8, 5, 1 ) ,
  y = c( 1, 2, 4, 8 ) ,
  z = c( 11, 8, 15, 3 ) , mode = 'markers' )

#jerarquia de figuras
...

#..................FINAL HOJA.....................

#comando importante
r

ggplotly(r)

#4) Construcción de temas y geoms.
#5) Uso del paquete leaflet, para la construcción de gráficos.


#NUEVO POSIBLE ORDEN A DAR OBJETIVOS
#HTML WIDGETS
#DIVERSOS PAQUETES A USAR 
#A) leaflet: Mapping
library(leaflet)
rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)

categories <- LETTERS[1:10]
df <- data.frame(
  lat = rand_lat(100), lng = rand_lng(100), size = runif(100, 5, 20),
  category = factor(sample(categories, 100, replace = TRUE), levels = categories),
  value = rnorm(100)
)
m <- leaflet(df) %>% addTiles()
m %>% addCircleMarkers(~lng, ~lat, radius = ~size)
m %>% addCircleMarkers(~lng, ~lat, radius = runif(100, 4, 10), color = c("red"))


#B) dygraphs: Time series
library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))

#C) plotly: A variety of plots, including maps
#Ver ejemplos anteriores
library(ggplot2)
library(plotly)
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)

#D) rbokeh: A variety of plots, including maps
library(rbokeh)
figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))


#Highcharter
library(magrittr)
library(highcharter)
highchart() %>% 
  hc_title(text = "Scatter chart with size and color") %>% 
  hc_add_series_scatter(mtcars$wt, mtcars$mpg,
                        mtcars$drat, mtcars$hp)

#E) networkD3: Network data
library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)

#F) d3heatmap: Heatmaps
library(d3heatmap)
d3heatmap(mtcars, scale="column", colors="Blues")

#G) DT: Data tables
library(DT)
datatable(iris, options = list(pageLength = 5))

#H) DiagrammeR: Diagrams and flowcharts
library(DiagrammeR)
grViz("
      digraph {
      layout = twopi
      node [shape = circle]
      A -> {B C D} 
      }")

#GRID PACKAGE
#A) GROBS
library(grid)
my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))

grid.draw(my_circle)

#
my_circle <- circleGrob(name = "my_circle",
                        x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))
grid.draw(my_circle)

my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
grid.draw(my_rect)

grid.edit("my_circle", gp = gpar(col = "red", lty = 1))

#
library(faraway)

wc_plot <- ggplot(worldcup, aes(x = Time, y = Passes)) + 
  geom_point()
grid.draw(wc_plot)

grid.draw(wc_plot)
grid.draw(my_circle)

#
wc_plot
grid.force()
grid.ls()

#
grid.edit("geom_point.points.102", gp = gpar(col = "red"))
grid.edit("GRID.text.118", gp = gpar(fontface = "bold"))


#
candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
lollipop <- gTree(children = gList(candy, stick))
grid.draw(lollipop)

#
grid.ls(lollipop)

#B) VIEWPORTS
grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5, 
                      width = 0.5, height = 0.5,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#
grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5, 
                      width = 0.5, height = 0.5,
                      just = c("center", "center"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#
grid.draw(rectGrob())
sample_vp <- viewport(x = 0.75, y = 0.75, 
                      width = 0.25, height = 0.25,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

# PROBLEMAS CON REGISTRO SE NECESITA UN API KEY
library(ggmap)

balt_counties <- map_data("county", region = "maryland") %>%
  mutate(our_counties = subregion %in% c("baltimore", "baltimore city"))
balt_map <- get_map("Baltimore County", zoom = 10) %>%
  ggmap(extent = "device") + 
  geom_polygon(data = filter(balt_counties, our_counties == TRUE),
               aes(x = long, y = lat, group = group),
               fill = "red", color = "darkred", alpha = 0.2)


#
maryland_map <- balt_counties %>%
  ggplot(aes(x = long, y = lat, group = group, fill = our_counties)) + 
  geom_polygon(color = "black") + 
  scale_fill_manual(values = c("white", "darkred"), guide = FALSE) + 
  theme_void() + 
  coord_map()

grid.draw(ggplotGrob(balt_map))
md_inset <- viewport(x = 0, y = 0, 
                     just = c("left", "bottom"),
                     width = 0.35, height = 0.35)
pushViewport(md_inset)
grid.draw(rectGrob(gp = gpar(alpha = 0.5, col = "white")))
grid.draw(rectGrob(gp = gpar(fill = NA, col = "black")))
grid.draw(ggplotGrob(maryland_map))
popViewport()

#C) GRID EXTRA PACKAGE
library(gridExtra)

time_vs_shots <- ggplot(worldcup, aes(x = Time, y = Shots)) + 
  geom_point()
player_positions <- ggplot(worldcup, aes(x = Position)) + 
  geom_bar()

grid.arrange(time_vs_shots, player_positions, ncol = 2)

#
grid.arrange(time_vs_shots, player_positions,
             layout_matrix = matrix(c(1, 2, 2), ncol = 3))


#
grid.arrange(time_vs_shots, player_positions,
             layout_matrix = matrix(c(1, NA, NA, NA, 2, 2), 
                                    byrow = TRUE, ncol = 3))


#
worldcup_table <- worldcup %>%
  filter(Team %in% c("Germany", "Spain", "Netherlands", "Uruguay")) %>%
  group_by(Team) %>%
  dplyr::summarize(`Average time` = round(mean(Time), 1),
                   `Average shots` = round(mean(Shots), 1)) %>%
  tableGrob()

grid.draw(ggplotGrob(time_vs_shots))
wc_table_vp <- viewport(x = 0.22, y = 0.85, 
                        just = c("left", "top"),
                        height = 0.1, width = 0.2)
pushViewport(wc_table_vp)
grid.draw(worldcup_table)
popViewport()

#BUILDING NEW GRAPHICAL ELEMENTS
#A) BULID NEW THEME
#ejemplo

library(ggplot2)
ggplot(data = mtcars, aes(x = disp, y = mpg)) + 
  geom_point() + 
  theme_classic()

#
x <- theme_get()
class(x)

#
new_theme <- theme_minimal()
theme_set(new_theme)

#Now your plots will use the theme_minimal() theme without you having to specify it.

ggplot(data = mtcars, aes(disp, mpg)) + 
  geom_point() + 
  facet_grid( . ~ gear)

#Perhaps the easiest thing to start with when customizing your own theme is
#to modify an existing theme (i.e. one that comes built-in to ggplot2). 
#In case your are interested in thoroughly exploring this area and learning 
#from others, there is also the ggthemes package on CRAN which provides 
#a number of additional themes for ggplot2.

newtheme <- theme_bw() + theme(plot.title = element_text(color = "darkred"))

#valor a modificar
newtheme$panel.border

#
newtheme <- newtheme + 
  theme(panel.border = element_rect(color = "green", size = 2))

#
library(faraway)
ggplot(data = worldcup, aes(Time, Shots)) + 
  geom_point() + 
  ggtitle("World Cup Data") + 
  newtheme

#
ggplot(data = worldcup, aes(Time, Shots)) + 
  geom_point() + 
  facet_wrap(facets = ~ Position, ncol = 2) + 
  ggtitle("World Cup Data") + 
  newtheme

#B) BUILD NEW GEOM

#New geoms in ggplot2 inherit from a top level class called 
#Geom and are constructed using a two step process.

#1) The ggproto() function is used to construct a new class 
#corresponding to your new geom. This new class specifies a 
#number of attributes and functions that describe how data 
#should be drawn on a plot.

#2) The geom_* function is constructed as a regular function.
#This function returns a layer to that can be added to a plot 
#created with the ggplot() function.


# GeomNEW <- ggproto("GeomNEW", Geom,
#                    required_aes = <a character vector of required aesthetics>,
#                    default_aes = aes(<default values for certain aesthetics>),
#                    draw_key = <a function used to draw the key in the legend>,
#                    draw_panel = function(data, panel_scales, coord) {
#                      ## Function that returns a grid grob that will 
#                      ## be plotted (this is where the real work occurs)
#                    }
# )


library(grid)
GeomMyPoint <- ggproto("GeomMyPoint", Geom,
                       required_aes = c("x", "y"),
                       default_aes = aes(shape = 1),
                       draw_key = draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)
                         
                         ## Let's print out the structure of the 'coords' object
                         str(coords)
                         
                         ## Construct a grid grob
                         pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = coords$shape
                         )
                       })


#
geom_mypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyPoint, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
ggplot(data = worldcup, aes(Time, Shots)) + geom_mypoint()

#Example: An Automatic Transparency Geom
GeomAutoTransparent <- ggproto("GeomAutoTransparent", Geom,
                               required_aes = c("x", "y"),
                               default_aes = aes(shape = 19),
                               draw_key = draw_key_point,
                               draw_panel = function(data, panel_scales, coord) {
                                 ## Transform the data first
                                 coords <- coord$transform(data, panel_scales)
                                 
                                 ## Compute the alpha transparency factor based on the
                                 ## number of data points being plotted
                                 n <- nrow(data)
                                 if(n > 100 && n <= 200)
                                   coords$alpha <- 0.3
                                 else if(n > 200)
                                   coords$alpha <- 0.15
                                 else
                                   coords$alpha <- 1
                                 ## Construct a grid grob
                                 grid::pointsGrob(
                                   x = coords$x,
                                   y = coords$y,
                                   pch = coords$shape,
                                   gp = grid::gpar(alpha = coords$alpha)
                                 )
                               })

#
geom_transparent <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE, 
                             show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomAutoTransparent, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
ggplot(data = worldcup, aes(Time, Shots)) + geom_transparent()

#
library(dplyr)
ggplot(data = sample_n(worldcup, 150), aes(Time, Shots)) +
  geom_transparent()

#
ggplot(data = sample_n(worldcup, 50), aes(Time, Shots)) + 
  geom_transparent()

#
ggplot(data = worldcup, aes(Time, Shots)) + 
  geom_transparent() + 
  facet_wrap(~ Position, ncol = 2) + 
  newtheme

#C) BUILD NEW STAT
#ejemplo

# StatNEW <- ggproto("StatNEW", Stat,
#                    compute_group = <a function that does computations>,
#                    default_aes = aes(<default values for certain aesthetics>),
#                    required_aes = <a character vector of required aesthetics>)



#Example: Normal Confidence Intervals
library(datasets)
library(dplyr)
data("airquality")
monthly <- dplyr::group_by(airquality, Month) %>%
  dplyr::summarize(ozone = mean(Ozone, na.rm = TRUE),
                   stderr = sd(Ozone, na.rm = TRUE) / sqrt(sum(!is.na(Ozone))))
monthly

#
ggplot(monthly, aes(x = Month, y = ozone)) + 
  geom_point() + 
  ylab("Ozone (ppb)")

#
StatConfint <- ggproto("StatConfint", Stat,
                       compute_group = function(data, scales) {
                         ## Compute the line segment endpoints
                         x <- data$x
                         xend <- data$x
                         y <- data$y - 1.96 * data$stderr
                         yend <- data$y + 1.96 * data$stderr
                         
                         ## Return a new data frame
                         data.frame(x = x, xend = xend,
                                    y = y, yend = yend)
                       },
                       required_aes = c("x", "y", "stderr")
)

#
stat_confint <- function(mapping = NULL, data = NULL, geom = "segment",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatConfInt, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
ggplot(data = monthly, aes(x = Month, y = ozone, stderr = stderr)) + 
  geom_point() + 
  ylab("Ozone (ppb)") + 
  geom_segment(stat = "confint")

#Combining Geoms and Stats
## This code is not runnable yet!
library(ggplot2)
library(datasets)
data(airquality)
mutate(airquality, Month = factor(Month)) %>%
  ggplot(aes(Month, Ozone)) + 
  geom_skinnybox()


#
StatSkinnybox <- ggproto("StatSkinnybox", Stat, 
                         compute_group = function(data, scales) {
                           probs <- c(0, 0.25, 0.5, 0.75, 1)
                           qq <- quantile(data$y, probs, na.rm = TRUE) 
                           out <- qq %>% as.list %>% data.frame
                           names(out) <- c("ymin", "lower", "middle", 
                                           "upper", "ymax")
                           out$x <- data$x[1]
                           out
                         },
                         required_aes = c("x", "y")
)

stat_skinnybox <- function(mapping = NULL, data = NULL, geom = "skinnybox",
                           position = "identity", show.legend = NA, 
                           outliers = TRUE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatSkinnybox, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(outliers = outliers, ...)
  )        
}

#
library(scales)
draw_panel_function <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) %>%
    mutate(lower = rescale(lower, from = panel_scales$y.range),
           upper = rescale(upper, from = panel_scales$y.range),
           middle = rescale(middle, from = panel_scales$y.range))
  med <- pointsGrob(x = coords$x,
                    y = coords$middle,
                    pch = coords$shape)
  lower <- segmentsGrob(x0 = coords$x,
                        x1 = coords$x,
                        y0 = coords$ymin,
                        y1 = coords$lower,
                        gp = gpar(lwd = coords$size))
  upper <- segmentsGrob(x0 = coords$x,
                        x1 = coords$x,
                        y0 = coords$upper,
                        y1 = coords$ymax,
                        gp = gpar(lwd = coords$size))
  gTree(children = gList(med, lower, upper))
}

GeomSkinnybox <- ggproto("GeomSkinnybox", Geom,
                         required_aes = c("x", "ymin", "lower", "middle", 
                                          "upper", "ymax"),
                         default_aes = aes(shape = 19, lwd = 2),
                         draw_key = draw_key_point,
                         draw_panel = draw_panel_function
)

#
geom_skinnybox <- function(mapping = NULL, data = NULL, stat = "skinnybox", 
                           position = "identity", show.legend = NA, 
                           na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(
    data = data, 
    mapping = mapping,
    stat = stat,
    geom = GeomSkinnybox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
mutate(airquality, Month = factor(Month)) %>%
  ggplot(aes(Month, Ozone)) + 
  geom_skinnybox()


#...........................#
#grafico imagen 1 (grafico de disperción)
library(plotly)

d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- plot_ly(
  d, x = ~carat, y = ~price,
  # Hover text:
  text = ~paste("Price: ", price, '$<br>Cut:', cut),
  color = ~carat, size = ~carat
)

#grafico imagen 2 (Histogramas)
p <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~rnorm(500)) %>%
  add_histogram(x = ~rnorm(500) + 1) %>%
  layout(barmode = "overlay")


#grafico imagen 3 (grafico de velas)
library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

# cutom colors
i <- list(line = list(color = '#FFD700'))
d <- list(line = list(color = '#0000ff'))

p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low,
          increasing = i, decreasing = d)



#grafico imagen 4 (grafico de la tierra)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/globe_contours.csv')
df$id <- seq_len(nrow(df))

library(tidyr)
d <- df %>%
  gather(key, value, -id) %>%
  separate(key, c("l", "line"), "\\.") %>%
  spread(l, value)

geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("grey90"),
  lakecolor = toRGB("white"),
  oceancolor = toRGB("white"),
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

p <- plot_geo(d) %>%
  group_by(line) %>%
  add_lines(x = ~lon, y = ~lat) %>%
  layout(
    showlegend = FALSE, geo = geo,
    title = 'Contour lines over globe<br>(Click and drag to rotate)'
  )



#grafico imagen 5 (mapa EEUU)
library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total.exports, text = ~hover, locations = ~code,
    color = ~total.exports, colors = 'Purples'
  ) %>%
  colorbar(title = "Millions USD") %>%
  layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )



