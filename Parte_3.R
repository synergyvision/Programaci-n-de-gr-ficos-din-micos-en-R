################
# HTML WIDGETS #
################

# DIVERSOS PAQUETES A USAR #

#____________________#
#A) leaflet: Mapping #
#____________________#
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

#_________________________#
#B) Dygraphs: Time series #
#_________________________#
library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))

#______________________________________________#
#C) plotly: A variety of plots, including maps #
#______________________________________________#
library(ggplot2)
library(plotly)
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)

#______________________________________________#
#D) rbokeh: A variety of plots, including maps #
#______________________________________________#
library(rbokeh)
figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))

#_______________#
#E) Highcharter #
#_______________#
library(magrittr)
library(highcharter)
highchart() %>% 
  hc_title(text = "Scatter chart with size and color") %>% 
  hc_add_series_scatter(mtcars$wt, mtcars$mpg,
                        mtcars$drat, mtcars$hp)

#___________________________#
#F) networkD3: Network data #
#___________________________#
library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)

#_______________________#
#G) d3heatmap: Heatmaps #
#_______________________#
library(d3heatmap)
d3heatmap(mtcars, scale="column", colors="Blues")

#___________________#
#H) DT: Data tables #
#___________________#
library(DT)
datatable(iris, options = list(pageLength = 5))

#_______________________________________#
#I) DiagrammeR: Diagrams and flowcharts #
#_______________________________________#
library(DiagrammeR)
grViz("
      digraph {
      layout = twopi
      node [shape = circle]
      A -> {B C D} 
      }")


#################
# PAQUETE PLOTY #
#################

library( plotly )
p <- plot_ly (
  x = rnorm( 1000 ), y = rnorm( 1000 ), mode = 'markers' )
p

#_______________________________________#
# graficos basicos - graficos de lineas #
#_______________________________________#

plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7), type = 'scatter' , mode = 'lines' )

#________________#
# nube de puntos #
#________________#

plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7 ),
  type = 'scatter' , mode = 'markers' )

#___________________#
# graficos de barra #
#___________________#

plot_ly (
  x = c( 1, 2, 3),
  y = c( 5, 6, 7),
  type = 'bar' , mode = 'markers' )

#______________________#
# graficos de burbujas #
#______________________#

plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7 ), type = 'scatter' , mode = 'markers' , size = c( 1, 5, 10 ), marker = list(
    color = c( 'red', 'blue' , 'green' )))

#________________#
# mapas de calor #
#________________#

plot_ly (z=volcano,type='heatmap')

#__________________#
# graficos de area #
#__________________#

plot_ly (
  x = c( 1, 2, 3 ),
  y = c( 5, 6, 7 ), type = 'scatter' , mode = 'lines' , fill = 'tozeroy' )

#___________________________#
# layout (diseño) - leyenda #
#___________________________#

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

#______#
# ejes #
#______#

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

#_____________________________________#
# graficos estadisticos - histogramas #
#_____________________________________#

x <- rchisq ( 100, 5, 0 ) 
plot_ly (
  x=x,
  type = 'histogram' )

#___________#
# Box-plots #
#___________#

plot_ly (
  y = rnorm( 50 ) , type = 'box' ) %>%
  add_trace( y = rnorm( 50, 1 ))

#________________#
# Histogramas 2D #
#________________#

plot_ly (
  x = rnorm( 1000, sd = 10 ) , y = rnorm( 1000, sd = 5 ) , type = 'histogram2d' )

#____________________________#
# mapas - grafico de burbuja #
#____________________________#

plot_ly (
  type = 'scattergeo' , lon = c( -73.5, 151.2 ) , lat = c( 45.5, -33.8 ) , marker = list (
    color = c( 'red' , 'blue' ) , size = c( 30, 50 ) , mode = 'markers' ))

#________________#
# cloropleth map #
#________________#

plot_ly (
  type = 'choropleth' ,
  locations = c( 'AZ', 'CA', 'VT' ) , locationmode = 'USA-states' , colorscale = 'Viridis' ,
  z = c( 10, 20, 40 )) %>%
  layout ( geo = list ( scope = 'usa' ))

#___________________#
# grafico de puntos #
#___________________#

plot_ly (
  type = 'scattergeo' ,
  lon = c( 42, 39 ) ,
  lat = c( 12, 22 ) ,
  text = c( 'Rome' , 'Greece' ) , mode = 'markers' )

#_____________________________#
# graficos 3d -superficies 3D #
#_____________________________#

plot_ly (
  type = 'surface' , z = ~volcano )

#______________________#
# graficos de linea 3D #
#______________________#

plot_ly (
  type = 'scatter3d' , x = c( 9, 8, 5, 1 ) ,
  y = c( 1, 2, 4, 8 ) ,
  z = c( 11, 8, 15, 3 ) , mode = 'lines' )

#___________________#
# nube de puntos 3D #
#___________________#

plot_ly (
  type = 'scatter3d' , x = c( 9, 8, 5, 1 ) ,
  y = c( 1, 2, 4, 8 ) ,
  z = c( 11, 8, 15, 3 ) , mode = 'markers' )

#_______________________________#
# COMANDO IMPORTANTE - ggplotly #
#_______________________________#

ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)
ggiris

ggplotly(ggiris)

####################
# PAQUETE DYGRAPHS #
####################

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)

#_____________________#
# Panning and Zooming #
#_____________________#

dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector()

#____________________#
# Point Highlighting #
#____________________#

lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))

#_________________________#
# Shading and Annotations #
#_________________________#

dygraph(nhtemp, main="New Haven Temperatures") %>%
  dySeries(label="Temp (F)", color="black") %>%
  dyShading(from="1920-1-1", to="1930-1-1", color="#FFE6E6") %>%
  dyShading(from="1940-1-1", to="1950-1-1", color="#CCEBD6")

#_________#
# Example #
#_________#
# Read the data (hosted on the gallery website)
data=read.table("https://python-graph-gallery.com/wp-content/uploads/bike.csv", header=T, sep=",") %>% head(300)
str(data)

# Since my time is currently a factor, I have to convert it to a date-time format!
data$datetime = ymd_hms(data$datetime)

# Then you can create the xts format, and thus use dygraph
don=xts(x = data$count, order.by = data$datetime)
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

#_______________#
# Line and dots #
#_______________#

# Create data + verify it is date format + change them to xts format:
data=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), value=runif(41))
str(data$time)
data=xts(x = data$value, order.by = data$time)

# Default = line plot --> See chart #316

# Add points
dygraph(data) %>%
  dyOptions( drawPoints = TRUE, pointSize = 4 )

#____________#
# Area chart #
#____________#

dygraph(data) %>%
  dyOptions( fillGraph=TRUE )

#___________#
# Step plot #
#___________#

dygraph(data) %>%
  dyOptions( stepPlot=TRUE, fillGraph=TRUE)

#___________#
# Stem plot #
#___________#

dygraph(data) %>%
  dyOptions( stemPlot=TRUE)

#______________#
# Candle stick #
#______________#

# Create data (needs 4 data points per date stamp)
trend=sin(seq(1,41))+runif(41)
data=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), value1=trend, value2=trend+rnorm(41), value3=trend+rnorm(41), value4=trend+rnorm(41) )
data=xts(x = data[,-1], order.by = data$time)

# Plot it
dygraph(data) %>%
  dyCandlestick()

#__________________________#
# Line chart with interval #
#__________________________#

# Create data
trend=sin(seq(1,41))+runif(41)
data=data.frame(time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), trend=trend, max=trend+abs(rnorm(41)), min=trend-abs(rnorm(41, sd=1)))
data=xts(x = data[,-1], order.by = data$time)

# Plot
dygraph(data) %>%
  dySeries(c("min", "trend", "max"))

#####################
# GROBS Y VIEWPORTS #
#####################
library(grid)

#_______#
# GROBS #
#_______#

#____________________#
# circulo en gráfico #
#____________________#
my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))

grid.draw(my_circle)

#________________________________________#
# circulo (cambio de color) y rentangulo #
#________________________________________#

my_circle <- circleGrob(name = "my_circle",
                        x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))
grid.draw(my_circle)

my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
grid.draw(my_rect)

grid.edit("my_circle", gp = gpar(col = "red", lty = 1))

#_______________________________#
# Circulo en gráfico cualquiera #
#_______________________________#

library(faraway)

wc_plot <- ggplot(worldcup, aes(x = Time, y = Passes)) + 
  geom_point()
grid.draw(wc_plot)

grid.draw(wc_plot)
grid.draw(my_circle)

#___________________________________________________#
# Cambio de caracteristicas específicas del gráfico #
#___________________________________________________#

wc_plot
grid.force()
grid.ls()

# elijo caracteristicas a cambiar 
grid.edit("geom_point.points.1070", gp = gpar(col = "red"))
grid.edit("GRID.text.1100", gp = gpar(fontface = "bold"))

#_________#
# Chupeta #
#_________#

candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
lollipop <- gTree(children = gList(candy, stick))
grid.draw(lollipop)

# caracteristicas
grid.ls(lollipop)

#___________#
# VIEWPORTS #
#___________#

#___________________________________#
# Viewport extremo superior derecho #
#___________________________________#

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5, 
                      width = 0.5, height = 0.5,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#_________________#
# Viewport centro #
#_________________#

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5, 
                      width = 0.5, height = 0.5,
                      just = c("center", "center"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#___________________________________________#
# Viewport extremo superior derecho pequeño #
#___________________________________________#

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.75, y = 0.75, 
                      width = 0.25, height = 0.25,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()


#####################
# PAQUETE GRIDEXTRA #
#####################

library(grid)
library(gridExtra)
library(faraway)

#_____________________________#
# 2 graficos en 1 ventana (1) #
#_____________________________#

time_vs_shots <- ggplot(worldcup, aes(x = Time, y = Shots)) + 
  geom_point()
player_positions <- ggplot(worldcup, aes(x = Position)) + 
  geom_bar()

grid.arrange(time_vs_shots, player_positions, ncol = 2)

#_____________________________#
# 2 graficos en 1 ventana (2) #
#_____________________________#

grid.arrange(time_vs_shots, player_positions,
             layout_matrix = matrix(c(1, 2, 2), ncol = 3))

#_____________________________#
# 2 graficos en 1 ventana (3) #
#_____________________________#

grid.arrange(time_vs_shots, player_positions,
             layout_matrix = matrix(c(1, NA, NA, NA, 2, 2), 
                                    byrow = TRUE, ncol = 3))


#__________________#
# Tabla en grafico #
#__________________#

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






