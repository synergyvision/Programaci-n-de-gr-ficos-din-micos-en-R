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




####################
# PAQUETE DYGRAPHS #
####################

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






