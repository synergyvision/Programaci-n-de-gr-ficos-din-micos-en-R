#############################################
# CONSTRUCCION DE NUEVOS ELEMENTOS GRAFICOS #
#############################################

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








#_______________________________#
# Construcción de un nuevo tema #
#_______________________________#

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

#___________________#
# valor a modificar #
#___________________#

newtheme$panel.border

#__________#
# modifico #
#__________#

newtheme <- newtheme + 
  theme(panel.border = element_rect(color = "green", size = 2))

#___________________#
# Uso de nuevo tema #
#___________________#

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


#_______________________________#
# Construcción de un nuevo Geom #
#_______________________________#

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

#_________________________________________#
# Example: An Automatic Transparency Geom #
#_________________________________________#

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

#_____________#
# uso de Geom #
#_____________#

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

#_______________________________#
# Construcción de un nuevo Stat #
#_______________________________#
#ejemplo

# StatNEW <- ggproto("StatNEW", Stat,
#                    compute_group = <a function that does computations>,
#                    default_aes = aes(<default values for certain aesthetics>),
#                    required_aes = <a character vector of required aesthetics>)


#______________________________________#
# Example: Normal Confidence Intervals #
#______________________________________#

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

#_____________#
# Uso de Stat #
#_____________#

ggplot(data = monthly, aes(x = Month, y = ozone, stderr = stderr)) + 
  geom_point() + 
  ylab("Ozone (ppb)") + 
  geom_segment(stat = "confint")


#____________________#
# Uso de Geom + Stat #
#____________________#

#Combining Geoms and Stats
## This code is not runnable yet!
library(ggplot2)
library(datasets)
data(airquality)
mutate(airquality, Month = factor(Month)) %>%
  ggplot(aes(Month, Ozone)) + 
  geom_skinnybox()

#___________#
# Creo Stat #
#___________#

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

#___________#
# Creo GEOM #
#___________#

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

#____________________#
# Uso de Geom + Stat #
#____________________#

mutate(airquality, Month = factor(Month)) %>%
  ggplot(aes(Month, Ozone)) + 
  geom_skinnybox()


