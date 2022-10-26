#######play space #########


#### geom_rain.R is also kinda the same ####
GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
                           required_aes = c("x", "y"),
                           default_aes = aes(shape = 19, colour = "black"),
                           draw_key = draw_key_point,
                           
                           draw_panel = function(data, panel_params, coord) {
                             coords <- coord$transform(data, panel_params)
                             grid::pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(col = coords$colour)
                             )
                           }
)

geom_simple_point <- function(mapping = NULL, data = NULL, stat = StatJitPoint,
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_simple_point(alpha = .2)

# using StatJitPoint

p1 <- ggplot(iris[1:50,], aes(Sepal.Length, Sepal.Width)) +
  stat_jitpoint(jit_distance = .05) +
  geom_point(color = "red")

# making some example longitudinal data
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)

temp_subset10 <- temp[temp$id %in% as.character(1:10),]

p2 <- ggplot(temp_subset10, aes(variable, value_round)) +
  stat_jitpoint(jit_distance = .05) +
  geom_point(color = "red")

ggplot_build(p2)$data
# with the same seed it jitters the subjects the same!



############## ############## ############## ############## ##############
# 6-10-22 attempt
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# https://docs.r4photobiology.info/gginnards/

# making some example longitudinal data
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)

temp_subset10 <- temp[temp$id %in% as.character(1:10),]




library(ggplot2); library(rlang); library(gginnards)

source("~/projects/rain/ggrain/playspace_modified/geom-point_nkj.r")
source("~/projects/rain/ggrain/playspace_modified/stat_jitpointNKJ.r")
source("~/projects/rain/ggrain/playspace_modified/geom_pointpath_nkj.r")

# dependencies
source("~/projects/rain/ggrain/playspace_modified/utilities-grid.r") # for ggname
source("~/projects/rain/ggrain/playspace_modified/utilities.r") # for ggname


ggplot(temp, aes(variable, value_round)) + 
  geom_point_nkj(stat = StatJitPoint, jit_distance = .1)




# it works so now I want to just add the stats for a red median dot but not use them!

dim(ggplot_build(ggplot(temp, aes(variable, value_round)) + 
                    geom_point_nkj(stat = StatJitPoint, jit_distance = .3))$data[[1]])


head(ggplot_build(ggplot(temp, aes(variable, value_round)) + 
  geom_point_nkj(stat = StatJitPoint, jit_distance = .3))$data[[1]])

ggplot(temp, aes(variable, value_round)) +
  geom_pointpath_nkj()

ggplot(temp, aes(variable, value_round)) +
  lemon::geom_pointpath()

# Both geom_pointpath and geom_pointline will only connect observations within the same group! 
# However, if linecolour is not waiver(), connections will be made between groups, 
# but possible in an incorrect order.

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  lemon::geom_pointline()




pj <- position_jitterdodge(jitter.width=0.2, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.05)

ggplot(temp, aes(x = variable, y = value, fill = id, group = id, color = variable)) +
  lemon::geom_pointline(alpha = .2) + #, position = pj
  theme(legend.position='none')







ggplot(temp, aes(variable, value_round)) + 
  geom_point_nkj(stat = StatJitPoint, position = "dodge2", jit_distance = .3) +
  geom_debug()

ggplot(temp, aes(variable, value_round, group = id, color = variable))+ 
  lemon::geom_pointpath(stat = StatJitPoint)





ggplot(temp_subset10, aes(x = as.character(variable), y = value, colour = as.character(id))) +
  lemon::geom_pointline()


pj <- position_jitterdodge(jitter.width=0.2, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.05)

ggplot(temp, aes(x = variable, y = value, fill = id, group = id, color = variable)) +
  lemon::geom_pointline(alpha = .2, color = "black", linecolour = 'black',
                        position = pj) + #, position = pj
  gghalves::geom_half_violin(aes(group = variable, fill = variable), color = NA, alpha = .3,
                             side = c("l", "l", "l", "l"),
                             #position = position_nudge(x = c(-.2, -.1, 0, 0))
                             position = position_nudge(x = -.2)) +
  gghalves::geom_half_boxplot(aes(group = variable, fill = variable),
                              center = TRUE, errorbar.draw = FALSE, outlier.shape = NA,
                              width = .1, colour = "black", alpha = .1,
                              position = position_nudge(x = -.1)) +
  theme(legend.position='none', panel.background = element_rect(fill = "white", colour = "grey50")) 



ggplot(temp, aes(value, x=1))+ 
  lemon::geom_pointpath(linecolor = NA)



ggplot(temp, aes(variable, value_round)) + 
  #geom_point_nkj(stat = StatJitPoint, jit_distance = .3) +
  stat_jitpoint(summary.fun = dim, geom= "debug") +
  stat_jitpoint(geom = GeomPointnkj)

#############
# geom_dist anatomy

library(ggdist); library(ggplot2)

df <- data.frame(
  abc = c("a"),
  value = rnorm(300, c(1, 8, 8, 3), c(1, 1.5, 1.5, 1))) 

ct1 <- df |> 
  ggplot(aes(y = abc, x = value, fill = abc)) +
  #stat_slab(scale = .7)
  stat_slabinterval(aes(size = NULL),scale = 0.7, geom = "slab", show.legend = NA,outline_bars = FALSE, 
                    show_point = FALSE,
                    show_interval = T
  ) # aes(thickness = stat(pdf*n)), scaled the size by the number of observations
#stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA) 

ct2 <- df |> 
  ggplot(aes(y = abc, x = value, fill = abc)) +
  #stat_slab(scale = 0.7) # aes(thickness = stat(pdf*n)), scaled the size by the number of observations
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA)


ct1 <- ggplot_build(ct1)$data
dim(ct1[[1]])
unique(ct1[[1]]$datatype)

ct2 <- ggplot_build(ct2)$data
dim(ct2[[1]])



########
# https://stackoverflow.com/questions/53931252/extending-ggplot2-how-to-build-a-geom-and-stat?noredirect=1&lq=1

# Inside geom-.r and stat-.r there are many very useful comments that clarify 
# how geoms and stats work. In particular (hat tips Claus Wilke over at github issues):

# https://github.com/tidyverse/ggplot2/blob/main/R/geom-.r
# https://github.com/tidyverse/ggplot2/blob/main/R/stat-.r



#########
# https://stackoverflow.com/questions/36156387/how-to-make-a-custom-ggplot2-geom-with-multiple-geometries?rq=1


library(ggplot2)
library(proto)
library(grid)

GeomManythings <- ggproto(
  "GeomManythings",
  Geom,
  setup_data = function(self, data, params) {
    data <- ggproto_parent(Geom, self)$setup_data(data, params)
    data
  },
  
  draw_group = function(data, panel_scales, coord) {
    n <- nrow(data)
    if (n <= 2)
      return(grid::nullGrob())
    
    
    # polygon hull for all points
    hull_df <-  data[chull(data[,c("x", "y")]), ]
    
    hull_grob <-
      GeomPolygon$draw_panel(hull_df, panel_scales, coord)
    
    # polygon hull for subset
    subset_of_x <-
      data[data$x > 0 & data$y > 0 ,]
    hull_of_subset_df <-subset_of_x[chull(subset_of_x[,c("x", "y")]),]
    hull_of_subset_df$fill <- "red" # testing
    hull_of_subset_grob <-  GeomPolygon$draw_panel(hull_of_subset_df, panel_scales, coord)
    
    coords <- coord$transform(data, panel_scales)     
    
    pg <- pointsGrob(x=mean(coords$x), y=mean(coords$y), 
                     default.units = "npc", gp=gpar(col="green", cex=3))
    
    ggplot2:::ggname("geom_mypolygon",
                     grobTree(hull_grob,
                              hull_of_subset_grob, pg))
    
    
  },
  
  
  required_aes = c("x", "y"),
  
  draw_key = draw_key_polygon,
  
  default_aes = aes(
    colour = "grey20",
    fill = "grey50",
    size = 0.5,
    linetype = 1,
    alpha = 0.5
  )
)

geom_manythings <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      geom = GeomManythings,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }


set.seed(9)
n <- 20
d <- data.frame(x = rnorm(n),
                y = rnorm(n))

ggplot(d, aes(x, y)) +
  geom_manythings()+
  geom_point()



######## Week 42 ########
# make a function like see::


# making some example longitudinal data
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)

temp_subset10 <- temp[temp$id %in% as.character(1:10),]

pj <- position_jitterdodge(jitter.width=0.2, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.05)

ggplot(temp_subset10, aes(x = variable, y = value, fill = variable, color = variable)) +
  lemon::geom_pointline(aes(group = id), alpha = .2, color = "black", linecolour = NA,
                        position = pj) + #, position = pj
  gghalves::geom_half_violin(color = NA, alpha = .3,
                             side = c("l", "l", "l", "l"),
                             #position = position_nudge(x = c(-.2, -.1, 0, 0))
                             position = position_nudge(x = -.2)) +
  gghalves::geom_half_boxplot(center = TRUE, errorbar.draw = FALSE, outlier.shape = NA,
                              width = .1, colour = "black", alpha = .1,
                              position = position_nudge(x = -.1)) +
  theme(legend.position='none', panel.background = element_rect(fill = "white", colour = "grey50"))


# this way does the jitter consistent
# check_da_jit <- ggplot(temp_subset10, aes(x = variable, y = value, fill = id, group = id, color = variable)) +
#   lemon::geom_pointline(alpha = .2, color = "black", linecolour = 'black',
#                         position = pj) + #, position = pj
#   theme(legend.position='none', panel.background = element_rect(fill = "white", colour = "grey50")) 
# 
# check_da_jit <- ggplot_build(check_da_jit)$data[[1]]
# 
# check_da_jit |> 
#   filter(group == 1)


check_da_jit <- ggplot(temp_subset10, aes(y = variable, x = value, fill = id, group = id, color = variable)) +
  lemon::geom_pointline(alpha = .2, color = "black", linecolour = 'black',
                        position = pj) + #, position = pj
  gghalves::geom_half_violin(aes(group = variable, fill = variable), color = NA, alpha = .3,
                             side = c("l", "l", "l", "l"),
                             #position = position_nudge(x = c(-.2, -.1, 0, 0))
                             position = position_nudge(x = -.2)) +
  theme(legend.position='none', panel.background = element_rect(fill = "white", colour = "grey50"))


# you can coord flip, but its better if geom_halves can take x & y and know

# update geoms from gghalves; inspired by geom_boxplot

ggplot(temp_subset10, aes(y = variable, x = value)) + gghalves::geom_half_boxplot()

# this is some dumb behaviour

# GOAL to make a flipping geom_half_boxplot() & geom_half_violin

# position_nudge with a vector
# https://stackoverflow.com/questions/54672468/ggplot2-how-to-nudge-the-position-of-points-in-geom-beeswarm
# check out the position nudge_any function at the bottom


# geom_rainflanked could do the subsetting that jordy does, 
# yet do it automatic with counted groups

# TODAY's GOAL
# 1) make one, just make a function that is geom_rain
# it should group & be able to extend you can hardcode stuff 
# but make sure that each thing is coded 

# jitter seed, col.vio, fill.vio, same for box, point & line

# 2) set alpha on lines from geom_pointline()


######## Oct 18th goal to make lemon::geom_pointline with line alpha



# boxplot.args = list(width = 0.2, alpha = 0.5, fill = NULL)
# 
# boxplot.args = boxplot.args[!is.null(boxplot.args)]
# 
# p <- ggplot(mpg, aes(class, hwy))
# p + exec(geom_boxplot, aes(class, hwy, color = class), inherit.aes = FALSE, !!!boxplot.args)


temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)

temp_subset10 <- temp[temp$id %in% as.character(1:10),]



lemon::geom_pointline()

library(ggplot2)


ggplot(temp_subset10 |> dplyr::filter(time == "t1"), 
       aes(variable, value, group = id)) +
  lemon::geom_pointline()

####### playspace stat identity sorted

# data
# library(ggplot2)
# temp <- lavaan::Demo.growth[,1:4]
# temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
# temp <- reshape2::melt(temp, id.vars = "id")
# temp$value_round <- round(temp$value,1)
# colnames(temp)[2] <- "time"
# 
# temp_subset10 <- temp[temp$id %in% as.character(1:10),]
# temp2 <- temp[temp$variable %in% c("t1","t2"),]


s1 <- ggplot(temp_subset10, aes(time, value, color = time, fill = time)) +
  ggplot2::geom_point(stat = StatIdentitySorted)


ggplot_build(s1)$data


library(rlang); library(ggplot2); library(grid)
pj = position_jitter(
  width = .2, 
  height = NULL,
  seed = 42)
  
  
ggplot(temp_subset10, aes(time, value, fill = time, group = id)) +
  geom_point_sorted(position = pj) +
  geom_line(position = pj)

ggplot(temp_subset10, aes(time, value, fill = time, group = id)) +
  geom_point(position = pj) +
  geom_line(position = pj)


ggplot_build(s1)$data


ggplot(temp_subset10, aes(time, value, fill = time, group = id)) +
  geom_point(position = pj) +
  geom_point_sorted(position = pj,
                    alpha = .3, color = "red")







#########
# working on 26/10/22


source("~/projects/rain/ggrain/stat-half-ydensity.R")
source("~/projects/rain/ggrain/geom_half_violin.R")

ggplot(iris, aes(y= Sepal.Length)) +
  geom_half_violin()

























