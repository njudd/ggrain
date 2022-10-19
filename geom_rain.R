#' Half-violin Half-dot plot
#'
#' Create a half-violin half-dot plot, useful for visualising the distribution
#' and the sample size at the same time.
#' @inheritParams geom_violinhalf
#' @inheritParams ggplot2::geom_dotplot
#' @param position_dots Position adjustment for dots, either as a string, or the
#'   result of a call to a position adjustment function.
#' @param size_dots,dots_size Size adjustment for dots.
#' @param color_dots,dots_color Color adjustment for dots.
#' @param fill_dots,dots_fill Fill adjustment for dots.
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violindot() +
#'   theme_modern()
#' @import ggplot2
#' @export

#############################################################
#############################################################
# TO DO Major things denoted with *

# *change alpha in pointline!!!

# Issues of orientation is how to jitter & nudge?
# you could have a stat that jitters (already built)
# you can do the jittering as a stat for pointline and than do position_nudge()
# what about postion dodge, maybe that would automatically adjust to wider jitters

# tomorrow to do: sort out fill & color...
# line 247 might be the way...? you want it to default when there's nothing
# it should be able to take +
# scale_fill_brewer(palette = "Dark2") +
#   scale_color_brewer(palette = "Dark2")


# *x & y should make the orientation not just coord_flip()




#############################################################
#############################################################




# for two have them flanking but long should be the same direction
# geom_rainAU()



# so dots in center, with space for a left right arguement for the box & violin

# the jitter arguement will scale with width!
# or it is just a width argu for dots


# See if you can nudge differently depending on the itteration


# color & colour args
# https://stackoverflow.com/questions/60348226/is-there-an-r-function-to-connect-grouped-data-points-created-by-a-geom-objec

# this is just notes & working on stuff


# detach("package:gghalves", unload=TRUE)


# you need to think of what params you want to inheret

# maybe just layer! (but violin & boxplot need geom_bar params; maybe they are competing tho)
#' @inheritParams layer
#' @inheritParams geom_bar
#' 
#' # only export the %||% from tidyverse 
#' 
#' @keywords internal
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 

dev.off()

geom_rain2 <- function(mapping = NULL,
                    data = NULL,
                    trim = TRUE,
                    show.legend = NA,
                    inherit.aes = TRUE,
                    id.long.var = NULL,
                    ...,
                    point.args = rlang::list2(
                      position = position_jitter(
                        width = .02, 
                        height = NULL,
                        seed = 42),
                      ...
                    ),
                    line.args = rlang::list2(
                      ...
                    ),
                    boxplot.args =  rlang::list2(
                      center = TRUE, errorbar.draw = FALSE, outlier.shape = NA, 
                      width = .08, position = position_nudge(x = .1),
                      ...
                    ),
                    violin.args = rlang::list2(
                      color = NA,
                      position = position_nudge(x = .15), side = "r",
                      ...
                    )
)
{

  
  e1 <- rlang::exec(geom_point, inherit.aes = TRUE, !!!point.args) 
  e3 <- rlang::exec(gghalves::geom_half_boxplot, inherit.aes = TRUE, !!!boxplot.args)
  e4 <- rlang::exec(gghalves::geom_half_violin, inherit.aes = TRUE, !!!violin.args)
  
  
  if (!is.null(id.long.var)){
    # quo_id.long.var = rlang::sym(id.long.var)
    # https://stackoverflow.com/questions/50960339/create-ggplot2-function-and-specify-arguments-as-variables-in-data-as-per-ggplot
    
    e2 <- rlang::exec(geom_line, aes(group = !!rlang::sym(id.long.var)), inherit.aes = TRUE, !!!line.args)
    
    # you need false, but you need to take x & y with you!!!
    

    
    # https://github.com/tidyverse/ggplot2/issues/3535
    # redoing geom_point with ordered data
    # I don't think this will work because data isn't passed
    # now it works but I need to pass the data arg
    # also the args are quite verbose, can you trim them down/
    
    data <-
      data |>
      dplyr::arrange(!!rlang::sym(id.long.var), time)
    
    e1 <- rlang::exec(geom_point, data = data, inherit.aes = TRUE, !!!point.args) 

    # list(e2, e4, e3, e1)
    list(e2, e1)
    

  }else{
    
    list(e4, e3, e1)

  }
}

# to trouble shoot turn off e3 &e4

# now you need to get the lines & jittering matched... the jitter seed should do it?
# there's this odd jitter function with nudge (jitter nudge)

# your function doesn't support overlapping violins like in rainclouds_2x2_repmems
# neither does geom_violin atm
# temp_g <- rbind(temp, temp)
# temp_g$g <- c(rep("a", dim(temp_g)[1]/2), rep("b", dim(temp_g)[1]/2))
# temp_g[1:(dim(temp_g)[1]/2),]$value <- temp_g[1:(dim(temp_g)[1]/2),]$value + 1
# 
# ggplot(temp_g, aes(time, value, fill = g)) +
#   geom_violin()

# temp_subset10 |> dplyr::filter(time == "t1")

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_rain2(data = temp_subset10,
             alpha = .3, id.long.var = 'id', 
             point.args = rlang::list2(
               alpha = .3,
               position = position_jitter(
                 width = .02, height = NULL, seed = 42)),
             line.args = rlang::list2(
               alpha = .3,
               position = position_jitter(
                 width = .02, height = NULL, seed = 42))
             ) +
  theme_minimal()



ggplot(temp, aes(time, value, fill = time)) + 
  geom_rain2(alpha = .3, id.long.var = 'id', line.args = list(alpha = .05)) +
  theme_minimal()

# current issue

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_line(aes(time, value, group = 'id'))

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_line(aes(time, value, group = id))




"%||%" <- function(a, b) if (!is.null(a)) a else b



geom_rain <- function(mapping = NULL,
                      data = NULL,
                      trim = TRUE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      dots_size = 0.7,
                      dots_color = waiver(),
                      dots_fill = waiver(),
                      binwidth = 0.05,
                      position_dots = ggplot2::position_nudge(x = -0.025, y = 0),
                      ...,
                      
                      # alpha
                      global_alpha = NULL, # global alpha; local alpha's take presidence
                      dot_alpha = NULL, 
                      violin_alpha = NULL,
                      box_alpha = NULL,
                      side = NULL, # underdeveloped; arg for the sides
                      
                      
                      #colors (need to do colour)
                      #global_color = NA,
                      dot_color = NULL, 
                      violin_color = NULL,
                      box_color = NULL,
                      
                      # fill?
                      #dot_fill = NULL, #  # does point have a fill?
                      violin_fill = NULL,
                      box_fill = NULL,
                      
                      
                      
                      #longitudinal should be dependednt to each other in the future
                      long = aes(),
                      linecolor = NA
                      
                      # size_dots = dots_size,
                      # color_dots = dots_color,
                      # fill_dots = dots_fill
                      ) {
  
  
  
  
  # first thing do color args that are bilingual and friendly to scales()
  # you need to figure out how to do defaults
  
  
  # The most major issue is position_nudge() with groups, I would like to avoid subseting if possible
  # might not be possible...
  
  
  # aes_string() should solve the null issue!
  
  
  # some sort of spacing function for position_nudge() based from jittering
  # this should check the side arguement and nudge accordingly
  
  # also an orientation function would be nice that takes "violin", "box", or "dot" as arguments
  
  
  # one with !!!
  
  # from: https://github.com/IndrajeetPatil/ggstatsplot/blob/main/R/ggbetweenstats.R
  # point_args = list(
  #   #position = pj,
  #   alpha = 0.4,
  #   size = .1,
  #   stroke = 0
  # )
  # violin_args = list(
  #   width = 0.5,
  #   alpha = 0.2
  # )
  # box_args = list(
  #   width = 0.5,
  #   alpha = 0.2
  # )
  
  
  # dont think i need this but may be useful for "ort" function
  #scale = c("area", "count", "width"),
 # scale <- match.arg(scale) 
  
  # setting default if there are no local or global alpha args
  dot_alpha <- dot_alpha %||% global_alpha %||% .2
  violin_alpha <- violin_alpha %||% global_alpha %||% .4
  box_alpha <- box_alpha %||% global_alpha %||% .8
  
  side <- side %||% "l" # rep("l", length(!!color))
  
  # for color
  dot_color <- dot_color %||% "black"
  violin_color <- violin_color %||% "black"
  box_color <- box_color %||% "black"
  
  # for fill
  # dot_fill <- dot_fill %||% "black" # does point have a fill?
  violin_fill <- violin_fill %||% "black"
  box_fill<- box_fill %||% "black"
  
  
  
  # missing line connection alpha
  # do we want alpha on the edge of violins & box's? (difficulty ++)
  

  # they insert an arg either pj list
  # pj = ggpp::position_jitternudge(width=.8, height = 0, seed=42, x = 0)
  
  pj = position_jitter(width=0.04, height = 0, seed=9)
  
  # pj = position_jitterdodge(jitter.width=0.2, seed=9,
  #                            jitter.height = 0,
  #                            dodge.width = .05)

  print(mapping)
  
  #mm <- aes(group = id)
  point <- geom_point(mapping = mapping, data = data, 
                                     #color = "black", #size = size_dots, 
                                     #fill = dot_fill, # does this exist?
                                     # alpha = dot_alpha, linecolour = linecolor, # this will need a specific arg or you need to cancel it for the other two!
                                     position = pj) 
  line <- geom_line(mapping = long, data = data, 
                                     #color = "black", #size = size_dots, 
                                     #fill = dot_fill, # does this exist?
                                     # alpha = dot_alpha, linecolour = linecolor, # this will need a specific arg or you need to cancel it for the other two!
                                     position = pj) 
  # pointline <- lemon::geom_pointline(mapping = long, data = data, 
  #                                    #color = "black", #size = size_dots, 
  #                                    #fill = dot_fill, # does this exist?
  #                         alpha = dot_alpha, linecolour = linecolor, # this will need a specific arg or you need to cancel it for the other two!
  #                         position = pj) 
  violin <- gghalves::geom_half_violin(mapping = mapping, data = data,
                               #color = violin_color, fill = violin_fill, 
                               alpha = violin_alpha,
                               side = side,
                               #position = position_nudge(x = c(-.2, -.1, 0, 0))
                               position = position_nudge(x = -.15)) #+
  box <- gghalves::geom_half_boxplot(mapping = mapping, data = data,
                                center = TRUE, errorbar.draw = FALSE, outlier.shape = NA, 
                                side = side,
                                #colour = box_color, fill = box_fill, 
                                alpha = box_alpha,
                                width = .08, position = position_nudge(x = -.08))

  list(point, line, violin , box)
}


ggplot(temp, aes(x = 1, y = value)) +
  geom_rain(side = "r")
# you need to sort out single geom colors later


# 
ggplot(temp, aes(y = variable,x  = value)) +
  geom_boxplot() + coord_flip()


ggplot(temp, aes(x = variable, y = value, fill = variable, color = variable)) +
  geom_rain(dot_alpha= 1) +
  theme_minimal() +
  theme(legend.position='none') +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") #+ 
  #coord_flip() # not behaving as expected


ggplot(temp_subset10, aes(x = variable, y = value, fill = variable, color = variable)) +
  geom_rain(violin_alpha= 1, long = aes(group = id)) +
  theme_minimal() +
  theme(legend.position='none') +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")


ct = position_jitter(width=0.04, height = 0, seed=9)

pd <- position_dodge(0.4)


ggplot(temp_subset10, aes(x = variable, y = value, fill = variable, color = variable, group = id)) +
  geom_point(position = pd) +
  geom_line(position = pd)


# testing 
library(ggplot2)
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)
colnames(temp)[2] <- "time"

temp_subset10 <- temp[temp$id %in% as.character(1:10),]
temp2 <- temp[temp$variable %in% c("t1","t2"),]





ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_rain() +
  coord_flip()


ggplot(temp, aes(x = variable, y = value, fill = variable, color = variable)) +
  geom_rain(dot_alpha = .1, violin_alpha = .5, box_alpha = 1) +
  theme(legend.position='none', panel.background = element_rect(fill = "white", colour = "grey50"))
  

ggplot(temp2, aes(x = variable, y = value, fill = variable, color = variable)) +
  geom_rain(long = aes(group = id), linecolor = "black") +
  theme(legend.position='none', panel.background = element_rect(fill = "white", colour = "grey50"))


point.args = list(
  #position = pj,
  alpha = 0.4,
  size = .1,
  stroke = 0
)
# violin.args = list(
#   width = 0.5,
#   alpha = 0.2
# )

ggplot(temp, aes(x = variable, y = value)) +
         geom_point(aes(,,!!!point.args), position = pj) # https://stackoverflow.com/questions/70202220/splice-operator-for-ggplot2-geom-point-function
# https://github.com/tidyverse/ggplot2/issues/2675
# geom_point(aes(!!!point.args)) # might work in the new version as there's a merged branch


ggplot(temp, aes(x = variable, y = value, group = id)) +
  lemon::geom_pointline(linealpha = .3)



ggplot(temp, aes(variable, value, fill = variable)) + 
  geom_paired_raincloud()

exec(geom_point, data = ~ filter(.x, !isanoutlier), aes(color = {{ x }}), !!!point.args)







