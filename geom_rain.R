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


# so dots in center, with space for a left right arguement for the box & violin

# the jitter arguement will scale with width!
# or it is just a width argu for dots

# https://stackoverflow.com/questions/60348226/is-there-an-r-function-to-connect-grouped-data-points-created-by-a-geom-objec

# this is just notes & working on stuff


# detach("package:gghalves", unload=TRUE)


# you need to think of what params you want to inheret

# maybe just layer! (but violin & boxplot need geom_bar params; maybe they are competing tho)
#' @inheritParams layer
#' @inheritParams geom_bar

geom_rain <- function(mapping = NULL,
                      data = NULL,
                      trim = TRUE,
                      scale = c("area", "count", "width"),
                      show.legend = NA,
                      inherit.aes = TRUE,
                      dots_size = 0.7,
                      dots_color = NULL,
                      dots_fill = NULL,
                      binwidth = 0.05,
                      position_dots = ggplot2::position_nudge(x = -0.025, y = 0),
                      ...,
                      size_dots = dots_size,
                      color_dots = dots_color,
                      fill_dots = dots_fill) {
  
  # aes_string() should solve the null issue!
  
  
  # from: https://github.com/IndrajeetPatil/ggstatsplot/blob/main/R/ggbetweenstats.R
  point_args = list(
    #position = pj,
    alpha = 0.4,
    size = .1,
    stroke = 0
  )
  # violin_args = list(
  #   width = 0.5,
  #   alpha = 0.2
  # )
  # box_args = list(
  #   width = 0.5,
  #   alpha = 0.2
  # )
  
  
  
  
  
  scale <- match.arg(scale)
  
  pj <- position_jitterdodge(jitter.width=0.2, seed=9,
                             jitter.height = 0,
                             dodge.width = 0.05)
  
  pointline <- lemon::geom_pointline(mapping = mapping, data = data,
                                     size = size_dots, #color = color_dots,
                          alpha = .2, linecolour = NA, # this will need a specific arg or you need to cancel it for the other two!
                          position = pj) #, position = pj
  violin <- gghalves::geom_half_violin(mapping = mapping, data = data,
                               color = NA, alpha = .3,
                               side = c("l", "l", "l", "l"),
                               #position = position_nudge(x = c(-.2, -.1, 0, 0))
                               position = position_nudge(x = -.2)) #+
  box <- gghalves::geom_half_boxplot(mapping = mapping, data = data,
                                center = TRUE, errorbar.draw = FALSE, outlier.shape = NA,
                                width = .1, colour = "black", alpha = .1,
                                position = position_nudge(x = -.1))

  list(pointline, violin, box)
}  
  


# need to add a global arg & specific ones that take presidence
local %**% global %**% default





# testing 
library(ggplot2)
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)

temp_subset10 <- temp[temp$id %in% as.character(1:10),]

ggplot(temp, aes(x = variable, y = value, fill = variable, color = variable)) +
  geom_rain() +
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






exec(geom_point, data = ~ filter(.x, !isanoutlier), aes(color = {{ x }}), !!!point.args)










