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

############## jittering (partially finished)
# Issues of orientation is how to jitter & nudge?
# you could have a stat that jitters (already built)
# you can do the jittering as a stat for pointline and than do position_nudge()
# what about postion dodge, maybe that would automatically adjust to wider jitters

# this is current solved by ordering the data

############## orientation (incomplete)
# currently hardcoded yet if you use a function you will have the default args issues again


############## overlapping (not started)
# your function doesn't support overlapping violins like in rainclouds_2x2_repmems
# neither does geom_violin atm
# temp_g <- rbind(temp, temp)
# temp_g$g <- c(rep("a", dim(temp_g)[1]/2), rep("b", dim(temp_g)[1]/2))
# temp_g[1:(dim(temp_g)[1]/2),]$value <- temp_g[1:(dim(temp_g)[1]/2),]$value + 1
# 
# ggplot(temp_g, aes(time, value, fill = g)) +
#   geom_violin()

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
    # also the args are quite verbose, can you trim them down
    # CHECK OUT JITTER_NUDGE()
    
    if(is.null(data)){
      stop("WARNING you need to specify a data arg in geom_rain for longitudinally connected plots: \n(e.g., geom_rain(data = x, id.long.var = 'id', ...)")
    }
    
    data <-
      data |>
      dplyr::arrange(!!rlang::sym(id.long.var), time)
    
    e1 <- rlang::exec(geom_point, data = data, inherit.aes = TRUE, !!!point.args) 

    list(e2, e4, e3, e1)
    # list(e2, e1)

  }else{
    list(e4, e3, e1)
  }
}

# to trouble shoot turn off e3 &e4

# temp_subset10 |> dplyr::filter(time == "t1")

# testing 
library(ggplot2)
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)
colnames(temp)[2] <- "time"

temp_subset10 <- temp[temp$id %in% as.character(1:10),]
temp2 <- temp[temp$variable %in% c("t1","t2"),]



#### workspace
# time is hardcoded; also not working atm

ggplot(temp, aes(time, value, fill = time)) + 
  geom_rain2(#data = temp,
             alpha = .3, id.long.var = 'id', line.args = list(alpha = .05)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")



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





# current issue

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_line(aes(time, value, group = 'id'))

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_line(aes(time, value, group = id))




# "%||%" <- function(a, b) if (!is.null(a)) a else b


# https://stackoverflow.com/questions/70202220/splice-operator-for-ggplot2-geom-point-function
# https://github.com/tidyverse/ggplot2/issues/2675
# geom_point(aes(!!!point.args)) # might work in the new version as there's a merged branch


ggplot(temp, aes(x = variable, y = value, group = id)) +
  lemon::geom_pointline(linealpha = .3)

ggplot(temp, aes(variable, value, fill = variable)) + 
  geom_paired_raincloud()

# exec(geom_point, data = ~ filter(.x, !isanoutlier), aes(color = {{ x }}), !!!point.args)







