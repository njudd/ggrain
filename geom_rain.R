#' Raincloud Plot
#' Nicholas Judd (njudd), Jordy van Langen (jorvlan) & Rogier Kievit
#' 21/10/2022
#'
#'
#' https://github.com/easystats/see/blob/main/R/geom_violindot.R
#' 
#' https://ggplot2-book.org/spring1.html
#' https://testthat.r-lib.org/
#' need library(grid)
#' need library(rlang)
#' need library(ggplot2)



#############################################################
###### PRIORITY###### PRIORITY###### PRIORITY###### PRIORITY
#############################################################
# TO DO Major things denoted with *


# need a flanking geom_rainpaired() fucntion

# needs to do left right automatically


############# longitudinal conencted jittered lines (DONE)
# uses geom-point-sorted()
# works with an arg for id.long.var

############## jittering (partially finished)
# Issues of orientation is how to jitter & nudge?
# you could have a stat that jitters (already built)
# you can do the jittering as a stat for pointline and than do position_nudge()
# what about postion dodge, maybe that would automatically adjust to wider jitters

# this is current solved by ordering the data

############## orientation (incomplete)
# currently hardcoded yet if you use a function you will have the default args issues again

# idea do this with "width" have a default percentage 20/15/65 (dots ditter, box, vio)
# tricky as jittering is now tied to it
# than min & max widths; when max is hit break & tell about ggpointdensity


############## overlapping (not started)
# your function doesn't support overlapping violins like in rainclouds_2x2_repmes
# neither does geom_violin atm
# temp_g <- rbind(temp, temp)
# temp_g$g <- c(rep("a", dim(temp_g)[1]/2), rep("b", dim(temp_g)[1]/2))
# temp_g[1:(dim(temp_g)[1]/2),]$value <- temp_g[1:(dim(temp_g)[1]/2),]$value + 1
# 
# ggplot(temp_g, aes(time, value, fill = g)) +
#   geom_violin()

############## make a paired option where you have them flanking



############## supplementary features
# 1. use https://github.com/LKremer/ggpointdensity
# 2. make an easy arg for left sided

#############################################################
#############################################################


# so dots in center, with space for a left right arguement for the box & violin

# the jitter arguement will scale with width!
# or it is just a width argu for dots

# See if you can nudge differently depending on the itteration

# color & colour args
# https://stackoverflow.com/questions/60348226/is-there-an-r-function-to-connect-grouped-data-points-created-by-a-geom-objec


source("~/projects/rain/ggrain/geom-point-sorted.r")
source("~/projects/rain/ggrain/utilities-grid.r")

library(rlang); library(grid)
library(ggpp) # for position_jitternudge


geom_rain <- function(mapping = NULL,
                      data = NULL,
                      trim = TRUE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      id.long.var = NULL,
                      
                      rain.side = NULL,
                      rain.center = NULL,
                      rain.width = NULL,
                      ...,
                      # one con of doing it this way is when the user changes one it wipes them all out
                      # yet its okay they can look at the function & copy the args
                      # one issue is that we will have to interactively edit it for additional features (i.e., orientation)
                      # Jordy: look at best settings for position_jitter()
                      point.args = rlang::list2( 
                        position = position_jitter(
                          width = .04, 
                          height = NULL,
                          seed = 42),
                        ...
                      ),
                      line.args = rlang::list2(
                        alpha = .2,
                        position = position_jitter(
                          width = .04, 
                          height = NULL,
                          seed = 42),
                        ...
                      ),
                      boxplot.args =  rlang::list2(
                        center = TRUE, errorbar.draw = FALSE, outlier.shape = NA, 
                        side = "r",
                        width = .08, position = position_nudge(x = .08),
                        ...
                      ),
                      violin.args = rlang::list2(
                        color = NA, side = "r",
                        width = .7, position = position_nudge(x = .13), 
                        ...
                      )
)
{
  
  # rigth/left arguement
  # orient argument (sets what is 0)
  # rain width argument (needs to use normal width/jittersize & nudging)
  
  # the width arguement might not make sense, since its always to the scale of the plot
  
  
  
  if (!is.null(rain.side) && rain.side %in% c("r", "l")) {
    violin.args$side <- rain.side
    boxplot.args$side <- rain.side

    if(rain.side == "l"){
      violin.args$position$x <- -violin.args$position$x
      boxplot.args$position$x <- -boxplot.args$position$x
    }
    
  } else if (!is.null(rain.side)) {
    stop("ERROR: the rain.side arguement only accepts 'l' for left and 'r' for right \n STOPPING")
  }
  
  e1 <- rlang::exec(geom_point_sorted, inherit.aes = TRUE, !!!point.args) # bang, bang, bang
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
    
    e1 <- rlang::exec(geom_point_sorted, aes(group = !!rlang::sym(id.long.var)), inherit.aes = TRUE, !!!point.args) # bang, bang, bang
    
    list(e2, e4, e3, e1)
    # list(e2, e1)
    
  }else{
    list(e4, e3, e1)
  }
}


rain.center <- c("dots", "box", "violin") 



ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_point(aes(time, value, group = id),
             position = ggpp::position_jitternudge(width = .04,
                                                   seed = 42, x = .1,
                                                   nudge.from = "jittered"))




# to trouble shoot turn off e3 &e4

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  # geom_rain()
  geom_rain(rain.side = 'l')


# testing 
library(ggplot2)
temp <- lavaan::Demo.growth[,1:4]
temp$id <- as.factor(as.character(rep(1:dim(temp)[1])))
temp <- reshape2::melt(temp, id.vars = "id")
temp$value_round <- round(temp$value,1)
colnames(temp)[2] <- "time"

temp_subset10 <- temp[temp$id %in% as.character(1:10),]
temp2 <- temp[temp$variable %in% c("t1","t2"),] 
  #Jordy: View(temp2) results in -> "no data available in table"?

#### workspace
# time is hardcoded; also not working atm


ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_rain(id.long.var = 'id') #temp_subset10


ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_point_sorted(color = "blue", alpha = .5) +
  geom_point(color = "red", alpha = .5)



ggplot(temp, aes(time, value, fill = time)) + 
  geom_rain(id.long.var = 'id', rain.side = 'l',
            line.args = list(alpha = .05), alpha = .3) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") #+
  #coord_flip()


# making a plot for the best default

library(patchwork)

plt_temp_oneTP <- ggplot(temp |> dplyr::filter(time == "t1"), aes(time, value, fill = time))
plt_temp <- ggplot(temp, aes(time, value, fill = time))


plt_vio_wd.7 <- 
  geom_rain(
    alpha = .3,
    violin.args = rlang::list2(
    color = NA, side = "r",
    width = .7, position = position_nudge(x = .13)),
    boxplot.args =  rlang::list2(
      center = TRUE, errorbar.draw = FALSE, outlier.shape = NA, 
      width = .08, position = position_nudge(x = .11)))

plt_vio_wd.6 <- 
  geom_rain(
    alpha = .3,
    violin.args = rlang::list2(
      color = NA, side = "r",
      width = .6, position = position_nudge(x = .13)),
    boxplot.args =  rlang::list2(
      center = TRUE, errorbar.draw = FALSE, outlier.shape = NA, 
      width = .08, position = position_nudge(x = .11)))

plt_vio_wd.5 <- 
  geom_rain(
    alpha = .3,
    violin.args = rlang::list2(
      color = NA, side = "r",
      width = .5, position = position_nudge(x = .13)),
    boxplot.args =  rlang::list2(
      center = TRUE, errorbar.draw = FALSE, outlier.shape = NA, 
      width = .08, position = position_nudge(x = .11)))

plt_temp + plt_vio_wd.5 + plt_temp + plt_vio_wd.7

plt_temp_oneTP + plt_vio_wd.5 + plt_temp_oneTP + plt_vio_wd.7







#Jordy:
  # Violins on same axis-location (x-tic) so no lines overlap the violins?
  # Trimming of violins in both figures (lines: 188-190 & 192-198) differs? Why?

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_rain(data = temp_subset10,
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
  theme_minimal() #+
  #theme_classic()
  #Jordy:
    # default option: theme_classic()? To avoid conflicting line perception?





# current issue

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_line(aes(time, value, group = 'id'))






# "%||%" <- function(a, b) if (!is.null(a)) a else b


# https://stackoverflow.com/questions/70202220/splice-operator-for-ggplot2-geom-point-function
# https://github.com/tidyverse/ggplot2/issues/2675
# geom_point(aes(!!!point.args)) # might work in the new version as there's a merged branch


ggplot(temp, aes(x = variable, y = value, group = id)) +
  lemon::geom_pointline(alpha = .3)

# https://yjunechoe.github.io/posts/2020-07-13-geom-paired-raincloud/
ggplot(temp, aes(variable, value, fill = variable)) + 
  geom_paired_raincloud()

# exec(geom_point, data = ~ filter(.x, !isanoutlier), aes(color = {{ x }}), !!!point.args)

# flips based on x & y arg

## Iris dataset test 
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_rain() #+
  #coord_flip()

ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_rain() #+
#coord_flip()


## mpg dataset test
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() #+
  #coord_flip()

ggplot(data = mpg, mapping = aes(x = hwy, y = class)) + 
  geom_boxplot()
# potential resources:
# https://stackoverflow.com/questions/68733790/why-is-coord-flip-better-than-switching-x-and-y-arguments-in-aes-in-ggpl