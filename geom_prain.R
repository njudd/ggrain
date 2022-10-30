#' Paired Raincloud Plot
#' Nicholas Judd (njudd), Jordy van Langen (jorvlan) & Rogier Kievit
#' 21/10/2022
#'
#'This function is only meant to make a two-by-two flanking raincloud
#'
#' https://github.com/easystats/see/blob/main/R/geom_violindot.R
#' 
#' https://ggplot2-book.org/spring1.html
#' https://testthat.r-lib.org/
#' need library(grid)
#' need library(rlang)
#' need library(ggplot2)


#############################################################
#############################################################


## UNCOMMENT THESE LINES TO MAKE IT WORK

# source("~/projects/rain/ggrain/geom-point-sorted.r")
# source("~/projects/rain/ggrain/utilities-grid.r")

# you should copy this and cite him
# devtools::source_url("https://raw.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R")

# library(rlang); library(grid)


geom_prain <- function(mapping = NULL,
                      data = NULL,
                      condition_by = NULL,
                      trim = TRUE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      id.long.var = NULL,
                      ...,
                      point.args = rlang::list2( 
                        alpha = .5,
                        ...
                      ),
                      point.args.pos = rlang::list2( 
                        position = position_jitter(
                          width = .02, 
                          height = NULL,
                          seed = 42)
                      ),
                      line.args = rlang::list2(
                        alpha = .2,
                        ...
                      ),
                      line.args.pos = rlang::list2(
                        position = position_jitter(
                          width = .02, 
                          height = NULL,
                          seed = 42)
                      ),
                      boxplot.args =  rlang::list2(
                        outlier.color = NA, # color = "black",
                        ...
                      ),
                      boxplot.args.pos =  rlang::list2(
                        width = .1,
                        position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1))
                      ),
                      violin.args = rlang::list2(
                        alpha = .3,
                        ...
                      ),
                      violin.args.pos = rlang::list2(
                        position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),
                                                        rep(.2, 256*2), rep(.2, 256*2)))
                      )
)
{
  
  point.args <- c(point.args, point.args.pos)
  line.args <- c(line.args, line.args.pos)
  boxplot.args <- c(boxplot.args, boxplot.args.pos)
  violin.args <- c(violin.args, violin.args.pos)
  
  if (is.null(id.long.var)){
    print("WARNING: If you want lines please give a id.long.var argument")
  }
  
  e1 <- rlang::exec(geom_point_sorted,
                    inherit.aes = TRUE, !!!point.args) # bang, bang, bang
  e3 <- rlang::exec(geom_boxplot, 
                      inherit.aes = TRUE, !!!boxplot.args)
  
  e4 <- rlang::exec(geom_paired_raincloud,
                      inherit.aes = TRUE, !!!violin.args)
  
  
  if (!is.null(id.long.var)){
    
    e2 <- rlang::exec(geom_line, aes(group = !!rlang::sym(id.long.var)), inherit.aes = TRUE, !!!line.args)
    
    e1 <- rlang::exec(geom_point_sorted, aes(group = !!rlang::sym(id.long.var)), inherit.aes = TRUE, !!!point.args) # bang, bang, bang
    
    list(e2, e4, e3, e1)

  }else{
    list(e4, e3, e1)
    
    
  }
}


# ggplot(temp2, aes(time, value, fill = group, color = group)) +
#   geom_prain() +
#   theme_minimal()


# this example is perrrrfect!!! 
# ggplot(temp2, aes(time, value, fill = group)) +
#   geom_point_sorted(aes(group = id), alpha = .2, # you need group = id for the points & lines to match
#                     position = position_jitter(
#                       width = .02, 
#                       height = NULL,
#                       seed = 42)) +
#   geom_line(aes(group = id), alpha = .2, # you need group = id for the points & lines to match
#     position = position_jitter(
#       width = .02, 
#       height = NULL,
#       seed = 42)) +
#   geom_boxplot(width = .1, color = "black", outlier.color = NA,
#                position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1))) +
#   geom_paired_raincloud(alpha = .3,
#                         position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),
#                                                         rep(.2, 256*2), rep(.2, 256*2)))) +
#   theme_minimal()
# 


# you need to comment the fuck out of this... 


# I think violin fails because position_nudge is applied to the whole ggplot dataframe
# you should do c(rep(-.2, 255*2), rep(.2, 255*2))
  
# one type of behaviour makes very little sense

# ggplot(temp2, aes(time, value, fill = group)) +
#   gghalves::geom_half_violin()
# 
# # they are seperated; "identity" or one nudge brings them back in place
# 
# ggplot(temp2, aes(time, value, fill = group)) +
#   gghalves::geom_half_violin(position = position_nudge(x = -.2))
# 
# 
# ggplot(temp2, aes(time, value, fill = group)) +
#   gghalves::geom_half_violin(position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),
#                                                              rep(.2, 256*2), rep(.2, 256*2))))
# 
