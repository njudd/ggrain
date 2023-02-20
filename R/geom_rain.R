#' Raincloud Plots
#'
#' This function displays individual data points, a boxplot and half a violin plot.
#' It also has the option to connect data points with lines across groups by specifying
#' an id to connect by. Lastly, if desired one can color the dots based of another variable.
#'
#' @name geom_rain
#' @inheritParams ggplot2::geom_boxplot
#' @param id.long.var A group to connect the lines by - must be a string (e.g., "id").
#' @param cov A covariate to color the dots by - must be as a string (e.g., "cov")
#' @param rain.side How you want the rainclouds displayed, right ("r"), left ("l") or flanking ("f"), for a 1-by-1 flanking raincloud use ("f1x1") and for a 2-by-2 use ("f2x2").
#' @param likert Currently developing, right now just addes y-jitter.
#' @param seed For the jittering in point & line to match. 
#' @param point.args A list of args for the dots
#' @param point.args.pos A list of positional args for the points
#' @param line.args A list of args for the lines, you need to specify a group to connect them with id.long.var
#' @param line.args.pos A list of positional args for the lines
#' @param boxplot.args A list of args for the boxplot
#' @param boxplot.args.pos A list of positional args for the boxplot
#' @param violin.args A list of args for the violin
#' @param violin.args.pos A list of positional args for the violin
#' @return Returns a list of three environments to be used with the 'ggplot()' function in the 'ggplot2' package.
#' @return If the id.long.var argument is used the output will be a list of 4 environments.
#' @return These 4 environments have a similar structure to 'geom_boxplot()', 'geom_violin()', 'geom_point()' and 'geom_line()' from 'ggplot2'.
#' need library(rlang)
#' need library(ggplot2)
#' depends = ggplot2
#' @importFrom ggplot2 aes
#' @importFrom gghalves geom_half_violin
#' @importFrom rlang list2 sym !! !!! exec
#' @importFrom ggpp position_dodgenudge
#' @export
#' @references Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R.,
#' van Langen, J., & Kievit, R. A.
#' Raincloud plots: a multi-platform tool for robust data visualization
#' Wellcome Open Research 2021, 4:63.
#' https://doi.org/10.12688/wellcomeopenres.15191.2
#'
#' @examples
#' e1 <- ggplot(iris, aes(Species, Sepal.Width, fill = Species))
#' e1 + geom_rain()
#'
#' # x must be the discrete variable
#' # orinetation can be changed with coord_flip()
#' e1 + geom_rain(alpha = .5) + coord_flip()
#'
#' # we can color the dots by a covariate
#' e1 + geom_rain(cov = "Sepal.Length")
#'
#' # we can edit elements individually
#' e1 + geom_rain(violin.args = list(alpha = .3, color = NA))
#'
#' # we can flip them
#' e1 + geom_rain(rain.side = 'l')
#' # and move them
#' e1 +
#' geom_rain(boxplot.args.pos = list(width = .1, position = position_nudge(x = -.2)))
#'
#' # they also work longitudinally
#' e2 <- ggplot(sleep, aes(group, extra, fill = group))
#' e2 + geom_rain(id.long.var = "ID")
#'
#' # we can add groups
#' sleep_dat <- cbind(sleep, data.frame(sex = c(rep("male", 5),
#' rep("female", 5), rep("male", 5), rep("female", 5))))
#' e3 <- ggplot(sleep_dat, aes(group, extra, fill = sex))
#' e3 + geom_rain(alpha = .6)
#'
#' # add likert example
#' e4 <- ggplot(mpg, aes(1, hwy, fill = manufacturer))
#' e4 + geom_rain(likert= TRUE)
#'
#' # lets make it look nicer
#' e4 + geom_rain(likert= TRUE,
#'  boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .095), width = .1),
#'  violin.args = list(color = NA, alpha = .5))


geom_rain <- function(mapping = NULL,
                      data = NULL,
                      #show.legend = NA, # you can put this in the args for geom's!
                      inherit.aes = TRUE,
                      id.long.var = NULL, # should lines be drawn & what should connect them?
                      cov = NULL, # should dots be colored due to a covariate?
                      rain.side = NULL, # The side to draw the violin/boxplot. "l" for left, "r" for right, "f" for flanking, defaults to "r"
                      likert = FALSE, #make sure you don't need to do more in the long for loop area
                      seed = 42, # for the jittering in point & line to match
                      # rain.center = NULL, currently not implimented
                      ...,
                      point.args = rlang::list2(
                        #alpha = .8#,
                        ...
                      ),
                      point.args.pos = rlang::list2(
                        position = position_jitter(
                          width = .04,
                          height = 0,
                          seed = seed)
                      ),
                      line.args = rlang::list2(
                        alpha = .2,
                        ...
                      ),
                      line.args.pos = rlang::list2(
                        position = position_jitter(
                          width = .04,
                          height = 0,
                          seed = seed),
                      ),
                      boxplot.args =  rlang::list2(
                        outlier.shape = NA,
                        ...
                      ),
                      boxplot.args.pos =  rlang::list2(
                        width = .05,
                        position = position_nudge(x = .10),
                      ),
                      violin.args = rlang::list2( # color = NA, this default is bad incase they use group = x, or color = x
                        ...
                      ),
                      violin.args.pos = rlang::list2(
                        side = "r", width = .7,
                        position = position_nudge(x = .15),
                      )
)
{


  # if rain.side == "paired" check if the defaults are used;
  # if so rewrite to paired nudging args; else take their args

  # rigth/left arguement
  # orient argument (sets what is 0)
  # rain width argument (needs to use normal width/jittersize & nudging)


  # if it is not null go through options, if it is none that throw error
  # yet if it is null just continue...

  if (!is.null(rain.side)) {
    if (rain.side == "r") {
      violin.args.pos$side <- rain.side # this is not needed as it is default
    }
    else if (rain.side == "l") {
      violin.args.pos$side <- rain.side
      violin.args.pos$position$x <- -violin.args.pos$position$x
      boxplot.args.pos$position$x <- -boxplot.args.pos$position$x
    }
    else if (rain.side == "f") {
      if ("side" %in% names(violin.args.pos)){

        warning("Option rain.side 'flanking' is being used with a side argument in violin.args.pos!!!
      \n
      If you want the nudging position defaults for a flanking 1-by-1 raincloud use (rain.side = 'f1x1')\n
      If you want the nudging position defaults for a flanking 2-by-2 raincloud use (rain.side = 'f2x2')\n

      Now defaulting to a 2-by-2", call. = FALSE)

        boxplot.args.pos <- rlang::list2(
          width = .08,
          position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1)))
        violin.args.pos <- rlang::list2(
          width = .7,
          position = position_nudge(x = c(rep(-.15, 256*2), rep(-.15, 256*2),
                                          rep(.15, 256*2), rep(.15, 256*2))))
      }
    }
    else if (rain.side == "f1x1") {
      boxplot.args.pos <- rlang::list2(
        width = .08,
        position = ggpp::position_dodgenudge(x = c(-.1, .1)))
      violin.args.pos <- rlang::list2(
        width = .7,
        position = position_nudge(x = c(rep(-.15, 256*2), rep(.15, 256*2))))
    }
    else if (rain.side == "f2x2") {
      boxplot.args.pos <- rlang::list2(
        width = .08,
        position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1)))
      violin.args.pos <- rlang::list2(
        width = .7,
        position = position_nudge(x = c(rep(-.15, 256*2), rep(-.15, 256*2),
                                        rep(.15, 256*2), rep(.15, 256*2))))
    } else
      stop("the rain.side arguement only accepts: \n 'l' for left \n 'r' for right \n 'f', 'f1x1', or 'f2x2' for flanking
           \n STOPPING", call. = FALSE)
  }

  # doing positional changes based off user input
  # if (!is.null(rain.side) && rain.side %in% c("r", "l")) {
  #   violin.args.pos$side <- rain.side
  #
  #   if(rain.side == "l"){
  #     violin.args.pos$position$x <- -violin.args.pos$position$x
  #     boxplot.args.pos$position$x <- -boxplot.args.pos$position$x
  #   }
  #
  # }
  # else if (!is.null(rain.side) && rain.side == "f"){
  #
  #   if ("side" %in% names(violin.args.pos)){
  #
  #     warning("Option rain.side 'flanking' is being used with a side argument in violin.args.pos!!!
  #     \n
  #     If you want the nudging position defaults for a flanking 1-by-1 raincloud use (rain.side = 'f1x1')\n
  #     If you want the nudging position defaults for a flanking 2-by-2 raincloud use (rain.side = 'f2x2')\n
  #
  #     Now defaulting to a 2-by-2", call. = FALSE)
  #
  #     boxplot.args.pos <- rlang::list2(
  #       width = .08,
  #       position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1)))
  #     violin.args.pos <- rlang::list2(
  #       width = .7,
  #       position = position_nudge(x = c(rep(-.15, 256*2), rep(-.15, 256*2),
  #                                       rep(.15, 256*2), rep(.15, 256*2))))
  #   }
  # } else if (!is.null(rain.side) && rain.side == "f1x1"){
  #
  #     boxplot.args.pos <- rlang::list2(
  #       width = .08,
  #       position = ggpp::position_dodgenudge(x = c(-.1, .1)))
  #     violin.args.pos <- rlang::list2(
  #       width = .7,
  #       position = position_nudge(x = c(rep(-.15, 256*2), rep(.15, 256*2))))
  #
  # } else if (!is.null(rain.side) && rain.side == "f2x2"){
  #
  #     boxplot.args.pos <- rlang::list2(
  #       width = .08,
  #       position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1)))
  #     violin.args.pos <- rlang::list2(
  #       width = .7,
  #       position = position_nudge(x = c(rep(-.15, 256*2), rep(-.15, 256*2),
  #                                       rep(.15, 256*2), rep(.15, 256*2))))
  #
  # } else if (!is.null(rain.side)) {
  #   stop("ERROR: the rain.side arguement only accepts 'l' for left, 'r' for right and 'f', 'f1x1', or 'f2x2' for flanking \n STOPPING", call. = FALSE)
  # }
  # else

  # likert option doign y-jittering
  if (likert == TRUE) {
    point.args.pos$position$height = .1
    line.args.pos$position$height = .1
    message("Likert = T; setting y axis jittering for point & line to .1")
    }



  # combining args with position args
  point.args <- c(point.args, point.args.pos)
  line.args <- c(line.args, line.args.pos)
  boxplot.args <- c(boxplot.args, boxplot.args.pos)
  violin.args <- c(violin.args, violin.args.pos)

  if(!is.null(cov)){ # remap the color in e1; if a covariate is specified
    e1 <- rlang::exec(geom_point_sorted, aes(color = !!rlang::sym(cov)), inherit.aes = TRUE, !!!point.args) # bang, bang, bang
  }
  else {
    e1 <- rlang::exec(geom_point_sorted, inherit.aes = TRUE, !!!point.args) # bang, bang, bang
  }

  e3 <- rlang::exec(geom_boxplot, inherit.aes = TRUE, !!!boxplot.args)

  if(!is.null(rain.side) && rain.side %in% c("f", "f1x1", "f2x2")){

    e4 <- rlang::exec(geom_paired_raincloud, inherit.aes = TRUE, !!!violin.args)
  }
  else {
    e4 <- rlang::exec(gghalves::geom_half_violin, inherit.aes = TRUE, !!!violin.args)
  }

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

    if(!is.null(cov)){ # remap the color in e1 to the covariate
      e1 <- rlang::exec(geom_point_sorted, aes(group = !!rlang::sym(id.long.var), color = !!rlang::sym(cov)), inherit.aes = TRUE, !!!point.args)
    }
    else {
      e1 <- rlang::exec(geom_point_sorted, aes(group = !!rlang::sym(id.long.var)), inherit.aes = TRUE, !!!point.args)
    }
    list(e2, e4, e3, e1)
  }
  else {
    list(e4, e3, e1)
  }
}












