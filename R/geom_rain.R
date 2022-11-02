#' Raincloud Plot
#' Nicholas Judd (njudd), Jordy van Langen (jorvlan) & Rogier Kievit
#' 21/10/2022
#'
#' @name geom_rain
#' @inheritParams ggplot2::geom_boxplot
#' @param id.long.var A group to connect the lines by (e.g., "id).
#' @param cov A covariate to color the dots by.
#' @param rain.side How you want the rainclouds displayed, right (r), left (l) or flanking (f).
#' @param point.args A list of args for the dots
#' @param point.args.pos A list of positional args for the points
#' @param line.args A list of args for the lines, you need to specify a group to connect them with id.long.var
#' @param line.args.pos A list of positional args for the lines
#' @param boxplot.args A list of args for the boxplot
#' @param boxplot.args.pos A list of positional args for the boxplot
#' @param violin.args A list of args for the violin
#' @param violin.args.pos A list of positional args for the violin
#'
#' need library(rlang)
#' need library(ggplot2)
#' depends = ggplot2
#' @importFrom ggplot2 aes
#' @importFrom gghalves geom_half_violin
#' @importFrom rlang list2 sym !! !!! exec
#' @importFrom ggpp position_dodgenudge
#' @export

geom_rain <- function(mapping = NULL,
                      data = NULL,
                      trim = TRUE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      id.long.var = NULL, # should lines be drawn & what should connect them?
                      cov = NULL, # should dots be colored due to a covariate?
                      rain.side = NULL, # do you want the rainclouds (l)eft, (r)ight or (f)lanking
                      # rain.center = NULL, currently not implimented
                      ...,
                      point.args = rlang::list2(
                        ...
                      ),
                      point.args.pos = rlang::list2(
                        position = position_jitter(
                          width = .04,
                          height = NULL,
                          seed = 42)
                      ),
                      line.args = rlang::list2(
                        alpha = .2,
                        ...
                      ),
                      line.args.pos = rlang::list2(
                        position = position_jitter(
                          width = .04,
                          height = NULL,
                          seed = 42),
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

  # the width arguement might not make sense, since its always to the scale of the plot


  # doing positional changes based off user input
  if (!is.null(rain.side) && rain.side %in% c("r", "l")) {
    violin.args.pos$side <- rain.side

    if(rain.side == "l"){
      violin.args.pos$position$x <- -violin.args.pos$position$x
      boxplot.args.pos$position$x <- -boxplot.args.pos$position$x
    }

  } else if (!is.null(rain.side) && rain.side %in% c("f", "flanking")){

    if ("side" %in% names(violin.args.pos)){

      # this shouldn't be by default yet only if side is there & position.nudge is somethign different!!!

      # warning("Option 'flanking' is used with defaults violin position arguments (i.e., violin.args.pos)
            # therefore new flanking position defaults are being used. If you wish to set your own defaults make sure you do
            # not specify a side argument for violin.args.pos otherwise they will be overridden", call. = FALSE)

      boxplot.args.pos <- rlang::list2(
        width = .08,
        position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1)))
      violin.args.pos <- rlang::list2(
        width = .7,
        position = position_nudge(x = c(rep(-.15, 256*2), rep(-.15, 256*2),
                                        rep(.15, 256*2), rep(.15, 256*2))))
    }
  } else if (!is.null(rain.side)) {
    stop("ERROR: the rain.side arguement only accepts 'l' for left and 'r' for right \n STOPPING", call. = FALSE)
  }
  # combining args with position args
  point.args <- c(point.args, point.args.pos)
  line.args <- c(line.args, line.args.pos)
  boxplot.args <- c(boxplot.args, boxplot.args.pos)
  violin.args <- c(violin.args, violin.args.pos)

  if(!is.null(cov)){ # remap the color in e1; if a covariate is specified
    e1 <- rlang::exec(geom_point_sorted, aes(color = !!rlang::sym(cov)), inherit.aes = TRUE, !!!point.args) # bang, bang, bang
  }else{
    e1 <- rlang::exec(geom_point_sorted, inherit.aes = TRUE, !!!point.args) # bang, bang, bang
  }

  e3 <- rlang::exec(geom_boxplot, inherit.aes = TRUE, !!!boxplot.args)

  if(!is.null(rain.side) && rain.side %in% c("f", "flanking")){

    e4 <- rlang::exec(geom_paired_raincloud,inherit.aes = TRUE, !!!violin.args)
  }else{
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
    }else{
      e1 <- rlang::exec(geom_point_sorted, aes(group = !!rlang::sym(id.long.var)), inherit.aes = TRUE, !!!point.args)
    }

    list(e2, e4, e3, e1)

  }else{
    list(e4, e3, e1)
  }
}












