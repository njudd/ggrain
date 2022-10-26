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


## MUST SUPPLY A DATA ARG & a condition arg

source("~/projects/rain/ggrain/geom-point-sorted.r")
source("~/projects/rain/ggrain/utilities-grid.r")

# you should copy this and cite him
devtools::source_url("https://raw.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R")



library(rlang); library(grid)


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
                        position = position_jitter(
                          width = .02, 
                          height = NULL,
                          seed = 42),
                        ...
                      ),
                      line.args = rlang::list2(
                        alpha = .2,
                        position = position_jitter(
                          width = .02, 
                          height = NULL,
                          seed = 42),
                        ...
                      ),
                      boxplot.args =  rlang::list2(
                        width = .1, color = "black", outlier.color = NA,
                        position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1)),
                        ...
                      ),
                      violin.args = rlang::list2(
                        alpha = .3,
                        position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),
                                                        rep(.2, 256*2), rep(.2, 256*2))),
                        ...
                      )
)
{
  
  
  
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


ggplot(temp2, aes(time, value, fill = group, color = group)) +
  geom_prain() +
  theme_minimal()


# this example is perrrrfect!!! 
ggplot(temp2, aes(time, value, fill = group)) +
  geom_point_sorted(aes(group = id), alpha = .2, # you need group = id for the points & lines to match
                    position = position_jitter(
                      width = .02, 
                      height = NULL,
                      seed = 42)) +
  geom_line(aes(group = id), alpha = .2, # you need group = id for the points & lines to match
    position = position_jitter(
      width = .02, 
      height = NULL,
      seed = 42)) +
  geom_boxplot(width = .1, color = "black", outlier.color = NA,
               position = ggpp::position_dodgenudge(x = c(-.1, -.1, .1, .1))) +
  geom_paired_raincloud(alpha = .3,
                        position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),
                                                        rep(.2, 256*2), rep(.2, 256*2)))) +
  theme_minimal()



# you need to comment the fuck out of this... 


# I think violin fails because position_nudge is applied to the whole ggplot dataframe
# you should do c(rep(-.2, 255*2), rep(.2, 255*2))
  
# one type of behaviour makes very little sense

ggplot(temp2, aes(time, value, fill = group)) +
  gghalves::geom_half_violin()

# they are seperated; "identity" or one nudge brings them back in place

ggplot(temp2, aes(time, value, fill = group)) +
  gghalves::geom_half_violin(position = position_nudge(x = -.2))


ggplot(temp2, aes(time, value, fill = group)) +
  gghalves::geom_half_violin(position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),
                                                             rep(.2, 256*2), rep(.2, 256*2))))




ggplot(temp2, aes(time, value, fill = group)) +
  geom_violin(position = position_nudge(x = -.2))


ggplot(temp2, aes(time, value, fill = group)) +
  gghalves::geom_half_violin(position = position_nudge(x = -.2))






ggplot(temp2, aes(time, value, fill = group)) +
  geom_paired_raincloud(alpha = .3,
                        position = position_nudge(x = -.3)) +
  gghalves::geom_half_boxplot(center = T, width = .3,
                              position = ggpp::position_dodgenudge(x = -.1))








ggplot(temp2, aes(time, value, fill = group)) +
  gghalves::geom_half_violin(alpha = .3,
                             position = position_nudge(x = -.3)) +
  gghalves::geom_half_boxplot(center = T, width = .3,
                              position = ggpp::position_dodgenudge(x = -.1))

devtools::source_url("https://raw.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R")













ggplot(temp2, aes(time, value, fill = group)) +
  gghalves::geom_half_violin(position = "identity") +
  # geom_boxplot(postition = "identity")
  gghalves::geom_half_boxplot(postition = "identity", center = T, width = .3)


ggplot(temp2, aes(time, value, fill = time, color = group)) +
  geom_point(alpha = .3, position = position_jitter(
    width = .02, 
    height = NULL,
    seed = 42)) +
  geom_boxplot(aes(time, value, fill = group, color =  group))


ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()

ggplot(temp2, aes(time, value, fill = time)) + 
  geom_pairedrain(data = temp2, 
                  condition_by = "time")


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
temp2 <- temp |> 
  dplyr::filter(time %in% c("t1", "t2"))

temp2$group <- c(rep("old", 200), rep("young", 200), rep("old", 200), rep("young", 200))
temp2$value[temp2$group == "young"] <- temp2$value[temp2$group == "young"] + 1
temp2$value[temp2$group == "old" & temp2$time == "t2"] <- temp2$value[temp2$group == "old" & temp2$time == "t2"] -2


#### workspace
# time is hardcoded; also not working atm


ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_pairedrain(data = temp_subset10, condition_by = "hmm") #temp_subset10


ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_point_sorted(color = "blue", alpha = .5) +
  geom_point(color = "red", alpha = .5)






ggplot(temp, aes(time, value, fill = time)) + 
  geom_rain(data = temp,
            alpha = .3, id.long.var = 'id', line.args = list(alpha = .05)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") #+
  #coord_flip()

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

ggplot(temp_subset10, aes(time, value, fill = time)) + 
  geom_line(aes(time, value, group = id))




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