#######play space #########


### package building



devtools::document()
devtools::load_all()


# devtools::build_vignettes(ggrain)

devtools::check()

usethis::use_mit_license("Nicholas Judd")






# making likert violin



# needs to work with neg numbers/









##### random code debugging #####


ggplot_build(p2)$data
lemon::geom_pointline()

#### moving the notes at the end of geom_rain() here



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





