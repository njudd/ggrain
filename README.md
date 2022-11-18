# ggrain - Raincloud Plots

`geom_rain()` is an R function for raincloud plots that is highly customizable, connects longitudinal observations, handles Likert data, and allows mapping of a covariate.

```
ggplot(iris, aes(x = 1, y = Sepal.Length)) +
  geom_rain()
```

Current dev goals:

- orientation error when the user tries to plot discrete data in the y-axis
- fix flanking option


### Installation

```r
if (!require(remotes)) {
    install.packages("remotes")
}
remotes::install_github('njudd/ggrain')

library(ggrain)
```

### Features


`geom_rain` is a combination of 4 different ggplot2 geom's (i.e., point, line, boxplot & violin).

- `id.long.var`: a grouping variable to connect the lines by
- `cov`: a covariate to remap the color of the points
- `Likert`: `True` or `False` response which adds y jittering
- `rain.side`: Which side to display the rainclouds: 'l' for left, 'r' for right and 'f' for flanking

Specific geom arguments can be passed with a list to any of the 4 geom's with the argument `{point/line/boxplot/violin}.args`. For a list of arguments that can be passed see the help files of the respective geom's (e.g., `?gghalves::geom_half_violin`).

Position-related arguments (e.g., jittering, nudging & width) can be passed with `{point/line/boxplot/violin}.args.pos`, see the help file of `?geom_rain` for defaults

![img](https://raw.githubusercontent.com/njudd/ggrain/main/time_group_cov.png)

### Basic examples

Different groups overlapped

```
ggplot(iris, aes(x = 1, y = Sepal.Length, fill = Species)) +
	geom_rain(alpha = .5)
```
A raincloud per group

```
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
	geom_rain(rain.side = 'l')
```

**for more examples see the vignette** 
