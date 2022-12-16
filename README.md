# ggrain - Raincloud Plots

`r
geom_rain()
` is an R function for raincloud plots that is highly customizable, connects longitudinal observations, handles Likert data, and allows mapping of a covariate.

```r
ggplot(iris, aes(x = 1, y = Sepal.Length)) +
  geom_rain()
```

We welcome all contributions. Please make a pull request if you would like to add something new!

Current dev goals:

- Get on CRAN (because of this the vignette must only be in .Rmd; download the package to see the vignette)
- Develop the likert option


### Installation

```r
if (!require(remotes)) {
    install.packages("remotes")
}
remotes::install_github('njudd/ggrain')

library(ggrain)
```

### Basic examples

1.  Raincloud per group

	```r
	ggplot(iris, aes(x = Species, y = Sepal.Length, fill = 	Species)) +
		geom_rain(rain.side = 'l')
	```

2.  Different groups overlapped

	```r
	ggplot(iris, aes(x = 1, y = Sepal.Length, fill = Species)) +
		geom_rain(alpha = .5)
	```

For more examples such as a 2-by-2 plot and multiple repeated measures, please see our [Vignette](https://www.njudd.com/raincloud-ggrain/).

![img](https://raw.githubusercontent.com/njudd/ggrain/main/inst/git_pics/basic_rain.png)



### Features


`geom_rain` is a combination of 4 different ggplot2 geom's (i.e., point, line, boxplot & violin).

- `id.long.var`: a grouping variable to connect the lines by
- `cov`: a covariate to remap the color of the points
- `Likert`: `True` or `False` response which adds y jittering
- `rain.side`: Which side to display the rainclouds: 'l' for left, 'r' for right and 'f' for flanking

Specific geom arguments can be passed with a list to any of the 4 geom's with the argument `{point/line/boxplot/violin}.args`. For a list of arguments that can be passed see the help files of the respective geom's (e.g., `?gghalves::geom_half_violin`).

Position-related arguments (e.g., jittering, nudging & width) can be passed with `{point/line/boxplot/violin}.args.pos`, see the help file of `?geom_rain` for defaults

![img](https://raw.githubusercontent.com/njudd/ggrain/main/inst/git_pics/time_group_cov.png)


