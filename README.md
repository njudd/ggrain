# ggrain - Raincloud Plots

`geom_rain()` is a R function for raincloud plots that is highly customizable, connects longitudinal observations, handle Likert data and allows mapping of a covariate.

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
- `rain.side`: Which side to display the rainclouds, 'l' for left and 'r' for right

Specific geom arguments can be passed with a list to any of the 4 geom's with the argument `{point/line/boxplot/violin}.args`. For a list of arguments that can be passed see the help files of the respective geom's (e.g., `?gghalves::geom_half_violin`).

Position related arguments (e.g., jittering, nudging & width) can be passed with `{point/line/boxplot/violin}.args.pos`, see the help file of `?geom_rain` for defaults


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

####Example 1 DEFUNCT
A single raincloud plot

```r
e1 <- ggplot(iris, aes(x = 1, y = Sepal.Length)) 
e1 +
  geom_rain()
```
- **Example 1.1:** Let's flip it & pass a color to the violin fill

```r
e1 +    geom_rain() + 
    coord_flip() #flipping it
```

- **Example 1.2:** Let's pass a fill color to the violin and change its transparancy

```r 
e1 +    geom_rain(violin.args = list(fill = "blue", alpha = .4))
```


- **Example 1.3:** Let's change the smoothing of the violin

```r
e1 +    geom_rain(violin.args = list(fill = "blue", adjust = .2)) + 
    coord_flip()
```
#### Example 2
Let's make rainclouds of the different species overlapped

```r
e2 <- ggplot(iris, aes(x = 1, y = Sepal.Length, fill = Species)) 
e2 +
  geom_rain()
```


- **Example 2.1:** lets change the transparancy

```r
e2 +
  geom_rain(alpha = .7)
```

- **Example 2.2:** let's have the boxplots dodge each other

```r
e2 +
  geom_rain(boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1), 
  width = .07))
```

#### Example 3: let's color the dots by Species

```r
e3 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species, color = Species)) 
e3 +
  geom_rain()
```
- **Example 3.1:** the boxplots are missing the median & IQR now, lets put that back

```r
e3 +
  geom_rain(boxplot.args = list(color = "black", outlier.color = NA))
```

#### Example 4: Let's color the dots by another covariate
```r
e4 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species))
e4 + 
	geom_rain(cov = "Sepal.Width")

```
