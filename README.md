# ggrain - Raincloud Plots

The goal is to have a geom_rain() so the following code runs:

```
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_rain()
```

This should work for one timepoint and many.

List of priorities:

- flips based on x & y arg
- orientation argument (to center dots, box or vio)
- has a longitudinal option
- allows customization
- has an automatic scalling function?


Eventually it should have a pre/post option with flanking/grouped rainclouds.


### Installation

```r
if (!require(remotes)) {
    install.packages("remotes")
}
remotes::install_github('njudd/ggrain')

library(ggrain)
```

### Key arguments for geom_rain

The goal of this function is to act in a similar fashion to other geom's in ggplot2. Ultimately, it is a combination of 4 different geom's (i.e., point, line, boxplot & violin). 

Geom specific arguments can be passed with a list of arguments to `{point/line/boxplot/violin}.args` while positional arguments can be modified with `{point/line/boxplot/violin}.args.pos`. The function has three extensions passed by the following arguements:

1. `cov`: a covariate to remap the color of the points (see Example x**)
2. `id.long.var`: a grouping variable to connect the lines by (see Example X**)
3. `Likert`: `True` or `False` response which adds some y-jittering.



### Examples

####Example 1
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
