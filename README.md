[![R-CMD-check](https://github.com/njudd/ggrain/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njudd/ggrain/actions/workflows/R-CMD-check.yaml)
[![Bugs](https://img.shields.io/github/issues/njudd/ggrain/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/njudd/ggrain/issues?q=is%3Aopen+is%3Aissue)
[![CRAN_Release_Badge](http://www.r-pkg.org/badges/version-ago/ggrain)](https://CRAN.R-project.org/package=ggrain)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/ggrain)](https://CRAN.R-project.org/package=ggrain)
[![](http://cranlogs.r-pkg.org/badges/ggrain)](https://cran.r-project.org/package=ggrain)
[![Vignette](https://img.shields.io/badge/Vignette-ggrain-orange.svg?colorB=E91E63)](https://www.njudd.com/raincloud-ggrain/)
<!---[![License: ]()](https://github.com/njudd/ggrain/LICENSE)--->

# `ggrain` - [Raincloud Plots](https://wellcomeopenresearch.org/articles/4-63/v2)

`ggrain` is an R-package that allows you to create Raincloud plots - following the 'Grammar of Graphics' (i.e., ggplot2) - that are: 

- Highly customizable
- Connect longitudinal observations
- Handles Likert data
- Allows mapping of a covariate.
	
### Example 

```r
ggplot(iris, aes(x = 1, y = Sepal.Length)) +
  geom_rain()
```

### Installation 

1. Download through CRAN
```r
install.packages("ggrain")

library(ggrain)
```

2. Download through GitHub
```r
if (!require(remotes)) {
    install.packages("remotes")
}
remotes::install_github('njudd/ggrain')

library(ggrain)
```

###  Simple examples

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


![img](https://raw.githubusercontent.com/njudd/ggrain/main/inst/git_pics/basic_rain.png)

### Vignette
For a complete overview of `ggrain` such as a 2-by-2 raincloud plot or multiple repeated measures, please see our [Vignette](https://www.njudd.com/raincloud-ggrain/).

### `ggrain` specific features

`geom_rain` is a combination of 4 different ggplot2 geom's (i.e., point, line, boxplot & violin).

- `id.long.var`: a grouping variable to connect the lines by
- `cov`: a covariate to remap the color of the points
- `Likert`: `True` or `False` response which adds y jittering
- `rain.side`: Which side to display the rainclouds: 'l' for left, 'r' for right and 'f' for flanking

Specific geom arguments can be passed with a list to any of the 4 geom's with the argument `{point/line/boxplot/violin}.args`. For a list of arguments that can be passed see the help files of the respective geom's (e.g., `?gghalves::geom_half_violin`).

Position-related arguments (e.g., jittering, nudging & width) can be passed with `{point/line/boxplot/violin}.args.pos`, see the help file of `?geom_rain` for defaults

![img](https://raw.githubusercontent.com/njudd/ggrain/main/inst/git_pics/time_group_cov_vin.png)

### Contributions / Issues

We warmly welcome all contributions. 
You can open an issue or make a pull request if you would like to add something new!

### Citation

[`ggrain`](https://github.com/njudd/ggrain) was developed by Nicholas Judd, Jordy van Langen and Rogier Kievit. 

<pre>
- Judd, Nicholas, van Langen, Jordy, & Kievit, Rogier.
    <i>ggrain: A Rainclouds Geom for 'ggplot2'.</i>
    R package version 0.0.1.
    <b>CRAN</b> 2023,
    <a href="https://CRAN.R-project.org/package=ggrain">https://CRAN.R-project.org/package=ggrain</a>
</pre>

### Funding
<img src="https://github.com/njudd/ggrain/blob/main/inst/git_pics/nwo_openscience.jpg" width="150" height="160" align="right"/>

In 2021, NWO (Dutch research council) announced their inaugural [NWO Open Science Fund](https://www.nwo.nl/en/researchprogrammes/open-science/open-science-fund). The Open Science Fund aims to support researchers to develop, test and implement innovative ways of making research open, accessible, transparent and reusable, covering the whole range of Open Science. The Raincloud plots team was awarded this fantastic initiative and is specifically working on:

- Creating the [`ggrain`](https://github.com/njudd/ggrain) R-package
- Creating a ShinyApp Raincloudplots
- Integrating Raincloudplots in [JASP Statistics](https://jasp-stats.org)
- Organzing [globally accessible, online workshops](https://github.com/jorvlan/raincloudplots-workshops) to help people create raincloudplots and improve their data visualizations in general.

You can read more about our awarded project here: https://www.nwo.nl/en/projects/203001011 or you can watch the online webinar hosted by NWO about our project: [![Webinar Open Science series S1E2: Open tools for data enrichment and visualization](https://github.com/njudd/ggrain/blob/main/inst/git_pics/raincloudplots_NWO_webinar.png)](https://youtu.be/Kvcyh_9KSbw?t=1910 "Webinar Open Science series S1E2: Open tools for data enrichment and visualization")


### Raincloud Plots 

**Paper**
<br>
<pre>
- Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., van Langen, J., & Kievit, R. A.
    Raincloud plots: a multi-platform tool for robust data visualization [version 2; peer review: 2 approved] 
    <b>Wellcome Open Research</b> 2021, 4:63. <a href="https://doi.org/10.12688/wellcomeopenres.15191.2">https://doi.org/10.12688/wellcomeopenres.15191.2</a>
</pre>

There are now ***3*** ways in which you can use our Raincloud Plots tools: 
- through a series of specific easy to modify scripts [https://github.com/RainCloudPlots/RainCloudPlots](https://github.com/RainCloudPlots/RainCloudPlots)
- through our initial [`raincloudplots`](https://github.com/jorvlan/raincloudplots) package
- through the newest R-package [`ggrain`](https://github.com/njudd/ggrain)
