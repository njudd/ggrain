<img src="https://github.com/jorvlan/open-visualizations/blob/master/R/package_figures/Rplot03.png" width="200" height="190" align="right"/>

[![R-CMD-check](https://github.com/njudd/ggrain/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njudd/ggrain/actions/workflows/R-CMD-check.yaml)
[![Bugs](https://img.shields.io/github/issues/njudd/ggrain/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/njudd/ggrain/issues?q=is%3Aopen+is%3Aissue)
![CRAN/METACRAN Version](https://img.shields.io/cran/v/ggrain)
<!---[[CRAN_Release_Badge](http://cranlogs.r-pkg.org/badges/version-ago/ggrain)](https://CRAN.R-project.org/package=ggrain)-->
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/ggrain)](https://CRAN.R-project.org/package=ggrain)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/ggrain)](https://cranlogs.r-pkg.org/)
[![](http://cranlogs.r-pkg.org/badges/ggrain)](https://cran.r-project.org/package=ggrain)
[![Vignette](https://img.shields.io/badge/Vignette-ggrain-orange.svg?colorB=E91E63)](https://www.njudd.com/raincloud-ggrain/)
[![](https://img.shields.io/badge/Raincloudplots-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://lcdlab.shinyapps.io/raincloudplots-shiny/)
<!---[![License: ]()](https://github.com/njudd/ggrain/LICENSE)--->

# `ggrain` - [Raincloud Plots](https://wellcomeopenresearch.org/articles/4-63/v2)

`ggrain` is an R-package that allows you to create Raincloud plots - following the 'Grammar of Graphics' (i.e., ggplot2) - that are: 

- Highly customizable
- Connect longitudinal observations
- Handles Likert data
- Allows mapping of a covariate.

### Citation

[`ggrain`](https://www.njudd.com/raincloud-ggrain/) was developed by [`Nicholas Judd`](https://www.njudd.com), [`Jordy van Langen`](https://github.com/jorvlan), Micah Allen, and [`Rogier Kievit`](https://lifespancognitivedynamics.com/). 

<pre>
- Judd, N., van Langen, J., Allen, M., & Kievit, R.A.
    <i>ggrain: A Rainclouds Geom for 'ggplot2'.</i>
    R package version 0.0.4.
    <b>CRAN</b> 2023, https://doi.org/10.32614/CRAN.package.ggrain,
    <a href="https://CRAN.R-project.org/package=ggrain">https://CRAN.R-project.org/package=ggrain</a>
</pre>

	
### Example 

```r
ggplot(iris, aes(x = 1, y = Sepal.Length)) +
  geom_rain()
```

### Installation 

There are two ways to install this package.

1. Download the [CRAN](https://CRAN.R-project.org/package=ggrain) version  
```r
install.packages("ggrain")

library(ggrain)
```

2. Download through [GitHub](https://github.com/njudd/ggrain)
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

Specific geom arguments can be passed with a list to any of the 4 geom's with the argument `{point/line/boxplot/violin}.args`. 
Position-related arguments (e.g., jittering, nudging & width) can be passed with `{point/line/boxplot/violin}.args.pos`, see the help file of `?geom_rain` for defaults

![img](https://raw.githubusercontent.com/njudd/ggrain/main/inst/git_pics/time_group_cov_vin.png)

### Contributions / Issues

We warmly welcome all contributions. 
You can open an issue or make a pull request if you would like to add something new!

### Scientific papers that used & cited 👏 `ggrain`
<pre>
<b>*</b> Yulugkural, Z., Yildiz, M., Topcu, E., Elmaslar Mert, H. T., & Temiz, A. (2026). 
	Epigenetic Modulation of IL‐7 and IL‐10: Toward Personalized Immune Therapies in Viral Epidemics. 
	<b>Journal of Immunology Research, 2026(1), 9467657.</b>
	<a href="https://doi.org/10.1155/jimr/9467657">https://doi.org/10.1155/jimr/9467657</a>
<b>*</b> Birdsey, L. P., Brown, S., Dos’ Santos, T., Evans, D., Runacres, A., Weston, M., & Field, A. (2026).
	National‐Standard Middle‐Distance Runners Maintain 1500 m Time Trial Running Performance on Successive Days. 
	<b>European journal of sport science, 26(3), e70142.</b>
	<a href="https://doi.org/10.1002/ejsc.70142">https://doi.org/10.1002/ejsc.70142</a>
<b>*</b> Yin, C., Kindt, A., Harms, A., Hartman, R., Hankemeier, T., & De Lange, E. (2026). 
	Lipidomic fingerprints reveal sex-, age-, and disease-dependent differences in the TgF344-AD transgenic rats. 
	<b>Metabolomics, 22(1), 9.</b>
	<a href="https://doi.org/10.1007/s11306-025-02350-z">https://doi.org/10.1007/s11306-025-02350-z</a>
<b>*</b> Weston, K. L., Burn, N. L., Goroski, A., Weston, M., Galna, B., Glossop, R., ... & Basterfield, L. (2026). 
	Feasibility of a school-based peer-led high-intensity interval training intervention: the Young Fitness Leaders project. 
	<b>BMC Public Health, 26, 799.</b>
	<a href="10.1186/s12889-026-26543-w">10.1186/s12889-026-26543-w</a>
<b>*</b> Pawel, S., & Held, L. (2026). 
	Bayes Factor Group Sequential Designs. 
	<b>arXiv preprint arXiv:2601.02851.</b>
	<a href="https://doi.org/10.48550/arXiv.2601.02851">https://doi.org/10.48550/arXiv.2601.02851</a>
<b>*</b> Dutschke, R., Thiele, G., Schoemann, M., Surrey, C., & Scherbaum, S. 
	Assessing complex belief structures with the triads task: Reliability and validity.
	<b>OSF</b>
	<a href="https://sciety.org/articles/activity/10.31234/osf.io/ubc85_v1">https://sciety.org/articles/activity/10.31234/osf.io/ubc85_v1</a>
<b>*</b> Urry, H. L., Plonski, P. E., Patel, P., Cathern, M. D., Taylor, H. A., & Brunyé, T. T. (2026). 
	Urgent, hurry up!!! Perceived time pressure affects fine motor performance via subjective distress in US adults. 
	<b>Journal of Experimental Psychology: Human Perception and Performance.</b>
	<a href="https://doi.org/10.1037/xhp0001386">https://doi.org/10.1037/xhp0001386</a>
<b>*</b> Petit, Q., Lecoq, S., Congnard, F., Cronier, N., de Müllenheim, P. Y., Abraham, P., & Noury‐Desvaux, B. (2026). 
	Heart rate increase results in case of positional venous entrapment. 
	<b>Clinical Physiology and Functional Imaging, 46(1), e70041.</b>
	<a href="https://doi.org/10.1111/cpf.70041">https://doi.org/10.1111/cpf.70041</a>
<b>*</b> Cruz, T. D. D., & de Lucena, M. A. (2026). 
	Training and oversight of algorithms in social decision-making: Algorithms with prescribed selfish defaults breed selfish decisions.    	      <b>Computers in Human Behavior, 108924.</b>
	<a href="https://doi.org/10.1016/j.chb.2026.108924">https://doi.org/10.1016/j.chb.2026.108924</a>
<b>*</b> Garofalo, S., Finotti, G., Orsoni, M., Giovagnoli, S., & Benassi, M. (2024). 
	Testing Bayesian Informative Hypotheses in Five Steps With JASP and R.
	<b>Advances in Methods and Practices in Psychological Science, 7(4), 25152459241260259.</b>
	<a href="https://doi.org/10.1177/25152459241260259">https://doi.org/10.1177/25152459241260259</a>
<b>*</b> de Müllenheim, P. Y. (2024). Analyser des données avec R.
	<a href="https://pydemull.github.io/Analyser-des-donnees-avec-R/Analyser-des-donn%C3%A9es-avec-R.pdf">https://pydemull.github.io/Analyser-des-donnees-avec-R/Analyser-des-donn%C3%A9es-avec-R.pdf</a>
<b>*</b> Robison, M. K., Celaya, X., Ball, B. H., & Brewer, G. A. (2024). 
    Task sequencing does not systematically affect the factor structure of cognitive abilities. 
    <b>Psychonomic Bulletin & Review, 31(2), 670-685.</b>
    <a href="https://doi.org/10.3758/s13423-023-02369-0">https://doi.org/10.3758/s13423-023-02369-0</a>
<b>*</b> Han, C., Danzeng, Q., Li, L., Bai, S., & Zheng, C. (2024). 
    Machine learning reveals PANoptosis as a potential reporter and 
    prognostic revealer of tumour microenvironment in lung adenocarcinoma. 
    <b>The Journal of Gene Medicine, 26(1), e3599.</b>
    <a href="https://doi.org/10.1002/jgm.3599">https://doi.org/10.1002/jgm.3599</a>
<b>*</b> Jiang, S., Shang, W. Z., Cui, J. Y., Yan, Y. Y., Yang, T., Hu, Y., ... & Wu, B. (2023). 
    Prevalence and Predictors of Hemorrhagic Foci on Long-term 
    Follow-up MRI of Recent Single Subcortical Infarcts. 
    <b>Translational Stroke Research, 1-11.</b>
    <a href="https://doi.org/10.1007/s12975-023-01224-7">https://doi.org/10.1007/s12975-023-01224-7</a>
<b>*</b> Senftleben, U., Schoemann, M., & Scherbaum, S. (2024). 
    Choice repetition bias in intertemporal choice: An eye-tracking study.
    <b>OSF (Open Science Framework) / PsyArXiv.</b>
    <a href="https://doi.org/10.31234/osf.io/g3v9m">https://doi.org/10.31234/osf.io/g3v9m</a>
<b>*</b> Bognar, M., Gyurkovics, M., Aczel, B., & van Steenbergen, H. (2023).
    The curve of control: Non-monotonic effects of task difficulty on cognitive control.
    <b>PsyArXiv</b>
    <a href="https://doi.org/10.31234/osf.io/ywup9">https://doi.org/10.31234/osf.io/ywup9</a>
</pre>	

### Funding
<img src="https://github.com/njudd/ggrain/blob/main/inst/git_pics/nwo_openscience.jpg" width="150" height="160" align="right"/>

In 2021, NWO (Dutch research council) announced their inaugural [NWO Open Science Fund](https://www.nwo.nl/en/researchprogrammes/open-science/open-science-fund). The Open Science Fund aims to support researchers to develop, test and implement innovative ways of making research open, accessible, transparent and reusable, covering the whole range of Open Science. The Raincloud plots team was awarded this fantastic initiative and is specifically working on:

- Creating the [`ggrain`](https://github.com/njudd/ggrain) R-package
- Creating an interactive R Shiny application [`raincloudplots`](https://lcdlab.shinyapps.io/raincloudplots-shiny/)
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

There are now ***4*** ways in which you can use our Raincloud Plots tools: 
- through a series of specific easy to modify scripts [https://github.com/RainCloudPlots/RainCloudPlots](https://github.com/RainCloudPlots/RainCloudPlots)
- through our initial [`raincloudplots`](https://github.com/jorvlan/raincloudplots) package
- through the newest R-package [`ggrain`](https://github.com/njudd/ggrain)
- through our R Shiny application: [`raincloudplots`](https://lcdlab.shinyapps.io/raincloudplots-shiny/)
