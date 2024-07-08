---
title: 'ggrain - a ggplot2 extension for raincloud plots'
tags:
  - R
  - plotting
  - ggplot2
  - rainclouds
authors:
  - name: Nicholas Judd
    orcid: 0000-0002-0196-9871
    equal-contrib: true
    corresponding: true
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Jordy van Langen
    orcid: 0000-0003-2504-2381
    equal-contrib: true
    affiliation: 1
  - name: Davide Poggiali
    orcid: 0000-0002-2894-0825
    affiliation: 2
  - given-names: Kirstie Whitaker
    orcid: 0000-0001-8498-4059
    affiliation: 3
  - given-names: Tom Rhys Marshall
    affiliation: 4
  - given-names: Micah Allen^[shared senior authorship]
    orcid: 0000-0001-9399-4179
    affiliation: "5, a"
  - given-names: Rogier Kievit^[shared senior authorship]
    orcid: 0000-0003-0700-4568
    affiliation: "1, a"
    
affiliations:
 - name: Department of Cognitive Neuroscience, Donders Institute for Brain, Cognition and Behavior, Radboud University Medical Center, Nijmegen, The Netherlands
   index: 1
 - name: Università di Padova, Padova, Italy
   index: 2
 - name: The Alan Turing Institute, London, UK
   index: 3
 - name: Centre for Human Brain Health, School of Psychology, University of Birmingham, Birmingham, United Kingdom
   index: 4
 - name: Center of Functionally Integrative Neuroscience, Department of Clinical Medicine, Aarhus University, Aarhus, Denmark
   index: 5
 - name: Shared Senior Authorship
   index: a 
date: 8 July 2025
bibliography: inst/references.bib
---

# Summary

Clear data visualization is essential to effectively communicate empirical findings across various research fields. Raincloud plots fill this need by offering a transparent and statistically robust approach to data visualization [@allen2021raincloud]. This is achieved by combining three plots in an aesthetically pleasing fashion. First, a dot plot displays raw data with minimal distortion, allowing a quick glance at the sample size and outlier identification. Next, a box plot displays key distributional summary statistics such as the median and interquartile range. Lastly, a violin plot transparently displays the underlying distribution of the data. Despite the widespread use of raincloud plots, an R package in alignment with the ‘grammar of graphics’ was lacking [@wilkinson2012grammar; @ggplot2]. `ggrain` fills this need by offering one easy-to-use function (`geom_rain`) allowing the quick and seamless plotting of rainclouds in the R ecosystem. Further, it enables more complex plotting features such as factorial grouping, mapping with a secondary (continuous) covariate, and connecting observations longitudinally across multiple waves.

# Statement of need

Done well, data visualization is one of the single most powerful ways in which scientists can communicate messages. However, through force of habit or conventions, researchers commonly use visualisation methods like the barplot, which removes the distributional properties of underlying data. Bar plots are sensitive to distortion, unable to accurately represent the raw data, and do not display potential differences in distributions. For these reasons, they can lead to misinterpretation about the magnitudes of statistical differences between samples [@weissgerber2015beyond] and are commonly criticized for being a non-transparent means to visualize data.

To overcome these challenges, we developed ‘raincloud plots’ [@allen2021raincloud] that aims to address these problems in an intuitive, modular, and statistically robust format (Figure 1). In essence, raincloud plots combine a ‘split-half violin’ (an un-mirrored Probability Density Function plotted against the redundant data axis), raw jittered data points, and a boxplot for standard visualization of the median and interquartile range. The combination of these plots allows maximal statistical information at a glance. Raincloud plots were very well received by the academic community, being in the top 5% of all research outputs scored by Altmetric, having more than 1000 GitHub stars, along with integration in popular statistical open-source software (Cf., [https://jasp-stats.org/2021/10/05/raincloud-plots-innovative-data-visualizations-in-jasp/](!https://jasp-stats.org/2021/10/05/raincloud-plots-innovative-data-visualizations-in-jasp/)) [@love2019jasp].

However, a robust and validated R-package was lacking.Therefore, we developed 'ggrain' allowing researchers in the R programming language [@rCore] to create raincloud plots in an easy and logical fashion. In `ggrain`, every plotting element:

- Is highly customizable 
- Connects pre-post differences or longitudinal observations across time (e.g., ecological momentary assessment data)
- Can handle Likert scale data
- Allows mapping of a covariate

As with the wider raincloud plots framework [@allen2021raincloud], our goal with `ggrain` is not to propose an entirely novel discovery, but rather to make a powerful, seamless-to-use R-package that is aligned with the Grammar of Graphics. Since its publication in March 2023 until the time of writing (June 2024), `ggrain` has been downloaded over 15.000 times, indicating the appreciation of our package and demand for its use.

# Usage

**ggrain** is available on CRAN [https://cran.r-project.org/web/packages/ggrain/index.html](!https://cran.r-project.org/web/packages/ggrain/index.html).  The `geom_rain` function is a combination of 4 different geom functions (i.e., `geom_point`, `geom_line`, `geom_boxplot` & `geom_half_violin`). The following four higher-level convenience arguments allow major changes to the raincloud:

- `id.long.var`: a grouping variable to connect the lines
- `cov`: a covariate to remap the color of the points
- `likert`: True or False response which adds y jittering
- `rain.side`: Which side to display the rainclouds: 'l' for left, 'r' for right and 'f' for flanking


Geom-specific arguments can be passed with a list to any of the 4 geom’s using the argument {point/line/boxplot/violin}.args. For a list of arguments that can be passed see the help files of the respective geom's (e.g., ?gghalves::geom_half_violin).

Position-related arguments (e.g., jittering, nudging & width) can be passed with {point/line/boxplot/violin}.args.pos, see the help file of ?geom_rain for defaults.

For detailed usage examples see the vignette [https://www.njudd.com/raincloud-ggrain/](!https://www.njudd.com/raincloud-ggrain/).

**ggrain** requires the **ggplot2** package [@ggplot2] to build `geom_rain` which follows the grammar of the graphics framework. The `gghalves` package is extensively used for their half `geom_half_violin` function [@gghalves]. `ggpp` is used for their combination of jitter and nudging for points [@ggpp]. Other dependencies include `grid`, `rlang`, `vctrs` & `cli` [@rCore; @rlang; @vctrs; @cli].

![Two example figures from **ggrain**: *a)* a grouped raincloud and *b)* a grouped repeated measures raincloud. Source code how to create these figures is available at: [https://github.com/njudd/ggrain/blob/main/inst/JOSS_figures.R](!https://github.com/njudd/ggrain/blob/main/inst/JOSS_figures.R).
](inst/git_pics/Pub_Fig.png)

# Acknowledgements

This project was supported by the Open Science Fund from the Dutch research council (Nederlandse Organisatie voor Wetenschappelijk Onderzoek, NWO, file number: 203.001.011). First we would like to acknowledge the coining of the name ‘raincloud plots’ by Jon Roiser on March 15, 2018. We also would like to thank all participants in our in-person and online raincloud plots workshops, as their feedback has considerably improved our package. We also thank Luisa Fassi & Jessica Schaaf for reviewing this manuscript. Finally, we would like to thank everyone who has ever used raincloud plots in their work, shared or advocated our work on social media, submitted bug reports or issues, and provided kind suggestions on how to improve our software. The ‘ggrain’ R-package is more robust, user-friendly, and intuitive because of them.

# References








