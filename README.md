# ggrain

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


#############################################################
###### PRIORITY###### PRIORITY###### PRIORITY###### PRIORITY
#############################################################
# TO DO Major things denoted with *

# make a covariate plot
# remap color arg to the covariate, see if it works with categorical vars

# https://github.com/tidyverse/ggplot2/blob/6434c1e1181ab8feff40e86d7cbdcd0b11cf3758/R/layer.r


# what do they mean by works with LIKERT


##### change geom_half_boxplot to geom_boxplot


# need a flanking geom_rainpaired() fucntion

# needs to do left right automatically


### you should make this use geom_boxplot...?


############# longitudinal conencted jittered lines (DONE)
# uses geom-point-sorted()
# works with an arg for id.long.var

############## jittering (partially finished)
# Issues of orientation is how to jitter & nudge?
# you could have a stat that jitters (already built)
# you can do the jittering as a stat for pointline and than do position_nudge()
# what about postion dodge, maybe that would automatically adjust to wider jitters

# this is current solved by ordering the data

############## orientation (incomplete)
# currently hardcoded yet if you use a function you will have the default args issues again

# idea do this with "width" have a default percentage 20/15/65 (dots ditter, box, vio)
# tricky as jittering is now tied to it
# than min & max widths; when max is hit break & tell about ggpointdensity


############## overlapping (not started)
# your function doesn't support overlapping violins like in rainclouds_2x2_repmes
# neither does geom_violin atm
# temp_g <- rbind(temp, temp)
# temp_g$g <- c(rep("a", dim(temp_g)[1]/2), rep("b", dim(temp_g)[1]/2))
# temp_g[1:(dim(temp_g)[1]/2),]$value <- temp_g[1:(dim(temp_g)[1]/2),]$value + 1
# 
# ggplot(temp_g, aes(time, value, fill = g)) +
#   geom_violin()

############## make a paired option where you have them flanking



############## supplementary features
# 1. use https://github.com/LKremer/ggpointdensity
# 2. make an easy arg for left sided

#############################################################
#############################################################


# so dots in center, with space for a left right arguement for the box & violin

# the jitter arguement will scale with width!
# or it is just a width argu for dots

# See if you can nudge differently depending on the itteration

# color & colour args
# https://stackoverflow.com/questions/60348226/is-there-an-r-function-to-connect-grouped-data-points-created-by-a-geom-objec


# uncomment to make the function work
# source("~/projects/rain/ggrain/geom-point-sorted.r")
# source("~/projects/rain/ggrain/utilities-grid.r")
# 
# library(rlang); library(grid)
# library(ggpp) # for position_jitternudge
