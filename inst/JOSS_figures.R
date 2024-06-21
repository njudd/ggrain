## Jordy van Langen & Nicholas Judd

## figure creation for 'ggrain' manuscript to Journal of Open Source Software

## Use and pre-load to setwd() all documents in : "Workshop8 - Brain development day" 

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(patchwork, tidyverse, ggrain) 

#################################################
# first we will start by simulating some data! :)
#################################################
set.seed(42)
groupb <- rnorm(250, 50, 25)

groupa <- groupb[groupb>15 & groupb <60] + rnorm(length(groupb[groupb>15 & groupb <60]))
half_extra <- runif((length(groupb) - length(groupa))/2,30, 45)
half_exp <- runif((length(groupb) - length(groupa))/2,45, 160)

groupa <- c(groupa, half_extra, half_exp)

df <- data.frame(score = c(groupa, groupb), group = c(rep("A", length(groupa)), rep("B", length(groupb))))


## simple figure

p_simple <- ggplot(simdat, aes(x=group,y=score, fill = group, colour = group)) +
  geom_rain(rain.side = "r",
            boxplot.args = list(color = "black", outlier.color = NA)) + 
  scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 26) 

# making long data
g_A1 <- rnorm(15, 6.5, 2)
set.seed(22)
g_A2 <- rnorm(15, 6.5, 2) + rnorm(15) + 1

g_B1 <- c(rnorm(14, 2, 1), 8.1)
set.seed(24)
g_B2 <- c(rnorm(14, 2, 1), 8.9) + rnorm(15) + 1

df2 <- data.frame(score = c(g_A1, g_A2, g_B1, g_B2), 
                  id = as.character(c(rep(1:15), rep(1:15), rep(16:30), rep(16:30))),
                  time = c(rep("1", 15), rep("2", 15), rep("1", 15), rep("2", 15)),
                  group = c(rep("A", 30), rep("B", 30)))

# making a more complex long data fig

p_complex <- ggplot(df2, aes(x = time, y = score, fill = group, color = group)) +
  geom_rain(id.long.var = "id", rain.side = "f2x2",
            boxplot.args = list(color = "black", outlier.color = NA),
            violin.args = list(adjust = 1.5, trim = FALSE)) +
  scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 26) 

p_all <- p_simple / p_complex + plot_annotation(tag_levels = "a")

ggsave("~/projects/rain/ggrain/inst/git_pics/Pub_Fig.png", bg = "white",
       width = 12, height = 10)
