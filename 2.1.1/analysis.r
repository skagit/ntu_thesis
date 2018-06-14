library(plyr) #should migrate to newer version- dplyr
library(ggplot2)
library(igraph)
library(data.table) #fastest option for reading, writing, and manipulating large data sets - often hundreds to thousands of times faster than plyr and dplyr
library(lme4)
library(gridExtra)
library(grid)

# Read Data

df <- fread("csv/diffuse.csv")

# Analyze

avg <- df[, list(prop=sum(innovated)/length(innovated)), by=c("topology","size","round")]

# Quick Visualization

qplot(round, prop, data=avg[size=="125",], color=as.factor(topology), geom="line")
qplot(round, prop, data=avg[size=="25",], color=as.factor(topology), geom="line")

## Plots

diff25 <- ggplot(data = avg[size==25 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), ] , aes(x=round, y=prop, color=topology)) + geom_point() + geom_line() + 
  ylab("Proportion of Innovators") + xlab("Round") +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1") ) +  ylim(0,1) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))

diff125 <- ggplot(data = avg[size==125 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), ] , aes(x=round, y=prop, color=topology)) + geom_point() + geom_line() + 
  ylab("Proportion of Innovators") + xlab("Round") +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1") ) +  ylim(0,1) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))
