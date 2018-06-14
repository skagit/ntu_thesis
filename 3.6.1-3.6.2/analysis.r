library(plyr) #should migrate to newer version- dplyr
library(ggplot2)
library(igraph)
library(data.table) #fastest option for reading, writing, and manipulating large data sets - often hundreds to thousands of times faster than plyr and dplyr
library(lme4)
library(gridExtra)
library(grid)
library(effects)
library(lmerTest)

#load data

dfs <- fread("csv/sim6.csv")
dfl <- fread("csv/sim7.csv")

df <- rbind(dfs, dfl)

avg <- df[, list(synth=mean(synthesis)), by=c("topology", "generation","size")]

df1 <- df[topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),]
df1$topology <- as.numeric(df1$topology)
mod_sim1_25 <- lmer(synthesis ~ topology + (1 | replication), data = df1[size==25 & generation == 100, ])
mod_sim1_125 <- lmer(synthesis ~ topology + (1 | replication), data = df1[size==125 & generation == 100,])

summary(mod_sim1_25)
summary(mod_sim1_125)

df2 <- df[topology %in% c("1.0", "hier", "0.4"), ]
df2$topology <- as.factor(df2$topology)
df2$size <- as.factor(df2$size)
df2$topology <- factor(df2$topology, levels=c("hier", "1.0", "0.4"))

mod_sim2_25  <- lmer(synthesis ~ topology + (1 | replication) , data = df2[df2$size==25 & df2$generation>100,])
mod_sim2_125 <- lmer(synthesis ~ topology + (1 | replication) , data = df2[df2$size==125 & df2$generation>100,])

summary(mod_sim2_25)
mod_sim2_25
summary(mod_sim2_125)
mod_sim2_125

df3 <- df[topology %in% c("1.0", "hier"), ]
df3$topology <- as.factor(df3$topology)
df3$size <- as.factor(df3$size)
df3$topology <- factor(df3$topology, levels=c("hier", "1.0"))

mod_sim2_x <- lmer(synthesis ~ topology * size + (1 | replication) , data = df3[generation>100,])

summary(mod_sim2_x)
mod_sim2_x

plot(effect("topology:size", mod_sim2_x))


##VISUALIZATION

low25 <- ggplot(data = avg[size==25 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), ] , aes(x=generation, y=synth, color=topology)) + geom_smooth() +
  ylab("Synthesis") + xlab("Generation") +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1") ) +
  guides(colour = guide_legend(override.aes = list(size=5))) +  ylim(1.35,1.45) + xlim(100, 200) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))


low25

low125 <- ggplot(data = avg[size==125 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), ] , aes(x=generation, y=synth, color=topology)) + geom_smooth() +
  ylab("Synthesis") + xlab("Generation") +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1") ) +
  guides(colour = guide_legend(override.aes = list(size=5))) +  ylim(1.35, 1.45) + xlim(100, 200) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))

low125

h25  <- ggplot(data = avg[size==25 & topology %in% c("1.0", "0.4", "hier"), ] , aes(x=generation, y=synth, color=topology)) + geom_smooth() +
  ylab("Synthesis") + xlab("Generation") +
  scale_color_discrete(name  = "Topology",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1","hier"),
                       labels=c("Complete", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1","Hierarchical") ) +
  guides(colour = guide_legend(override.aes = list(size=5))) +  ylim(1.35,1.45) + xlim(100, 200) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))

h25

h125  <- ggplot(data = avg[size==125 &  topology %in%  c("1.0", "0.4", "hier"), ] , aes(x=generation, y=synth, color=topology)) + geom_smooth() +
  ylab("Synthesis") + xlab("Generation") +
  scale_color_discrete(name  = "Topology",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1", "hier"),
                       labels=c("Complete", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1", "Hierarchical") ) +
  guides(colour = guide_legend(override.aes = list(size=5))) +  ylim(1.35,1.45) + xlim(100, 200) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))
h125