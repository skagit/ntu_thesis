#load dependencies
library(plyr) #should migrate to newer version- dplyr
library(ggplot2) 
library(igraph)
library(data.table) #fastest option for reading, writing, and manipulating large data sets - often hundreds to thousands of times faster than plyr and dplyr
library(lme4)
library(gridExtra)
library(grid)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#############################################################################################################################################
#### 25-Agent Versions ######################################################################################################################
#############################################################################################################################################

###SOCIAL TOPOLOGY EXPERIMENT Simulation 3 ################################################################################################################ 

##PREPARE DATA

#load signal data
social_dt <- fread("./sim3_summary_.csv")

#convert to appropriate variable types
social_dt$network_id <- as.factor(social_dt$network_id)
social_dt$agent_id <- as.factor(social_dt$agent_id)
social_dt$topology <- as.factor(social_dt$topology)

#average over all agents in each network, i.e. one complexity score per each generation of each network run
social_average <- social_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology", "network_id")]

##VISUALIZATION
social_main25 <- ggplot(social_dt[, list(
        complexity = mean(complexity)), 
        by=c("generation", "topology")],
  aes(x=generation, y=complexity, color=topology)) + geom_point() + geom_line() + 
  scale_color_manual(name  = "Network",
                     values=c(hue_pal()(4)),
                       breaks=c("complete", "random", "hierarchical", "barabasi"),
                       labels=c("Complete", "Random", "Hierarchical", "Barabasi-Albert")) +
  ylab("Mean Level of Reanalysis") + xlab("Generation") +
guides(colour = guide_legend(override.aes = list(size=5))) +  ylim(0,20) +
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))

social_dt$agent_type <- "Non-Hub      "
social_dt$agent_type[ social_dt$agent_id == 5 ] <- "Hub"

social_dt$agent_group[ social_dt$agent_id %in% 1:5 ]<- "Central"
social_dt$agent_group[ social_dt$agent_id %in% 6:10 ] <- "Top Left"
social_dt$agent_group[ social_dt$agent_id %in% 11:15 ] <- "Top Right"
social_dt$agent_group[ social_dt$agent_id %in% 16:20 ] <- "Bottom Left"
social_dt$agent_group[ social_dt$agent_id %in% 21:25 ] <- "Bottom Right"

nogroup <- ggplot(
  data= social_dt[network_id == 3 & generation >= 0 & generation <= 1000 & stage=="birth", list(
    complexity = mean(complexity)), by=c("generation", "network_id")], 
  aes(x = generation, y = complexity) ) + 
  geom_line() + ylim(0,15) +
  ylab("Mean Reanalysis") + xlab("Generation")

drop_group <- ggplot(
  data= social_dt[network_id == 3 & generation >= 480 & generation <= 510 & stage=="birth", list(
    complexity = mean(complexity)), by=c("generation", "network_id", "agent_group")], 
  aes(x = generation, y = complexity, color = agent_group) ) + 
  geom_line() + ylim(0,10) +
  ylab("Mean Reanalysis") + xlab("Generation") +
  scale_color_discrete(name  = "Position") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))

drop_type <- ggplot(
  data= social_dt[network_id == 3 & generation >= 480 & generation <= 510 & stage=="birth", list(
    complexity = mean(complexity)), by=c("generation", "network_id", "agent_type")], 
  aes(x = generation, y = complexity, color = agent_type) ) + 
  geom_line() + ylim(0,10) +
  ylab("Mean Reanalysis") + xlab("Generation") + 
  scale_color_discrete(name  = "Agent Type") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))

multiplot(nogroup, drop_group, drop_type)


##ANALYSIS
df <- social_dt
df2 <- df[topology %in% c("barabasi", "hierarchical", "complete", "random"), ]
df2$topology <- as.factor(df2$topology)
df2$size <- as.factor(df2$size)
df2$topology <- factor(df2$topology, levels=c("hierarchical", "complete", "random", "barabasi"))

mod_sim2_25  <- lmer(complexity ~ topology + (1 | network_id ) , data = df2[df2$size==25 & df2$generation>100,])

summary(mod_sim2_25)

df3 <- df[topology %in% c("1.0", "hier"), ]
df3$topology <- as.factor(df3$topology)
df3$size <- as.factor(df3$size)
df3$topology <- factor(df3$topology, levels=c("hier", "1.0"))

mod_sim2_x <- lmer(synthesis ~ topology * size + (1 | network_id ) , data = df3[generation>100,])

summary(mod_sim2_x)

###TRANSITIVITY EXPERIMENT  SIMULATION 4 ###################################################################################################################

##PREPARE DATA

#load signal data
transitivity_dt <- fread("sim4_summary.csv")

#convert to appropriate variable types
transitivity_dt$network_id <- as.factor(transitivity_dt$network_id)
transitivity_dt$agent_id <- as.factor(transitivity_dt$agent_id)

#average over all agents for each network
transitivity_average<- transitivity_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology", "network_id")]


##VISUALIZATION

#average over all agents and networks (old analysis)
transitivity_average_old <- transitivity_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology")]

non_lin <- transitivity_dt[ generation >= 950, list(
  complexity = mean(complexity)
), by=c("topology","generation")]

#add column treating transitivity as a groupable factor for graphing purposes
transitivity_average_old$topology_group <- as.factor(transitivity_average_old$topology)

ggplot(data = transitivity_average_old, 
  aes(x=generation, y=complexity, color=topology_group)) + geom_point() + geom_line() +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1")) +
  ylab("Mean Level of Reanalysis") + xlab("Generation") +
  guides(colour = guide_legend(override.aes = list(size=5))) +  ylim(0,15) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.title.y =element_text(size=14))
        
qplot(x=topology, y=complexity, data=non_lin)

errs <- summarySE(non_lin, measurevar = "complexity", groupvars=c("topology"))

ggplot(data = errs, 
       aes(x=topology, y=complexity)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=complexity-se, ymax=complexity+se, width=.05)) +
  ylab("Mean Level of Reanalysis") + xlab("Transitivity") + 
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))



##ANALYSIS
df1 <- transitivity_dt
df1 <- df[topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),]
df1$topology <- as.numeric(df1$topology)
mod_sim1_25 <- lmer(complexity ~ topology + (1 | network_id ), data = df1[size==25 & generation == 100, ])

summary(mod_sim1_25)

#### local transitivty vs. complexity

head(transitivity_dt)
transitivity_local <- transitivity_dt[, list(
  complexity = mean(complexity)
), by=c("transitivity_local", "generation")]

transitivity_local_average <- transitivity_local[ , list(
  complexity = mean(complexity)
), by=c("transitivity_local")]

gen1 <- ggplot(
  data = transitivity_local[generation==1,], aes(x = transitivity_local, y = complexity)) + 
  geom_point() + geom_smooth() + ylim(0,16) +
  ylab("Mean Level of Reanalysis") + xlab("Local Transitivity\nGeneration 1")

gen100 <- ggplot(
  data = transitivity_local[generation==100,], aes(x = transitivity_local, y = complexity)) + 
  geom_point() + geom_smooth() + ylim(0,16) +
  ylab("Mean Level of Reanalysis") + xlab("Local Transitivity\nGeneration 100")

gen1000 <- ggplot(
  data = transitivity_local[generation==1000,], ylim=c(0,15), aes(x = transitivity_local, y = complexity)) + 
  geom_point() + geom_smooth() + ylim(0,16) +
  ylab("Mean Level of Reanalysis") + xlab("Local Transitivity\nGeneration 1000")

gen_avg <- ggplot(
  data = transitivity_local_average, aes(x = transitivity_local, y = complexity)) + 
  geom_point() + geom_smooth() + ylim(0,16) +
  ylab("Mean Level of Reanalysis") + xlab("Local Transitivity\nAll Generations")

multiplot(gen1, gen1000, gen100, gen_avg, cols = 2)

h2 <- ggplot(data=transitivity_dt[generation==1 & topology==0.2, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.2")
h3 <- ggplot(data=transitivity_dt[generation==1 & topology==0.3, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.3")
h4 <- ggplot(data=transitivity_dt[generation==1 & topology==0.4, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.4")
h5 <- ggplot(data=transitivity_dt[generation==1 & topology==0.5, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.5")

multiplot(h4,h2,h5,h3, cols=2)

#############################################################################################################################################
#### 125-Agent Versions #####################################################################################################################
#############################################################################################################################################

###SOCIAL TOPOLOGY EXPERIMENT################################################################################################################ 

##PREPARE DATA

#load signal data
social_dt <- fread("sim5_social.csv")

#convert to appropriate variable types
social_dt$network_id <- as.factor(social_dt$network_id)
social_dt$agent_id <- as.factor(social_dt$agent_id)
social_dt$topology <- as.factor(social_dt$topology)

#average over all agents in each network, i.e. one complexity score per each generation of each network run
social_average <- social_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology", "network_id")]

##VISUALIZATION
social_main125 <- ggplot(social_dt[ topology %in% c("complete","hierarchical"), list(
  complexity = mean(complexity)), 
  by=c("generation", "topology")],
  aes(x=generation, y=complexity, color=topology)) + geom_point() + geom_line() +
  scale_color_manual(name  = "Network",
                       values = c("#7CAE00","#00BFC4"),
                       breaks=c("complete", "hierarchical"),
                       labels=c("Complete", "Hierarchical")) +
  ylab("Mean Level of Reanalysis") + xlab("Generation") +
  guides(colour = guide_legend(override.aes = list(size=5))) + ylim(0,20) +
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))

social_main125

social_si <- ggplot(social_dt[, list(
  complexity = mean(complexity)), 
  by=c("generation", "topology")],
  aes(x=generation, y=complexity, color=topology)) + geom_point() + geom_line() +
  scale_color_discrete(name  = "Network",
                     breaks=c("complete", "random", "hierarchical", "barabasi"),
                     labels=c("Complete", "Random", "Hierarchical", "Barabasi")) +
  ylab("Mean Level of Reanalysis") + xlab("Generation") +
  guides(colour = guide_legend(override.aes = list(size=5))) + ylim(0,20) +
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))

##ANALYSIS

df <- social_dt
df2 <- df[topology %in% c("barabasi", "hierarchical", "complete", "random"), ]
df2$topology <- as.factor(df2$topology)
df2$size <- as.factor(df2$size)
df2$topology <- factor(df2$topology, levels=c("hierarchical", "complete", "random", "barabasi"))

mod_sim2_25  <- lmer(complexity ~ topology + (1 | network_id ) , data = df2[df2$size==125 & df2$generation>100,])

summary(mod_sim2_25)

df3 <- df[topology %in% c("1.0", "hier"), ]
df3$topology <- as.factor(df3$topology)
df3$size <- as.factor(df3$size)
df3$topology <- factor(df3$topology, levels=c("hier", "1.0"))

mod_sim2_x <- lmer(complexity ~ topology * size + (1 | network_id) , data = df3[generation>100,])

###TRANSITIVITY EXPERIMENT###################################################################################################################

##PREPARE DATA

#load signal data
transitivity_dt <- fread("sim5_transitivity.csv")

#convert to appropriate variable types
transitivity_dt$network_id <- as.factor(transitivity_dt$network_id)
transitivity_dt$agent_id <- as.factor(transitivity_dt$agent_id)

#average over all agents for each network
transitivity_average<- transitivity_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology", "network_id")]


##VISUALIZATION

#average over all agents and networks (old analysis)
transitivity_average_old <- transitivity_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology")]

#add column treating transitivity as a groupable factor for graphing purposes
transitivity_average_old$topology_group <- as.factor(transitivity_average_old$topology)

ggplot(transitivity_average_old, 
       aes(x=generation, y=complexity, color=topology_group)) + geom_point() + geom_line() +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1")) + ylim(0,20) +
  ylab("Mean Level of Reanalysis") + xlab("Generation") + #ggtitle("Reanalysis by Transitivity") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size=14))

##ANALYSIS

df1 <- transitivity_dt
df1 <- df[topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),]
df1$topology <- as.numeric(df1$topology)
mod_sim1_25 <- lmer(complexity ~ topology + (1 | network_id ), data = df1[size==125 & generation == 100, ])

summary(mod_sim1_25)


#############################################################################################################################################
#### Network Visualization ##################################################################################################################
#############################################################################################################################################

social_dt <- fread("../data/summary/transitivity_net.csv")
example_0.2 <- social_dt[social_dt$network_id==1 & social_dt$topology=="0.2", c(3,4)]
example_0.7 <- social_dt[social_dt$network_id==1 & social_dt$topology=="0.7", c(3,4)]

example_0.2_graph <- graph.data.frame(example_0.2, directed=FALSE)
example_0.7_graph <- graph.data.frame(example_0.7, directed=FALSE)

plot(example_0.2_graph, layout=layout.circle, vertex.label=NA, vertex.color="black")
plot(example_0.7_graph, layout=layout.circle, vertex.label=NA, vertex.color="black")

social_dt <- fread("../data/summary/social.csv")
social125_dt <- fread("../data/summary/125social.csv")



#############################################################################################################################################
#### Network Structure ######################################################################################################################
#############################################################################################################################################

###WORKING WITH GRAPH DATA

#load network data
social_net <- fread("../data/summary/social_net.csv")

#to create a graph from a data frame, igraph expects two columns of data, where each row specifies an edge of the graph
#our network data include additional information about the network_id and topology so we filter this out when constructing a graph
example_random <- social_net[social_net$network_id==85 & social_net$topology=="hierarchical", c(3,4)]
example_random_graph <- graph.data.frame(example_random, directed=FALSE)
plot(example_random_graph)

#gloabl clustering coeficient
transitivity(example_random_graph, type="global", isolates="zero")

#local clustering coeficient
transitivity(example_random_graph, type="local", isolates="zero")

#degree
degree(example_random_graph)

#for igraph functions (like the local transitivity above) that produce agent by agent results, the vector of results is ordered according to the vertex list
V(example_random_graph)

#the vertex list makes no guarantee about ordering according to the vertex names, so you cannot assume that, say, the value of index 5 in a result like the local transitivity output corresponds to agent number 5 in the graph plot

#############################################################################################################################################
#### 25-Agent Versions BIRTH DEATH ##########################################################################################################
#############################################################################################################################################

###SOCIAL TOPOLOGY EXPERIMENT################################################################################################################ 

##PREPARE DATA

#load signal data
social_dt <- fread("../data/summary/bd.csv")

#convert to appropriate variable types
social_dt$network_id <- as.factor(social_dt$network_id)
social_dt$agent_id <- as.factor(social_dt$agent_id)
social_dt$topology <- as.factor(social_dt$topology)
social_dt$stage <- as.factor(social_dt$stage)

#average over all agents in each network, i.e. one complexity score per each generation of each network run
social_average <- social_dt[, list(
  complexity = mean(complexity)
), by=c("generation", "topology", "network_id")]



#########################################
###ISSUES MISC###########################
#########################################
##NESTING SYNTAX
#should be the same as grouping using network_id_unique above in model c_t, not sure why 
c_t_nest <- lmer(complexity ~ topology + (1 | topology/network_id), data = transitivity_average[transitivity_average$generation>=500,])
summary(c_t)
summary(c_t_nest) # has additional ten random effects groups under topology

##GLMER POISSON
#convert complexity measures to integer for poisson distribution
transitivity_average$complexity <- as.integer(transitivity_average$complexity)

#model fails to converge
c_t_glmer <- glmer(complexity ~ topology + (1 | network_id_unique), data = transitivity_average[transitivity_average$generation>=500,], family=poisson())

summary(c_t_glmer)

##OLD ANOVA (unaveraged over multiple final generations)
transitivity_average$topology_group <- as.factor(transitivity_average$topology)
transitivity_aov <- aov(complexity ~ topology_group, data = transitivity_average[transitivity_average$generation==100,])
summary(transitivity_aov)
TukeyHSD(transitivity_aov)
