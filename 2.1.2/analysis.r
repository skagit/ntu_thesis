##################################################################  HELPER FUNCTIONS

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

##################################################################  ANALYSIS CODE BEGINS HERE

library(plyr) #should migrate to newer version- dplyr
library(ggplot2) 
library(igraph)
library(data.table) #fastest option for reading, writing, and manipulating large data sets - often hundreds to thousands of times faster than plyr and dplyr
library(lme4)
library(gridExtra)
library(grid)

trans <- fread("data_csv/transmission.csv")

ties <- fread("data_csv/transmission_ties.csv")

#explore relationships
avg <- trans[, list(prop=sum(innovated)/length(innovated)), by=c("topology","size","generation")]

avg_all <- trans[generation > 10, list(prop=sum(innovated)/length(innovated)), by=c("topology","size")]

avg_all[size==125,][order(prop),]
avg_all[size==25,][order(prop),]

qplot(generation, prop, data=avg[size==125,  ], color=as.factor(topology), geom="line")
qplot(generation, prop, data=avg[size==25,  ], color=as.factor(topology), geom="line")


#create data fram with by-agent structural measures
ties_agent = data.frame(matrix(ncol = 6, nrow = 0))
ties_agent_names = c("topology", "size", "replication", "agent_id", "degree", "transitivity_local")
colnames(ties_agent) = ties_agent_names

for ( sz in c(25,125) ) {
  for ( nid in 1:30 ) { #iterate over each of the 100 replications
    for (nt in c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1", "hierarchical") ) { #and iterate over each of the four conditions
      df <- graph.data.frame(ties[ties$size==sz & ties$replication==nid & ties$topology==nt, c(4,5)], directed=FALSE) #generate corresponding graph
      tl = transitivity(df, type="local", isolates="zero") #calculate local transitivity per agent
      td = data.frame( topology = rep(nt, sz), size = rep(sz ,sz), replication = rep(nid, sz),  agent_id=V(df)$name, degree=as.vector(degree(df)), transitivity_local=tl) #build results into temp data frame for individual graph
      ties_agent = rbind(ties_agent, td) #add rows from temp to master data frame of agent structure data
    }
  }
}

ties_agent$agent_id <- as.integer(ties_agent$agent_id)

#merge data frames together
trans <- merge(trans, ties_agent, by=c("topology", "size", "replication", "agent_id"))

##VISUALIZATION

trans25 <- ggplot(data = avg[size==25 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), ] , aes(x=generation, y=prop, color=topology)) + geom_point() + geom_line() + 
  ylab("Proportion of Innovators") + xlab("Generation") +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1") )  +  ylim(0,1) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))


trans25

trans125 <- ggplot(data = avg[size==125 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), ] , aes(x=generation, y=prop, color=topology)) + geom_point() + geom_line() + 
  ylab("Proportion of Innovators") + xlab("Generation") +
  scale_color_discrete(name  = "Connection Probability",
                       breaks=c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"),
                       labels=c("1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1") ) +  ylim(0,1) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.text = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y =element_text(size=24))


trans125

#### local transitivty vs. complexity

head(trans)
head(ties_agent)

#### 25 AGENT

transitivity_local <- trans[size==25 & generation >= 20 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), list(
  prop=sum(innovated)/length(innovated)
), by=c("transitivity_local", "generation")]

transitivity_local_average <- transitivity_local[, list(
   prop=mean(prop)
), by=c("transitivity_local")]

gen1 <- ggplot(
  data = transitivity_local[generation==10 ,], aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0.8,1) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nGeneration 10")

gen10 <- ggplot(
  data = transitivity_local[generation==15 ,], aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0.8,1) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nGeneration 15")

gen100 <- ggplot(
  data = transitivity_local[generation==25,], ylim=c(0,1), aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0.8,1) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nGeneration 25")

gen_avg <- ggplot(
  data = transitivity_local_average, aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0.8,1) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nAll Generations")

multiplot(gen1, gen100, gen10, gen_avg, cols = 2)

h2 <- ggplot(data=trans[generation==1 & topology==0.2 & size==25, ], aes(transitivity_local)) + geom_histogram() + ylim(0,200) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.2")
h3 <- ggplot(data=trans[generation==1 & topology==0.8 & size==25, ], aes(transitivity_local)) + geom_histogram() + ylim(0,200) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.8")
h4 <- ggplot(data=trans[generation==1 & topology==0.9 & size==25, ], aes(transitivity_local)) + geom_histogram() + ylim(0,200) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.9")
h5 <- ggplot(data=trans[generation==1 & topology==0.5 & size==25, ], aes(transitivity_local)) + geom_histogram() + ylim(0,200) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.5")

multiplot(h4,h2,h5,h3, cols=2)

#### 125 AGENT

transitivity_local <- trans[size==125 & generation <= 100 & topology %in% c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1"), list(
  prop=sum(innovated)/length(innovated)
), by=c("transitivity_local", "generation")]

transitivity_local_average <- transitivity_local[, list(
  prop=mean(prop)
), by=c("transitivity_local")]

gen1 <- ggplot(
               data = transitivity_local[generation==1,], aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0,0.5) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nGeneration 1")

gen10 <- ggplot(
  data = transitivity_local[generation==10,], aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0,0.5) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nGeneration 100")

gen100 <- ggplot(
  data = transitivity_local[generation==100,], ylim=c(0,1), aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0,0.5) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nGeneration 1000")

gen_avg <- ggplot(
  data = transitivity_local_average, aes(x = transitivity_local, y = prop)) + 
  geom_point() + geom_smooth() + ylim(0,0.5) +
  ylab("Proportion of Innovation") + xlab("Local Transitivity\nAll Generations")

multiplot(gen1, gen1000, gen100, gen_avg, cols = 2)

h2 <- ggplot(data=trans[generation==1 & topology==0.2 & size==125, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.2")
h3 <- ggplot(data=trans[generation==1 & topology==0.3 & size==125, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.3")
h4 <- ggplot(data=trans[generation==1 & topology==0.4 & size==125, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.4")
h5 <- ggplot(data=trans[generation==1 & topology==0.5 & size==125, ], aes(transitivity_local)) + geom_histogram() + ylim(0,600) + xlim(0,1) +
  ylab("Number of Agents") + xlab("Local Transitivity\nConnection Probability 0.5")

multiplot(h4,h2,h5,h3, cols=2)

