library(plyr)
library(ggplot2)
library(igraph)
library(data.table)

#############################################################################################################################################
####25-Agent Versions#######################################################################################################################
#############################################################################################################################################

### Create ../data/summary CSVs
#no need to run again unless wanting to recreate summary csv files from original experiment data

##SOCIAL TOPOLOGY EXPERIMENT SUMMARY SIMULATION 4
#read data from social topology experiment

#signals
social_df <- fread("./csv/sim4.csv", header=T, sep=",")

#network structure
social_net <- fread("./csv/sim4_ed.csv", header=T, sep=",")

#assign proper data types
social_df$typo <- as.factor(social_df$typo)
social_df$id <- as.factor(social_df$id)
social_df$gen <-as.numeric(social_df$gen)
social_df$agent <-as.factor(social_df$agent)
social_df$cons <-as.factor(social_df$cons)
social_df$var <- as.factor(social_df$var)
social_df$signal <-as.factor(social_df$signal)
social_df$level <-as.numeric(social_df$level)

social_net$id <- as.factor(social_net$id)
social_net$type <- as.factor(social_net$type)
social_net$a <- as.factor(social_net$a)
social_net$b <- as.factor(social_net$b)

#replace topology level giant with random
levels(social_df$typo)[levels(social_df$typo)=="giant"] <- "random"
levels(social_net$type)[levels(social_net$type)=="giant"] <- "random"

#assign new colummn names
names(social_df) = c("network_id", "topology", "n_agents", "generation","agent_id","meaning","realization","origin","reanalyses")
names(social_net) = c("network_id", "topology","a","b")

#write updated social network structure csv
fwrite(social_net, "sim4_summary_net.csv")

#Ineficient compared to data.table usage below
#create new data frame that averages over all signals for each agent
#social_average <- ddply(social_df[n_agents==25,], c("network_id", "topology", "generation", "agent_id"), summarise, complexity = mean(reanalyses))

social_dt <- data.table(social_df)
social_average <- social_dt[, list(
  complexity = mean(reanalyses)  
), by=c("network_id", "topology", "generation", "agent_id")]

#remove raw unaveraged data frame
remove(social_df)

#create data fram with by-agent structural measures
social_net_agent = data.frame(matrix(ncol = 5, nrow = 0))
social_net_agent_names = c("network_id", "topology", "agent_id", "degree", "transitivity_local")
colnames(social_net_agent) = social_net_agent_names

for (nid in 1:100) { #iterate over each of the 100 replications
  for (nt in c("barabasi","hierarchical","random","complete")){ #and iterate over each of the four conditions
    df <- graph.data.frame(social_net[social_net$id==nid & social_net$type==nt,c(3,4)], directed=FALSE) #generate corresponding graph
    tl = transitivity(df, type="local", isolates="zero") #calculate local transitivity per agent
    td = data.frame(network_id=rep(nid,25), topology=rep(nt,25), agent_id=V(df)$name, degree=as.vector(degree(df)), transitivity_local=tl) #build results into temp data frame for individual graph
    social_net_agent = rbind(social_net_agent, td) #add rows from temp to master data frame of agent structure data
  }
}

#merge data frames together
social_merged <- merge(social_average, social_net_agent, by=c("network_id","agent_id","topology"))

#remove raw unmerged data frames
remove(social_average)
remove(social_net_agent)

#write merged data frame to csv
fwrite(social_merged, "sim4_summary.csv")

###TRANSITIVITY EXPERIMENT SUMMARY SIMULATION 3

#signals
transitivity_df <- fread("./csv/sim3.csv", header=T, sep=",")

#network structure
transitivity_net <- fread("./csv/sim3.csv", header=T, sep=",")

#assign proper data types
transitivity_df$typo <- as.factor(transitivity_df$typo)
transitivity_df$id <- as.factor(transitivity_df$id)
transitivity_df$gen <-as.numeric(transitivity_df$gen)
transitivity_df$agent <-as.factor(transitivity_df$agent)
transitivity_df$cons <-as.factor(transitivity_df$cons)
transitivity_df$var <- as.factor(transitivity_df$var)
transitivity_df$signal <-as.factor(transitivity_df$signal)
transitivity_df$level <-as.numeric(transitivity_df$level)

transitivity_net$id <- as.factor(transitivity_net$id)
transitivity_net$type <- as.factor(transitivity_net$type)
transitivity_net$a <- as.factor(transitivity_net$a)
transitivity_net$b <- as.factor(transitivity_net$b)

#assign new colummn names
names(transitivity_df) = c("network_id", "topology", "n_agents", "generation","agent_id","meaning","realization","origin","reanalyses")
names(transitivity_net) = c("network_id", "topology","a","b")

#write updated transitivity network structure csv
fwrite(transitivity_net, "sim3_summary_net.csv")

#create new data frame that averages over all signals for each agent
transitivity_average <- ddply(transitivity_df[n_agents==25,], c("generation", "topology", "agent_id", "network_id"), summarise, complexity = mean(reanalyses))

transitivity_dt <- data.table(transitivity_df)
transitivity_average <- transitivity_dt[, list(
  complexity = mean(reanalyses)  
), by=c("network_id", "topology", "generation", "agent_id")]

#remove raw unaveraged data frame
remove(transitivity_df)

#create data frame with by-agent structural measures
transitivity_net_agent = data.frame(matrix(ncol = 5, nrow = 0))
transitivity_net_agent_names = c("network_id", "topology", "agent_id", "degree", "transitivity_local")
colnames(transitivity_net_agent) = transitivity_net_agent_names

for (nid in 1:100) { #iterate over each of the 100 replicationsm
  for (nt in c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1")){ #and iterate over each of the 10 conditions
    df <- graph.data.frame(transitivity_net[transitivity_net$id==nid & transitivity_net$type==nt,c(3,4)], directed=FALSE) #generate corresponding graph
    tl = transitivity(df, type="local", isolates="zero") #calculate local transitivity per agent
    td = data.frame(network_id=rep(nid,25), topology=rep(nt,25), agent_id=V(df)$name, degree=as.vector(degree(df)), transitivity_local=tl) #build results into temp data frame for individual graph
    transitivity_net_agent = rbind(transitivity_net_agent, td) #add rows from temp to master data frame of agent structure data
  }
}

prob = c()
gl = c()

for (nid in 1:100) { #iterate over each of the 100 replicationsm
  for (nt in c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1")){ #and iterate over each of the 10 conditions
    df <- graph.data.frame(transitivity_net[transitivity_net$network_id==nid & transitivity_net$topology==nt,c(3,4)], directed=FALSE)
    tg = transitivity(df, type="global", isolates="zero") #calculate local transitivity per agent
    prob = c(prob, nt)
    gl = c(gl, tg)
  }
}

globs = data.table( data.frame(prob, gl) )
globs_average <- globs[, list(
  global = mean(gl)  
), by=c("prob")]
    
#merge data frames together
transitivity_merged <- merge(transitivity_average, transitivity_net_agent, by=c("network_id","agent_id","topology"))

#write merged data frame to csv
fwrite(transitivity_merged, "sim3_summary.csv")

#remove raw unmerged data frames
remove(transitivity_average)
remove(transitivity_net_agent)

#############################################################################################################################################
####125-Agent Versions#######################################################################################################################
#############################################################################################################################################

##SOCIAL TOPOLOGY EXPERIMENT SUMMARY
#read data from social topology experiment

#signals
social_df <- fread("./csv/sim5_social.csv", header=T, sep=",")

#network structure
social_net <- fread("./csv/sim5_social_ed.csv", header=T, sep=",")

#assign proper data types
social_df$typo <- as.factor(social_df$typo)
social_df$id <- as.factor(social_df$id)
social_df$gen <-as.numeric(social_df$gen)
social_df$agent <-as.factor(social_df$agent)
social_df$cons <-as.factor(social_df$cons)
social_df$var <- as.factor(social_df$var)
social_df$signal <-as.factor(social_df$signal)
social_df$level <-as.numeric(social_df$level)

social_net$id <- as.factor(social_net$id)
social_net$type <- as.factor(social_net$type)
social_net$a <- as.factor(social_net$a)
social_net$b <- as.factor(social_net$b)

#replace topology levels
levels(social_df$typo)[levels(social_df$typo)=="randomx"] <- "random"
levels(social_net$type)[levels(social_net$type)=="randomx"] <- "random"

levels(social_df$typo)[levels(social_df$typo)=="completex"] <- "complete"
levels(social_net$type)[levels(social_net$type)=="completex"] <- "complete"

levels(social_df$typo)[levels(social_df$typo)=="hierarchicalx"] <- "hierarchical"
levels(social_net$type)[levels(social_net$type)=="hierarchicalx"] <- "hierarchical"

levels(social_df$typo)[levels(social_df$typo)=="barabasix"] <- "barabasi"
levels(social_net$type)[levels(social_net$type)=="barabasix"] <- "barabasi"

#assign new colummn names
names(social_df) = c("network_id", "topology", "n_agents", "generation","agent_id","meaning","realization","origin","reanalyses")
names(social_net) = c("network_id", "topology","a","b")

#remove parens from network_id columns in network structure data
social_net$network_id <- gsub("\\(", "", social_net$network_id)
social_net$network_id <- gsub("\\)", "", social_net$network_id)

#write updated social network structure csv
fwrite(social_net, "sim5_social_summary_net.csv")

social_dt <- data.table(social_df)
social_average <- social_dt[, list(
  complexity = mean(reanalyses)  
), by=c("network_id", "topology", "generation", "agent_id")]

#remove raw unaveraged data frame
remove(social_df)

#create data fram with by-agent structural measures
social_net_agent = data.frame(matrix(ncol = 5, nrow = 0))
social_net_agent_names = c("network_id", "topology", "agent_id", "degree", "transitivity_local")
colnames(social_net_agent) = social_net_agent_names

for (nid in 1:50) { #iterate over each of the 50 replications
  for (nt in c("barabasi","hierarchical","random","complete")){ #and iterate over each of the four conditions
    df <- graph.data.frame(social_net[social_net$network_id==nid & social_net$topology==nt,c(3,4)], directed=FALSE) #generate corresponding graph
    tl = transitivity(df, type="local", isolates="zero") #calculate local transitivity per agent
    td = data.frame(network_id=rep(nid,125), topology=rep(nt,125), agent_id=V(df)$name, degree=as.vector(degree(df)), transitivity_local=tl) #build results into temp data frame for individual graph
    social_net_agent = rbind(social_net_agent, td) #add rows from temp to master data frame of agent structure data
  }
}

#merge data frames together
social_merged <- merge(social_average, social_net_agent, by=c("network_id","agent_id","topology"))

#remove raw unmerged data frames
remove(social_average)
remove(social_net_agent)

#write merged data frame to csv
fwrite(social_merged, "sim5_social_summary.csv")

###TRANSITIVITY EXPERIMENT SUMMARY

#signals
transitivity_df <- fread("./csv/sim5_transitivity.csv", header=T, sep=",")

#network structure
transitivity_net <- fread("./csv/sim5_transitivity.csv", header=T, sep=",")

#assign proper data types
transitivity_df$typo <- as.factor(transitivity_df$typo)
transitivity_df$id <- as.factor(transitivity_df$id)
transitivity_df$gen <-as.numeric(transitivity_df$gen)
transitivity_df$agent <-as.factor(transitivity_df$agent)
transitivity_df$cons <-as.factor(transitivity_df$cons)
transitivity_df$var <- as.factor(transitivity_df$var)
transitivity_df$signal <-as.factor(transitivity_df$signal)
transitivity_df$level <-as.numeric(transitivity_df$level)

transitivity_net$id <- as.factor(transitivity_net$id)
transitivity_net$type <- as.factor(transitivity_net$type)
transitivity_net$a <- as.factor(transitivity_net$a)
transitivity_net$b <- as.factor(transitivity_net$b)

#replace x in topology level names
levels(transitivity_df$typo)[levels(transitivity_df$typo)=="completex"] <- "1.0"
levels(transitivity_net$type)[levels(transitivity_net$type)=="completex"] <- "1.0"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.9x"] <- "0.9"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.9x"] <- "0.9"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.8x"] <- "0.8"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.8x"] <- "0.8"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.7x"] <- "0.7"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.7x"] <- "0.7"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.6x"] <- "0.6"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.6x"] <- "0.6"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.5x"] <- "0.5"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.5x"] <- "0.5"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.4x"] <- "0.4"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.4x"] <- "0.4"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.3x"] <- "0.3"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.3x"] <- "0.3"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.2x"] <- "0.2"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.2x"] <- "0.2"

levels(transitivity_df$typo)[levels(transitivity_df$typo)=="0.1x"] <- "0.1"
levels(transitivity_net$type)[levels(transitivity_net$type)=="0.1x"] <- "0.1"


#assign new colummn names
names(transitivity_df) = c("network_id", "topology", "n_agents", "generation","agent_id","meaning","realization","origin","reanalyses")
names(transitivity_net) = c("network_id", "topology","a","b")

#remove parens from network_id columns in network structure data
transitivity_net$network_id <- gsub("\\(", "", transitivity_net$network_id)
transitivity_net$network_id <- gsub("\\)", "", transitivity_net$network_id)

#write updated transitivity network structure csv
fwrite(transitivity_net, "sim5_transitivity_summary_net.csv")

#create new data frame that averages over all signals for each agent

transitivity_dt <- data.table(transitivity_df)
transitivity_average <- transitivity_dt[, list(
  complexity = mean(reanalyses)  
), by=c("network_id", "topology", "generation", "agent_id")]

#remove raw unaveraged data frame
remove(transitivity_df)

#create data frame with by-agent structural measures
transitivity_net_agent = data.frame(matrix(ncol = 5, nrow = 0))
transitivity_net_agent_names = c("network_id", "topology", "agent_id", "degree", "transitivity_local")
colnames(transitivity_net_agent) = transitivity_net_agent_names

for (nid in 1:50) { #iterate over each of the 50 replicationsm
  for (nt in c("1.0","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1")){ #and iterate over each of the 10 conditions
    df <- graph.data.frame(transitivity_net[transitivity_net$network_id==nid & transitivity_net$topology==nt,c(3,4)], directed=FALSE) #generate corresponding graph
    tl = transitivity(df, type="local", isolates="zero") #calculate local transitivity per agent
    td = data.frame(network_id=rep(nid,125), topology=rep(nt,125), agent_id=V(df)$name, degree=as.vector(degree(df)), transitivity_local=tl) #build results into temp data frame for individual graph
    transitivity_net_agent = rbind(transitivity_net_agent, td) #add rows from temp to master data frame of agent structure data
  }
}

#merge data frames together
transitivity_merged <- merge(transitivity_average, transitivity_net_agent, by=c("network_id","agent_id","topology"))

#write merged data frame to csv
fwrite(transitivity_merged, "sim5_transitivity_summary.csv")

#remove raw unmerged data frames
remove(transitivity_average)
remove(transitivity_net_agent)