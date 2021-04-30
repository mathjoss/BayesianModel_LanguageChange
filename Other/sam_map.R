
df <- read.csv("/home/mjosserand/Documents/Netlogo/1000pop_biased.csv")
df_map <- df[df$update.algorithm == "MAP",]
df_sam <- df[df$update.algorithm == "SAM",]

df_bis_map <- df_map[2:nrow(df_map),18:ncol(df_map)]
df_bis_sam <- df_sam[2:nrow(df_sam),18:ncol(df_sam)]

df_new_map <- data.frame()
df_new_sam <- data.frame()

for (agent in c(2:102)){

  df_new_map[agent, 1] <- agent
  print(agent)
  
  for (el in 1:nrow(df_bis_map)){
    vec <- c(as.character(df_bis_map[el,]))
    index_agent <- match(as.character(agent), vec)  
    df_new_map[agent, el+1] <- as.integer(vec[index_agent + 1])
  }
}


for (agent in c(2:102)){
  
  df_new_sam[agent, 1] <- agent
  print(agent)
  
  for (el in 1:nrow(df_bis_sam)){
    vec <- c(as.character(df_bis_sam[el,]))
    index_agent <- match(as.character(agent), vec)  
    df_new_sam[agent, el+1] <- as.integer(vec[index_agent + 1])
  }
}


colnames(df_new_map)[1] <- "agent_number"
df_new_map <- df_new_map[-c(1,100), ] 
colnames(df_new_sam)[1] <- "agent_number"
df_new_sam <- df_new_sam[-c(1,100), ] 

df_delta_map <- data.frame()

for (row in 1:nrow(df_new_map)){
  df_delta_map[row,1] <- df_new_map[row,1]
  for (col in 3:ncol(df_new_map)){
    if (df_new_map[row, col]==df_new_map[row, col-1]){
      delta=0
    }
    if (df_new_map[row, col]!=df_new_map[row, col-1]){
      delta=1
    }
    df_delta_map[row, col-1] <- delta
  }
}
colnames(df_delta_map)[1] <- "agent_number"

df_delta_sam <- data.frame()

for (row in 1:nrow(df_new_sam)){
  df_delta_sam[row,1] <- df_new_sam[row,1]
  for (col in 3:ncol(df_new_sam)){
    if (df_new_sam[row, col]==df_new_sam[row, col-1]){
      delta=0
    }
    if (df_new_sam[row, col]!=df_new_sam[row, col-1]){
      delta=1
    }
    df_delta_sam[row, col-1] <- delta
  }
}
colnames(df_delta_sam)[1] <- "agent_number"

library(ggplot2)
library(tidyr)

df_delta_map_gathered <- gather(df_delta_map, time, measurement, V2:V200)
df_delta_map_gathered$time <- gsub("V","",as.character(df_delta_map_gathered$time))
df_delta_map_gathered$time <- as.numeric(df_delta_map_gathered$time)
df_delta_map_gathered$samp_strat <- "MAP"

df_delta_sam_gathered <- gather(df_delta_sam, time, measurement, V2:V200)
df_delta_sam_gathered$time <- gsub("V","",as.character(df_delta_sam_gathered$time))
df_delta_sam_gathered$time <- as.numeric(df_delta_sam_gathered$time)
df_delta_sam_gathered$samp_strat <- "SAM"

df_final <- rbind(df_delta_map_gathered, df_delta_sam_gathered)

ggplot(data=df_final[df_final$agent_number==10 & df_final$time < 100,], aes(x=time, y=measurement)) +
  geom_line() +
  facet_grid(. ~ samp_strat)

av_map_200 <- rowMeans(df_delta_map[,2:200], na.rm=TRUE)
av_sam_200 <- rowMeans(df_delta_sam[,2:200], na.rm=TRUE)

av_map_150 <- rowMeans(df_delta_map[,2:150], na.rm=TRUE)
av_sam_150 <- rowMeans(df_delta_sam[,2:150], na.rm=TRUE)

av_map_100 <- rowMeans(df_delta_map[,2:100], na.rm=TRUE)
av_sam_100 <- rowMeans(df_delta_sam[,2:100], na.rm=TRUE)

av_map_50 <- rowMeans(df_delta_map[,2:50], na.rm=TRUE)
av_sam_50 <- rowMeans(df_delta_sam[,2:50], na.rm=TRUE)

df_mean_map_200 <-data.frame(sampling_stratregy = "MAP", agent_number=df_delta_map$agent_number, mean_delta=av_map_200, time_max=200)
df_mean_sam_200 <-data.frame(sampling_stratregy = "SAM", agent_number=df_delta_sam$agent_number, mean_delta=av_sam_200, time_max=200)
df_mean_map_150 <-data.frame(sampling_stratregy = "MAP", agent_number=df_delta_map$agent_number, mean_delta=av_map_150, time_max=150)
df_mean_sam_150 <-data.frame(sampling_stratregy = "SAM", agent_number=df_delta_sam$agent_number, mean_delta=av_sam_150, time_max=150)
df_mean_map_100 <-data.frame(sampling_stratregy = "MAP", agent_number=df_delta_map$agent_number, mean_delta=av_map_100, time_max=100)
df_mean_sam_100 <-data.frame(sampling_stratregy = "SAM", agent_number=df_delta_sam$agent_number, mean_delta=av_sam_100, time_max=100)
df_mean_map_50 <-data.frame(sampling_stratregy = "MAP", agent_number=df_delta_map$agent_number, mean_delta=av_map_50, time_max=50)
df_mean_sam_50 <-data.frame(sampling_stratregy = "SAM", agent_number=df_delta_sam$agent_number, mean_delta=av_sam_50, time_max=50)

df_mean_100 <- rbind(df_mean_map_200, df_mean_sam_200, df_mean_map_150, df_mean_sam_150,  df_mean_map_100, df_mean_sam_100,  df_mean_map_50, df_mean_sam_50 )
df_mean_100$time_max <- as.factor(df_mean_100$time_max)
ggplot(data=df_mean, aes(x=sampling_stratregy, y = mean_delta, fill=time_max)) +
  geom_boxplot() +
  theme_bw(base_size=14) +
  scale_fill_viridis_d(begin=0.2)

### both....
df_mean_0$prop_biased <- 0
df_mean_50$prop_biased <- 50
df_mean_100$prop_biased <- 100

df_mean_all <- rbind(df_mean_0, df_mean_50, df_mean_100)
df_mean_all$prop_biased <- paste(df_mean_all$prop_biased , "% biased agents", sep="")
df_mean_all$prop_biased <- factor(df_mean_all$prop_biased, levels = c("0% biased agents","50% biased agents", "100% biased agents"))

ggplot(data=df_mean_all[(df_mean_all$prop_biased=="0% biased agents" | df_mean_all$prop_biased=="100% biased agents") & (df_mean_all$time_max==50 | df_mean_all$time_max==150),], aes(x=time_max, y = mean_delta, fill=sampling_stratregy)) +
  geom_boxplot() +
  facet_grid(prop_biased ~ .) +
  theme_bw(base_size=14) +
  scale_fill_viridis_d(begin=0.4) +
  scale_x_discrete(breaks=c("50","150"),
                   labels=c("after 50 iterations", "after 150 iterations")) +
  theme(axis.title.x = element_blank()) 
  



##### -----------------------------------------------------------------------------------------------

## Other type of boxplot 


df2 <- read.csv("/home/mjosserand/Documents/Netlogo/langval_100it_prop100.csv")


df2_gathered <- gather(df2, condition, lang_val, mean.langval:mean.langval.0)
df2_gathered <- df2_gathered[df2_gathered$`X.step.`==50 | df2_gathered$`X.step.`==100 | df2_gathered$`X.step.`==150 | df2_gathered$`X.step.`==200,]

df_100 <- df2_gathered
df_0$prop_biased <- 0
df_100$prop_biased <- 100

df_fin <- rbind(df_0, df_100)
df_fin$prop_biased <- paste(df_fin$prop_biased , "% biased agents", sep="")
df_fin$prop_biased <- factor(df_fin$prop_biased, levels = c("0% biased agents","50% biased agents", "100% biased agents"))

ggplot(data=df_fin[df_fin$condition=="mean.langval" & (df_fin$X.step.=="50 iterations" | df_fin$X.step.=="150 iterations"),], aes(x=X.step., y=lang_val, fill=update.algorithm))+
  geom_boxplot() +
  facet_grid(prop_biased ~ .) +
  theme_bw(base_size=14) +
  scale_fill_viridis_d(begin=0.4) +
  labs(fill = "Sampling strategy", y="Language value") +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(breaks=c("mean.langval","mean.langval.0","mean.langval.1"),
                   labels=c("All agents", "Unbiased agents", "Biased agents (strong, 50% pop)"))

