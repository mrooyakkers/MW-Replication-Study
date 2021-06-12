# Data Cleaning
# Import dataset
rm(list=ls())
main_df <- read.csv("main_df.csv")

# Removing the subjects with greater than 15% errors
id_codes <- unique(main_df$ID)
list_id <- NULL
list_cor <- NULL

for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- main_df[main_df$ID == id,]
  n <- c(current[ , seq(6, 34, by = 2)])
  perc_correct <- sum(unlist(n))/600
  list_id <- c(list_id, id)
  list_cor <- c(list_cor, perc_correct)
}

check <- as.data.frame(cbind(list_id, list_cor))
list <- print(check$list_id[check$list_cor < 0.85])
length(list)
df_edit <- main_df 

# Removing bad subs from df_edit 
for (i in 1:length(list)) {
  df_edit <- df_edit[df_edit$ID != list[i], ]  
}

# Removing responses that are longer than 3 seconds
df_edit[,7:35][df_edit[,7:35] >= 3] <- NA

# Identifying the percetage of trials removed 
RT_vector <- c(df_edit[ , seq(7, 35, by = 2)])
sum(is.na(unlist(RT_vector))) / length(unlist(RT_vector))

# Loop that describes percentage of participants dropped from each condition
impl_cond <- 0
expl_cond <- 0
for (i in 1:length(list)) {
  id <- list[i]
  current <- main_df[main_df$ID == id,]
  if (current$Condition[1] == 'b') {
    impl_cond <- impl_cond + 1
  } else if (current$Condition[1] == 'a') {
    expl_cond <- expl_cond + 1
  }
}

# 115 successfully completed the experiment, then we drop:
impl_cond/(impl_cond+expl_cond) 
expl_cond/(impl_cond+expl_cond) 

# Checking remaining condition length
explicit <- df_edit[df_edit$Condition == "a",]
length(unique(explicit$ID)) 
implicit <- df_edit[df_edit$Condition == "b",]
length(unique(implicit$ID)) 

# Grabbing rcentage of triplets for the training sequence across conditions.
get_acc <- function(sequence, responses){
  seq <- c(sequence, sequence)
  triplets <- c()
  for(i in 1:(length(seq)-2)){
    triplets = c(triplets, paste(seq[i:(i+2)], collapse = ""))
  }
  triplets = unique(triplets)
  acc <- 0
  resp <- c()
  for(i in 1:(length(responses)-2)){
    resp = c(resp, paste(responses[i:(i+2)], collapse = ""))
    if(paste(responses[i:(i+2)], collapse = "") %in% triplets == TRUE){
      acc <- acc+1
    }
  }
  out = list(
    ACC = acc/(length(responses)-2),
    N = length(responses)-2
  )
  return (out)
}

# Inclusion trial for the implicit condition
id_codes <- unique(implicit$ID)
acc <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  response <- head(current$Colour_Resp_Replicate, -10)
  sequence <- rep(c('r', 'g', 'b', 'y', 'g'), 8)
  accuracy <- get_acc(sequence, response)
  acc <- c(acc, accuracy$ACC)
}
implicit_inclusion <- as.data.frame(cbind(id_codes, acc))
mean(implicit_inclusion$acc) 

# Inclusion trial for the explicit condition: 
id_codes <- unique(explicit$ID)
acc <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  response <- head(current$Colour_Resp_Replicate, -10)
  sequence <- rep(c('r', 'g', 'b', 'y', 'g'), 8)
  accuracy <- get_acc(sequence, response)
  acc <- c(acc, accuracy$ACC)
}
explicit_inclusion <- as.data.frame(cbind(id_codes, acc))
mean(acc) 

# Exclusion trial for implicit condition:
id_codes <- unique(implicit$ID)
acc <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  response <- head(current$Colour_Resp_Random, -10)
  sequence <- rep(c('r', 'g', 'b', 'y', 'g'), 8)
  accuracy <- get_acc(sequence, response)
  acc <- c(acc, accuracy$ACC)
}
implicit_exclusion <- as.data.frame(cbind(id_codes, acc))
mean(acc) 

# Exclusion trial for explicit condition: 
id_codes <- unique(explicit$ID)
acc <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  response <- head(current$Colour_Resp_Random, -10)
  sequence <- rep(c('r', 'g', 'b', 'y', 'g'), 8)
  accuracy <- get_acc(sequence, response)
  acc <- c(acc, accuracy$ACC)
}
explicit_exclusion <- as.data.frame(cbind(id_codes, acc))
mean(acc) 

# Checking if the inclusion tasks occured above chance
t.test(implicit_inclusion$acc, implicit_exclusion$acc, paired = TRUE)
t.test(explicit_inclusion$acc, explicit_exclusion$acc, paired = TRUE)

# Plot for Triplets 
library(ggplot2)
library(dplyr)
library(stats)
# Figure 2
data <- data.frame(
  Condition = c("Implicit", "Explicit"),
  Training  = c(mean(implicit_inclusion$acc)*100, mean(explicit_inclusion$acc)*100),
  Random = c(mean(implicit_exclusion$acc)*100, mean(explicit_exclusion$acc)*100)
)

# proper matrix
bilan <- aggregate(cbind(Training,Random) ~ Condition , data=data, sum)
rownames(bilan) <- bilan[,1]
bilan <- as.matrix(bilan[,-1])
colnames(bilan) <- c("Training Sequence", "Random Sequence")

# Plot boundaries
lim <- 1.4*max(bilan)
barplot <- barplot(bilan, beside = T, legend.text=T, 
                   col = c("#FF7547", "#0096A3"), 
                   args.legend = list(x = "topright", inset = c(0.05, 0.15)),
                   ylim = c(0,lim), ylab = "Percentage of Triplets")

# add error bars
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev <- aggregate(cbind(Training,Random)~Condition , data=data, sum)
rownames(stdev) <- stdev[,1]
stdev <- as.matrix(stdev[,-1]) * 1.96 / 10
error.bar(barplot,bilan, stdev)

# Subjective Experience Probes 
# Implicit 
# Was the sequence of large square colours random? 
id_codes <- unique(implicit$ID)
question1 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  q1 <- current$Aware_Q1[1]
  question1 <- c(question1, q1)
}
length(question1[question1 == 'y']) / length(question1) 
length(question1[question1 == 'n']) / length(question1) 

# For the large squares, did some colours occur more than others? 
id_codes <- unique(implicit$ID)
question2 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  q1 <- current$Aware_Q2[1]
  question2 <- c(question2, q1)
}

length(question2[question2 == 'y']) / length(question2) 
length(question2[question2 == 'n']) / length(question2) 

# For the large squares, the succession of colors was often predictable? 
id_codes <- unique(implicit$ID)
question3 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  q3 <- current$Aware_Q3[1]
  question3 <- c(question3, q3)
}

length(question3[question3 == 'y']) / length(question3) 
length(question3[question3 == 'n']) / length(question3) 

# For the large squares, did one sequence of colors often occur?
id_codes <- unique(implicit$ID)
question4 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  q4 <- current$Aware_Q4[1]
  question4 <- c(question4, q4)
}
length(question4[question4 == 'y']) / length(question4) 
length(question4[question4 == 'n']) / length(question4) 

# For the large squares, did one sequence of colors repeat throughout the experiment? 
id_codes <- unique(implicit$ID)
question5 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  q4 <- current$Aware_Q5[1]
  question5 <- c(question5, q4)
}

length(question5[question5 == 'y']) / length(question5) 
length(question5[question5 == 'n']) / length(question5) 

# Explicit  
# Was the sequence of large square colours random? 
id_codes <- unique(explicit$ID)
question1 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  q1 <- current$Aware_Q1[1]
  question1 <- c(question1, q1)
}
length(question1[question1 == 'y']) / length(question1) 
length(question1[question1 == 'n']) / length(question1) 

# For the large squares, did some colours occur more than others? 
id_codes <- unique(explicit$ID)
question2 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  q1 <- current$Aware_Q2[1]
  question2 <- c(question2, q1)
}
length(question2[question2 == 'y']) / length(question2) 
length(question2[question2 == 'n']) / length(question2) 

# For the large squares, the succession of colors was often predictable? 
id_codes <- unique(explicit$ID)
question3 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  q3 <- current$Aware_Q3[1]
  question3 <- c(question3, q3)
}
length(question3[question3 == 'y']) / length(question3) 
length(question3[question3 == 'n']) / length(question3) 

# For the large squares, did one sequence of colors often occur?
id_codes <- unique(explicit$ID)
question4 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  q4 <- current$Aware_Q4[1]
  question4 <- c(question4, q4)
}
length(question4[question4 == 'y']) / length(question4) 
length(question4[question4 == 'n']) / length(question4) # 

# For the large squares, did one sequence of colors repeat throughout the experiment? 
id_codes <- unique(explicit$ID)
question5 <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  q4 <- current$Aware_Q5[1]
  question5 <- c(question5, q4)
}
length(question5[question5 == 'y']) / length(question5) 
length(question5[question5 == 'n']) / length(question5) 


# Outlier removal 
list <- c("Block1_RT", "Block2_RT", "Block3_RT", "Block4_RT", "Block5_RT", "Block6_RT", "Block7_RT", 
          "Block8_RT", "Block9_RT", "Block10_RT", "Block11_RT", "Block12_RT", "Block13_RT", "Block14_RT", "Block15_RT")
length_trials <- NULL
length_outliers <- NULL
for (i in 1:length(list)) {
  block <- list[i]
  length_trials <- c(length_trials, df_edit[ , block])
  outliers <- boxplot(df_edit[ ,block], na.rm = TRUE)$out
  length_outliers <- c(length_outliers, outliers)
  x<- df_edit[-which(df_edit[, block] %in% outliers),]
              
}
length(length_outliers) / length(length_trials)

explicit <- df_edit[df_edit$Condition == "a",]
length(unique(explicit$ID)) 
implicit <- df_edit[df_edit$Condition == "b",]
length(unique(implicit$ID)) 

# t.test comparing overall reaction times
ExplicitRT_all <- NULL
ImplicitRT_all <- NULL
for (i in 1:length(list)) {
  block <- list[i]
  Explicit <- explicit[ , block]
  ExplicitRT_all <- c(ExplicitRT_all, Explicit)
  Implicit <- implicit[ , block]
  ImplicitRT_all <- c(ImplicitRT_all, Implicit)
  
}
t.test(ExplicitRT_all*1000, ImplicitRT_all*1000)
sd(ExplicitRT_all, na.rm = TRUE)
sd(ImplicitRT_all, na.rm = TRUE)

# Figure - graph showing changes in RT over time 
# Medians, like in Brosowsky 
list <- c("Block1_RT", "Block2_RT", "Block3_RT", "Block4_RT", "Block5_RT", "Block6_RT", "Block7_RT", 
          "Block8_RT", "Block9_RT", "Block10_RT", "Block11_RT", "Block12_RT", "Block13_RT", "Block14_RT", "Block15_RT")
ExplicitRT <- NULL
ImplicitRT <- NULL
for (i in 1:length(list)) {
  block <- list[i]
  ExplicitMedian <- median(explicit[ , block], na.rm = TRUE)
  ExplicitRT <- c(ExplicitRT, ExplicitMedian)
  ImplicitMedian <- median(implicit[ , block], na.rm = TRUE)
  ImplicitRT <- c(ImplicitRT, ImplicitMedian)
}
ExplicitRT_Plot <- ExplicitRT*1000
ImplicitRT_Plot <- ImplicitRT*1000

plot(ExplicitRT, type = c('l'), lwd = 3, col = '#FF7547', 
     ylab = "Median Reaction Time (ms)", xlab = "Block", ylim = c(600, 950))
abline(v = 14, col = "lightgrey", lwd = 4)
lines(ImplicitRT, type = 'l', lwd = 3, col = '#0096A3')
lines(ExplicitRT, type = 'l', lwd = 3, col = '#FF7547')
legend(11.5, 900,legend=c("Implicit", "Explicit"),
       col=c("#0096A3", "#FF7547"), lty=1, cex=1, lwd = 3)
axis(1, at = seq(1, 15, by = 2), las=1)

# Get medians for each participant at important Blocks (1, 13, 14, 15)
# Implicit 
id_codes <- unique(implicit$ID)
Block1_Imp <- NULL
Block13_Imp <- NULL
Block14_Imp <- NULL
Block15_Imp <- NULL
Implicit_Transfer <- NULL
Implicit_Practice <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  Block1_Imp <- c(Block1_Imp, median(current$Block1_RT, na.rm = TRUE)*1000)
  Block13_Imp <- c(Block13_Imp, median(current$Block13_RT, na.rm = TRUE)*1000)
  Block14_Imp <- c(Block14_Imp, median(current$Block14_RT, na.rm = TRUE)*1000)
  Block15_Imp <- c(Block15_Imp, median(current$Block15_RT, na.rm = TRUE)*1000)
}
Implicit_Transfer <- Block14_Imp - ((Block13_Imp + Block15_Imp)/2)
Implicit_Practice <- Block13_Imp - Block1_Imp

# Explicit 
id_codes <- unique(explicit$ID)
Block1_Exp <- NULL
Block13_Exp <- NULL
Block14_Exp <- NULL
Block15_Exp <- NULL
Explicit_Transfer <- NULL
Explicit_Practice <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  Block1_Exp <- c(Block1_Exp, median(current$Block1_RT, na.rm = TRUE)*1000)
  Block13_Exp <- c(Block13_Exp, median(current$Block13_RT, na.rm = TRUE)*1000)
  Block14_Exp <- c(Block14_Exp, median(current$Block14_RT, na.rm = TRUE)*1000)
  Block15_Exp <- c(Block15_Exp, median(current$Block15_RT, na.rm = TRUE)*1000)
}
Explicit_Transfer <- Block14_Exp - ((Block13_Exp + Block15_Exp)/2)
Explicit_Practice <- Block13_Exp - Block1_Exp

# Implicit practice effects
t.test(Block1_Exp, Block13_Exp, paired = TRUE)
Implicit_Practice <- data.frame(cbind(Block1_Exp, Block13_Exp))
write.csv(Implicit_Practice, "Implicit_Practice.csv")

# Explicit practice effects
t.test(Block1_Imp, Block13_Imp, paired = TRUE)
Explicit_Practice <- data.frame(cbind(Block1_Exp, Block13_Exp))
write.csv(Explicit_Practice, "Explicit_Practice.csv")

# Practice effects between condition
t.test(Explicit_Practice, Implicit_Practice)
sd(Explicit_Practice)
sd(Implicit_Practice)
practice <- as.data.frame(cbind(Explicit_Practice, Implicit_Practice))
write.csv(practice, "practice.csv")

# Transfer 
# explicit transfer effects
Block13_Block15 <- (Block13_Exp+Block15_Exp)/2
t.test(Block14_Exp, Block13_Block15, paired = TRUE)
Transfer_Explicit_RT <- as.data.frame(cbind(Block14_Exp, Block13_Block15))
write.csv(Transfer_Explicit_RT, "Transfer_Explicit_RT.csv")

# implicit transfer effects
Block13_Block15 <- (Block13_Imp+Block15_Imp)/2
t.test(Block14_Imp, Block13_Block15, paired = TRUE)
Implicit_Explicit_RT <- as.data.frame(cbind(Block14_Imp, Block13_Block15))
write.csv(Implicit_Explicit_RT, "Implicit_Explicit_RT.csv")

# transfer effects between condition
t.test(Explicit_Transfer, Implicit_Transfer)
sd(Explicit_Transfer)
sd(Implicit_Transfer)
transfer <- as.data.frame(cbind(Explicit_Transfer, Implicit_Transfer))
write.csv(transfer, "transfer.csv")

# Bar plot width = 250
# Plot Reaction Time Practice Effects
vector <- c(mean(Implicit_Practice, na.rm = TRUE), mean(Explicit_Practice, na.rm = TRUE))
vector <- vector
barplot <- barplot(vector, beside = T, legend.text=T, col = c("#0096A3","#FF7547"), 
                   ylim = c(-200, 0), ylab = "RT Practice Effects (ms)")
stdev <- vector*1.96 / 10
error.bar(barplot,vector, stdev)

# Plot Reaction Time Transfer Effects
vector <- c(mean(Implicit_Transfer, na.rm = TRUE), mean(Explicit_Transfer, na.rm = TRUE))
vector <- vector
barplot <- barplot(vector, beside = T, legend.text=T, col = c("#0096A3","#FF7547"), 
                   ylim = c(-10, 50), ylab = "RT Transfer Effects (ms)")
stdev <- vector*1.96 / 10
error.bar(barplot,vector, stdev)

# Mindwandering Analysis 
# Comparing the depth of mindwandering between groups
list <- seq(1, 15)
Exp_MW <- NULL
explicit_MW <- explicit$MW_Response[!is.na(explicit$MW_Response)]
explicit_block <- rep(1:15,length(unique(explicit$ID)))
df_exp_MW<- as.data.frame(cbind(explicit_MW, explicit_block))
for (i in 1:length(list)) {
  val <- mean(df_exp_MW$explicit_MW[df_exp_MW$explicit_block == list[i]])
  Exp_MW <- c(Exp_MW, val)
}

Imp_MW <- NULL
implicit_MW <- implicit$MW_Response[!is.na(implicit$MW_Response)]
implicit_block <- rep(1:15,length(unique(implicit$ID)))
df_imp_MW<- as.data.frame(cbind(implicit_MW, implicit_block))
for (i in 1:length(list)) {
  val <- mean(df_imp_MW$implicit_MW[df_imp_MW$implicit_block == list[i]])
  Imp_MW <- c(Imp_MW, val)
}
t.test(implicit_MW, explicit_MW)

# Mindwandering PLot
list <- seq(1, 15)
Exp_MW <- NULL
explicit_MW <- explicit$MW_Response[!is.na(explicit$MW_Response)]
explicit_block <- rep(1:15,length(unique(explicit$ID)))
df_exp_MW<- as.data.frame(cbind(explicit_MW, explicit_block))
for (i in 1:length(list)) {
  val <- mean(df_exp_MW$explicit_MW[df_exp_MW$explicit_block == list[i]])
  Exp_MW <- c(Exp_MW, val)
}

Imp_MW <- NULL
implicit_MW <- implicit$MW_Response[!is.na(implicit$MW_Response)]
implicit_block <- rep(1:15,length(unique(implicit$ID)))
df_imp_MW<- as.data.frame(cbind(implicit_MW, implicit_block))
for (i in 1:length(list)) {
  val <- mean(df_imp_MW$implicit_MW[df_imp_MW$implicit_block == list[i]])
  Imp_MW <- c(Imp_MW, val)
}

plot(Exp_MW, type = c('l'), lwd = 3, col = '#FF7547', 
     ylab = "Depth of Mindwandering", xlab = "Block", ylim = c(25, 65))
abline(v = 14, col = "lightgrey", lwd = 4)

lines(Imp_MW, type = 'l', lwd = sd(Imp_MW)*2, col = '#D6FCFF')
lines(Exp_MW, type = 'l', lwd = sd(Exp_MW)*2, col = '#FFE0D6')
lines(Imp_MW, type = 'l', lwd = 3,col = '#0096A3')
lines(Exp_MW, type = 'l', lwd = 3,col = '#FF7547')
legend(12, 37,legend=c("Implicit", "Explicit"),
       col=c("#0096A3", "#FF7547"), lty=1, cex=1, lwd = 3)
axis(1, at = seq(1, 15, by = 2), las=1)

# Mixed ANOVA with block as within subjects factor and group as between subjects factor 
# Implicit
list <- c("Block1_RT", "Block2_RT", "Block3_RT", "Block4_RT", "Block5_RT", "Block6_RT", "Block7_RT", 
          "Block8_RT", "Block9_RT", "Block10_RT", "Block11_RT", "Block12_RT", "Block13_RT", "Block14_RT", "Block15_RT")
ImplicitRT <- NULL
participant <- unique(implicit$ID)
for (i in 1:length(participant)) { 
  ID <- participant[i]
  current_df <- implicit[implicit$ID == ID,]
  for (i in 1:length(list)) {
    block <- list[i]
    ImplicitMedian <- median(current_df[ , block], na.rm = TRUE)
    ImplicitRT <- c(ImplicitRT, ImplicitMedian)
  }
  ImplicitMedian <- NULL
}
ParticipantID <- as.numeric(rep(unique(implicit$ID), each = 15))
Block <- rep(1:15, length(unique(implicit$ID)))
RT_Averages <- ImplicitRT*1000
Prior_RT <- c(1, head(RT_Averages, -1))
Mindwandering <- as.numeric(implicit$MW_Response[!is.na(implicit$MW_Response)])
MW_Prior  <- c(1, head(Mindwandering, -1))
implicit_data <- as.data.frame(cbind(ParticipantID, Block, Prior_RT, RT_Averages, Mindwandering, MW_Prior))
implicit_data$MW_Prior[implicit_data$Block == 1] <- NA

# then explicit 
participant <- unique(explicit$ID)
ExplicitRT <- NULL
for (i in 1:length(participant)) { 
  ID <- participant[i]
  current_df <- explicit[explicit$ID == ID,]
  
  for (i in 1:length(list)) {
    block <- list[i]
    ExplicitMedian <- median(current_df[ , block], na.rm = TRUE)
    ExplicitRT <- c(ExplicitRT, ExplicitMedian)
    
  }
  ExplicitMedian <- NULL
}

ParticipantID <- as.numeric(rep(unique(explicit$ID), each = 15))
Block <- rep(1:15, length(unique(explicit$ID)))
RT_Averages <- ExplicitRT*1000
Prior_RT <- c(1, head(RT_Averages, -1))
Mindwandering <- as.numeric(explicit$MW_Response[!is.na(explicit$MW_Response)])
MW_Prior  <- c(1, head(Mindwandering, -1))
explicit_data <- as.data.frame(cbind(ParticipantID, Block, Prior_RT, RT_Averages, Mindwandering, MW_Prior))
explicit_data$MW_Prior[explicit_data$Block == 1] <- NA

# Is condition a significant predictor in this model?
data <- as.data.frame(rbind(implicit_data, explicit_data))
Awareness <- c(rep("implicit", nrow(implicit_data)), rep("explicit", nrow(explicit_data)))
data <- as.data.frame(cbind(data, Awareness))
summary(aov(Mindwandering ~ Block*Awareness, data = data))
t.test(explicit_data$RT_Averages, implicit_data$RT_Averages)

# Extra exploratory model 
library(nlme)
data <- data[data$Block != 1, ]  
model <- lme(RT_Averages ~ Mindwandering*MW_Prior*Block*Prior_RT*Awareness, random = ~1 | ParticipantID, data = data)
summary(model)

# Practice Effects for MW
# Implicit 
Block1 <- implicit_data$Mindwandering[implicit_data$Block == 1]
Block13 <- implicit_data$Mindwandering[implicit_data$Block == 13]
t.test(Block13, Block1, paired = TRUE)
Imp_Prac_MW <- Block13 - Block1

# Explicit
Block1 <- explicit_data$Mindwandering[explicit_data$Block == 1]
Block13 <- explicit_data$Mindwandering[explicit_data$Block == 13]
t.test(Block13, Block1, paired = TRUE)
Exp_Prac_MW <- Block13 - Block1

# Between condition
t.test(Imp_Prac_MW, Exp_Prac_MW)
sd(Exp_Prac_MW)
sd(Imp_Prac_MW)

MW_practice <- as.data.frame(cbind(Imp_Prac_MW, Exp_Prac_MW))
write.csv(MW_practice, "MW_practice.csv")

# Plot MW Practice Effects
vector <- c(mean(Imp_Prac_MW, na.rm = TRUE), mean(Exp_Prac_MW, na.rm = TRUE))
# Plot boundaries
barplot <- barplot(vector, beside = T, legend.text=T, col = c("#0096A3","#FF7547"), 
                   ylim = c(0, 50), ylab = "Mindwandering Practice Effects")
stdev <- vector*1.96 / 10
error.bar(barplot,vector, stdev)

# MW Transfer Effects 
# Implicit 
Block13 <- implicit_data$Mindwandering[implicit_data$Block == 13]
Block14 <- implicit_data$Mindwandering[implicit_data$Block == 14]
Block15 <- implicit_data$Mindwandering[implicit_data$Block == 15]
Implicit_Transfer <- Block14 - ((Block13 + Block15)/2)
Block13_Block15 <- (Block13 + Block15)/2
t.test(Block14, Block13_Block15, paired = TRUE)

# Explicit 
Block13 <- explicit_data$Mindwandering[explicit_data$Block == 13]
Block14 <- explicit_data$Mindwandering[explicit_data$Block == 14]
Block15 <- explicit_data$Mindwandering[explicit_data$Block == 15]
Explicit_Transfer <- Block14 - ((Block13 + Block15)/2)
Block13_Block15 <- (Block13 + Block15)/2
t.test(Block14, Block13_Block15, paired = TRUE)
t.test(Explicit_Transfer, Implicit_Transfer)

MW_Transfer <- as.data.frame(cbind(Explicit_Transfer, Implicit_Transfer))
write.csv(MW_Transfer, "MW_transfer.csv")

vector <- c(mean(Implicit_Transfer), mean(Explicit_Transfer))
barplot <- barplot(vector, beside = T, legend.text=T, col = c("#0096A3","#FF7547"), 
                   ylim = c(-10, 0), ylab = "Mindwandering Transfer Effects")
stdev <- vector*1.96 / 10
error.bar(barplot,vector, stdev)

# Interaction Model 
library(nlme)
model <- lme(RT_Averages ~ Mindwandering*Block*Awareness, random = ~1 | ParticipantID, data = data)
summary(model)

# Predicting transfer & practice effects model 
id_codes <- unique(explicit$ID)
transfer <- NULL
practice <- NULL
MW <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- explicit[explicit$ID == id,]
  mean <- mean(current$Block13_RT, na.rm = TRUE)
  trans <- mean(current$Block14_RT, na.rm = TRUE) - mean
  transfer <- c(transfer, trans)
  #
  prac <- mean(current$Block13_RT, na.rm = TRUE) - mean(current$Block1_RT, na.rm = TRUE)
  practice <- c(practice, prac)
  #
  ind_MW <- mean(current$MW_Response, na.rm = TRUE)
  MW <- c(MW, ind_MW)
}
transfer_explicit <- transfer
MW_explicit <- MW
practice_explicit <- practice

id_codes <- unique(implicit$ID)
transfer <- NULL
MW <- NULL
practice <- NULL
for (i in 1:length(id_codes)) {
  id <- id_codes[i]
  current <- implicit[implicit$ID == id,]
  mean <- mean(current$Block13_RT, na.rm = TRUE)
  trans <- mean(current$Block14_RT, na.rm = TRUE) - mean
  transfer <- c(transfer, trans)
  #
  prac <- mean(current$Block13_RT, na.rm = TRUE) - mean(current$Block1_RT, na.rm = TRUE)  
  practice <- c(practice, prac)
  #
  ind_MW <- mean(current$MW_Response, na.rm = TRUE)
  MW <- c(MW, ind_MW)
}
transfer_implicit <- transfer
MW_implicit <- MW
practice_implict <- practice

# transfer effect
explicit_data <- as.data.frame(cbind(unique(explicit_data$ParticipantID), MW_explicit, transfer_explicit))
implicit_data <- as.data.frame(cbind(unique(implicit_data$ParticipantID), MW_implicit, transfer_implicit))

Awareness <- c(rep("explicit", nrow(explicit_data)), rep("implicit", nrow(implicit_data)))
Transfer <- c(transfer_explicit, transfer_implicit)
MW <- c(MW_explicit, MW_implicit)
Practice <- c(practice_explicit, practice_implict)
data2 <- as.data.frame(cbind(Awareness, Transfer, MW, Practice))
#
summary(lm(transfer_explicit*1000 ~ MW_explicit))
summary(lm(transfer_implicit*1000 ~ MW_implicit))

# Predicting Practice Effects 
summary(lm(practice_explicit*1000~MW_explicit))
summary(lm(practice_implict*1000~MW_implicit))

plot(MW_explicit, practice_explicit*1000, pch = 16, col = "#F6B7BB", ylab = "Practice Effect (ms)", 
     xlab = "Depth of Mindwandering", ylim = c(-400, 400), xlim = c(0, 100))
abline(lm(practice_explicit*1000~MW_explicit), col="#E8505B", lwd = 3)

points(MW_implicit, practice_implict*1000, pch = 16, col = "#CFD9DE")
abline(lm(practice_implict*1000~MW_implicit), col="#455A64", lwd = 3)

plot(MW_explicit, transfer_explicit*1000, pch = 16, col = "#F6B7BB", ylab = "Transfer Effect (ms)", 
     xlab = "Depth of Mindwandering", ylim = c(-400, 400), xlim = c(0, 100))
abline(lm(transfer_explicit*1000~MW_explicit), col="#E8505B", lwd = 3)

points(MW_implicit, transfer_implicit*1000, pch = 16, col = "#CFD9DE")
abline(lm(transfer_implicit*1000~MW_implicit), col="#455A64", lwd = 3)

# predict estimates when MW is at 0 and at 100
# model with mindwandering, block, and awareness as the fixed effects and subject as the random effect
lm <- lme(RT_Averages ~ Mindwandering*Block*Awareness, random = ~1 | ParticipantID, data = data)
summary(lm)
library(ggplot2)
library(ggeffects)
dat <- ggpredict(lm, terms = c("Mindwandering [0, 100]", "Awareness", "Block[1:15]"))
dat

# implicit loop 
fac <- c(1:15)
df_implicit <- as.data.frame(cbind(NA, NA))
for (i in 1:length(fac)) {
  block <- fac[i]
  cur <- dat$predicted[dat$group == "implicit" & dat$facet == block]
  df_implicit <- as.data.frame(rbind(df_implicit, cur))
}
df_implicit <- df_implicit[-1,]
colnames(df_implicit) <- c("MW_0", "MW_100")

# explicit loop
fac <- c(1:15)
df_explicit <- as.data.frame(cbind(NA, NA))
for (i in 1:length(fac)) {
  block <- fac[i]
  cur <- dat$predicted[dat$group == "explicit" & dat$facet == block]
  df_explicit <- as.data.frame(rbind(df_explicit, cur))
}
df_explicit <- df_explicit[-1,]
colnames(df_explicit) <- c("MW_0", "MW_100")

# implicit plot 
plot(df_implicit$MW_100, type = c('l'), lwd = 3, col = '#0096A3', main="Implicit Condition", 
     ylab = "Reaction Time (ms)", xlab = "Block", ylim = c(700, 950))
lines(df_implicit$MW_0, type = 'l', lwd = 3,col = '#A3D2E1')
legend(10, 950,legend=c("MW 100", "MW 0"),
       col=c("#0096A3", "#A3D2E1"), lty=1, cex=1, lwd = 3)
axis(1, at = seq(1, 15, by = 2), las=1)

# explicit plot
plot(df_explicit$MW_100, type = c('l'), lwd = 3, col = '#FF7547', main="Explicit Condition", 
     ylab = "Reaction Time (ms)", xlab = "Block", ylim = c(700, 950))
lines(df_implicit$MW_0, type = 'l', lwd = 3,col = '#FFB399')
legend(10, 950,legend=c("MW 100", "MW 0"),
       col=c("#FF7547", "#FFB399"), lty=1, cex=1, lwd = 3)
axis(1, at = seq(1, 15, by = 2), las=1)

summary(lm(implicit_inclusion$acc~MW_implicit))
summary(lm(explicit_inclusion$acc~MW_explicit))

# model <- lme(RT_Averages ~ Mindwandering*Block*Awareness, random = ~1 | ParticipantID, data = data)
write.csv(data, file = "data.csv")
# see JASP 

# Table 1A
list <- c("Block1_RT", "Block2_RT", "Block3_RT", "Block4_RT", "Block5_RT", "Block6_RT", "Block7_RT", 
          "Block8_RT", "Block9_RT", "Block10_RT", "Block11_RT", "Block12_RT", "Block13_RT", "Block14_RT", "Block15_RT")
list_correct <- c("Block1_Correct", "Block2_Correct", "Block3_Correct", 
                  "Block4_Correct", "Block5_Correct", "Block6_Correct", "Block7_Correct", 
          "Block8_Correct", "Block9_Correct", "Block10_Correct", "Block11_Correct", 
          "Block12_Correct", "Block13_Correct", "Block14_Correct", "Block15_Correct")
Implicit_Means <- NULL
Implicit_Correct <- NULL 
for (i in 1:length(list)) {
  block <- list[i]
  Mean <- median(implicit[ , block], na.rm = TRUE)
  Implicit_Means <- c(Implicit_Means, Mean)
  #
  block_correct <- list_correct[i]
  Mean <- mean(implicit[ , block_correct], na.rm = TRUE)
  Implicit_Correct <- c(Implicit_Correct, Mean)
  #
}
MW <- explicit$MW_Response[!is.na(explicit$MW_Response)]
MW_matrix <- matrix(MW, ncol = 15, byrow = TRUE)
MW_means <- colMeans(MW_matrix)
#
explicit_Means <- NULL
explicit_Correct <- NULL 
for (i in 1:length(list)) {
  block <- list[i]
  Mean <- median(explicit[ , block], na.rm = TRUE)
  explicit_Means <- c(explicit_Means, Mean)
  #
  block_correct <- list_correct[i]
  Mean <- mean(explicit[ , block_correct], na.rm = TRUE)
  explicit_Correct <- c(explicit_Correct, Mean)
  #
}
MW <- explicit$MW_Response[!is.na(explicit$MW_Response)]
MW_matrix <- matrix(MW, ncol = 15, byrow = TRUE)
MW_means <- colMeans(MW_matrix)
explicit_Correct*1000

# DF with each participant and median RT for each block 
list <- c("Block1_RT", "Block2_RT", "Block3_RT", "Block4_RT", "Block5_RT", "Block6_RT", "Block7_RT", 
          "Block8_RT", "Block9_RT", "Block10_RT", "Block11_RT", "Block12_RT", "Block13_RT", "Block14_RT", "Block15_RT")
ImplicitRT <- NULL
participant <- unique(implicit$ID)
for (i in 1:length(participant)) { 
  ID <- participant[i]
  current_df <- implicit[implicit$ID == ID,]
  
  for (i in 1:length(list)) {
    block <- list[i]
    ImplicitMedian <- median(current_df[ , block], na.rm = TRUE)
    ImplicitRT <- c(ImplicitRT, ImplicitMedian)
  }
  ImplicitMedian <- NULL
}
ImplicitRT <- ImplicitRT*1000

participant <- unique(explicit$ID)
ExplicitRT <- NULL
for (i in 1:length(participant)) { 
  ID <- participant[i]
  current_df <- explicit[explicit$ID == ID,]
  
  for (i in 1:length(list)) {
    block <- list[i]
    ExplicitMedian <- median(current_df[ , block], na.rm = TRUE)
    ExplicitRT <- c(ExplicitRT, ExplicitMedian)
    }
  ExplicitMedian <- NULL
}
ExplicitRT <- ExplicitRT*1000
#
Explicit_Data <- as.data.frame(cbind(ExplicitRT, rep(1:15, length(ExplicitRT))))
Implicit_Data <- as.data.frame(cbind(ImplicitRT, rep(1:15, length(ImplicitRT))))

# grab standard error
SD_Explicit <- NULL 
for (i in 1:15) {
  SD_Explicit <- c(SD_Explicit, sd(Explicit_Data$ExplicitRT[Explicit_Data$V2 == i]))
}
SD_Implicit <- NULL 
for (i in 1:15) {
  SD_Implicit <- c(SD_Implicit, sd(Implicit_Data$ImplicitRT[Implicit_Data$V2 == i]))
}

# Calculate Standard Error 
#Divide the standard deviation by the square root of the sample size (n). That gives you the “standard error”.
SE_Explicit <- SD_Explicit / sqrt(length(unique(explicit$ID)))
SE_Implicit <- SD_Implicit /  sqrt(length(unique(implicit$ID)))
# plot
plot(x = ExplicitRT_Plot, type = c('l'), lwd = 3, col = '#FF7547', 
     ylab = "Median Reaction Time (ms)", xlab = "Block", ylim = c(625, 950))
abline(v = 14, col = "lightgrey", lwd = 4)
lines(ImplicitRT_Plot, type = 'l', lwd = c(SE_Implicit*2), col = '#CFD9DE')
lines(ExplicitRT_Plot, type = 'l', lwd = c(SE_Explicit*2), col = '#F6B7BB')
lines(ImplicitRT_Plot, type = 'l', lwd = 3, col = '#455A64')
lines(ExplicitRT_Plot, type = 'l', lwd = 3, col = '#E8505B')
legend(10, 925,legend=c("Explicit", "Implicit"),
       col=c("#E8505B", "#455A64"), lty=1, cex=1, lwd = 3)
axis(1, at = seq(1, 15, by = 2), las=1)


# grab standard error
SD_Explicit <- NULL 
for (i in 1:15) {
  SD_Explicit <- c(SD_Explicit, sd(df_exp_MW$explicit_MW[df_exp_MW$explicit_block == i]))
}
SD_Implicit <- NULL 
for (i in 1:15) {
  SD_Implicit <- c(SD_Implicit, sd(df_imp_MW$implicit_MW[df_imp_MW$implicit_block == i]))
}

# Calculate Standard Error 
#Divide the standard deviation by the square root of the sample size (n). That gives you the “standard error”.
SE_Explicit <- SD_Explicit / sqrt(length(unique(explicit$ID)))
SE_Implicit <- SD_Implicit /  sqrt(length(unique(implicit$ID)))

# Final Mindwandering PLots 
plot(Exp_MW, type = c('l'), lwd = 3, col = '#FF7547', 
     ylab = "Depth of Mindwandering", xlab = "Block", ylim = c(25, 65))
abline(v = 14, col = "lightgrey", lwd = 4)
lines(Imp_MW, type = 'l', lwd = c(SE_Implicit*2), col = '#EBFDFF')
lines(Exp_MW, type = 'l', lwd = c(SE_Explicit*2), col = '#FFF0EB')
lines(Imp_MW, type = 'l', lwd = 3,col = '#0096A3')
lines(Exp_MW, type = 'l', lwd = 3,col = '#FF7547')
legend(10, 37,legend=c("Explicit", "Implicit"),
       col=c("#FF7547", "#0096A3"), lty=1, cex=1, lwd = 3)
axis(1, at = seq(1, 15, by = 2), las=1)

# Exploratory Follow up - see JASP :///
# 1 is implicit, 2 is explicit 
Implicit_MW_Prior <- data$MW_Prior[data$Awareness == "implicit"]
Implicit_RT <- data$RT_Averages[data$Awareness == "implicit"]
summary(lm(Implicit_RT ~ Implicit_MW_Prior))
per_im_prior <- as.data.frame(cbind(Implicit_MW_Prior, Implicit_RT))
write.csv(per_im_prior, "per_im_prior.csv")

Explicit_MW_Prior <- data$MW_Prior[data$Awareness == "explicit"]
Explicit_RT <- data$RT_Averages[data$Awareness == "explicit"]
summary(lm(Explicit_RT ~ Explicit_MW_Prior))
per_ex_prior <- as.data.frame(cbind(Explicit_MW_Prior, Explicit_RT))
write.csv(per_ex_prior, "per_ex_prior.csv")

# Just MW
Implicit_MW <- data$Mindwandering[data$Awareness == "implicit"]
Implicit_RT <- data$RT_Averages[data$Awareness == "implicit"]
summary(lm(Implicit_RT ~ Implicit_MW))
per_im <- as.data.frame(cbind(Implicit_MW, Implicit_RT))
write.csv(per_im, "per_im.csv")

Explicit_MW <- data$Mindwandering[data$Awareness == "explicit"]
Explicit_RT <- data$RT_Averages[data$Awareness == "explicit"]
summary(lm(Explicit_RT ~ Explicit_MW))
per_ex <- as.data.frame(cbind(Explicit_MW, Explicit_RT))
write.csv(per_ex, "per_ex.csv")

