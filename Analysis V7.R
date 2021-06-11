## Analysis Version 4

# Loading dataset 
rm(list=ls())
raw.df <- read.csv("data.csv")

# Required packages
library("dplyr")
library("simr")
library("lme4")
library("nlme")

#### Data Cleaning #### 
# Shifting dataset so that all MW probes are maintained in the dataset even after filtering for block 1-13
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
raw.df$probe_tut_response <- shift(raw.df$probe_tut_response, 1)

# Filter so we only have blocks 2-13 a
list_of_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)                 
raw.df <- filter(raw.df, srt_block %in% list_of_values)

# Filtering to remove items unrelated to the current research question
# The authors included a lot of extra measures + tasks that we will not be using 
list <- unique(raw.df$ss_code)
new.df <- as.data.frame(raw.df[1,])
for (i in 1:length(list)) {
  code <- list[i]
  a <- raw.df[raw.df$ss_code == code,]
  a <- a[1:1040, ]             
  new.df <- rbind(new.df, a)
}
df <- new.df[-1,]


# CLEANING
# Identify subjects with greater than 15% errors
bad.subs <- df %>%
  filter(trial_type == "srt-normal" | trial_type == "srt-reversed",
         srt_accuracy == "0") %>%
  group_by(ss_code, trial) %>%
  mutate(srt_error = 0) %>%
  select(ss_code, trial, srt_error) %>% 
  distinct() %>%
  summarize(n = n()) %>%
  group_by(ss_code) %>%
  summarize(n = n () / 1200) %>%
  filter(n > .15) %>%
  .$ss_code
# Takes a bit to run

# Remove bad subjects 
df <- df %>% filter(ss_code %in% bad.subs == FALSE)

# Grab MW values before removing/cleaning learning trials (we risk deleting rows that contain MW probes)
MindwanderingValues <- as.numeric(na.omit(df$probe_tut_response))

# Remove first trial from every block cause it is messsyy + the original authors did this
df <- df %>% filter(srt_trial %in% c(0) == FALSE)

# Remove reaction times greater than 3 seconds
df <- df %>% filter(srt_response_time < 3000 | is.na(srt_response_time))

# Tag RTs as outliers using van selst and jolicour aka original author's method 
xsize <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 35, 50, 80)
stds <- c(1.3,1.458,1.68,1.841,1.961,2.05,2.12,2.173,2.22,2.246,2.274,2.31,2.326,2.391,2.41,2.4305,2.45,2.48,2.5)

minmax <- df %>%
  filter(srt_accuracy == 1) %>%
  mutate(srt_response_time = as.numeric(as.character(srt_response_time)),
         srt_block = as.numeric(as.character(srt_block))) %>%
  group_by(ss_code, srt_block) %>%
  summarize (
    Ntrials = n(),
    cutoff = stds[length(xsize[xsize <= length(srt_response_time)])],
    min = mean(srt_response_time) - sd(srt_response_time) * stds[length(xsize[xsize <= length(srt_response_time)])],
    max = mean(srt_response_time) + sd(srt_response_time) * stds[length(xsize[xsize <= length(srt_response_time)])]
  )

test.df <- left_join(df, minmax, by = c("ss_code", "srt_block"))
test.df$outlier <- test.df$srt_response_time < test.df$min | test.df$srt_response_time > test.df$max

check <- df %>%
  filter(srt_accuracy == 1) %>%
  mutate(srt_response_time = as.numeric(as.character(srt_response_time)),
         srt_block = as.numeric(as.character(srt_block)))
percent_removed = (nrow(df %>% filter(srt_accuracy == 1)) - 
                     nrow(test.df %>% filter(srt_accuracy == 1, outlier == FALSE))) /  
  nrow(df %>% filter(srt_accuracy == 1))

# Removing rows where srt_accuracy = 0 (like the original authors)
df <- df[df$srt_accuracy != 0, ] 

###### Getting values for actual datafram
# Get RT for each block.
final <- NULL
rt_averages <- NULL
block <- 1:13
blocks <- NULL
participant_reference <- unique(df$ss_code)
for (i in 1:length(participant_reference)) {
  code <- participant_reference[i]
  current <- df[df$ss_code == code,]
    for (i in 1:length(block)) {
      mean <- median(current$srt_response_time[current$srt_block == i])
      rt_averages <- c(rt_averages, mean)
      blocks <- c(blocks, current$srt_block == i)
    }
  
  }

# Putting everything into a dataset 
ParticipantID <- rep(1:length(unique(df$ss_code)), each = 13)
Block <- rep(1:13, length(unique(df$ss_code)))
Mindwandering <- MindwanderingValues
MW_Prior <- c(NA, head(MindwanderingValues, -1)) # so that mw at end of block 1 matches with learning rate for block 2
RT_Averages <- rt_averages
Prior_RT <- c(NA, head(RT_Averages, -1))
length(unique(df$ss_code[df$exp == "explicit"]))
length(unique(df$ss_code[df$exp == "implicit"]))
a1 <- rep(1, 91*13)
a2 <- rep(2, 99*13)
Condition <- c(a1, a2)
df2 <- as.data.frame(cbind(ParticipantID, Block, Mindwandering, MW_Prior, RT_Averages, Prior_RT, Condition))
df2 <- df2[df2$Block != 1,]

model <- lme(RT_Averages ~ Mindwandering*MW_Prior*Block*Prior_RT*Condition, random = ~1 | ParticipantID, data = df2)
summary(model)


# 1 is implicit, 2 is explicit 
Implicit_MW_Prior <- df2$MW_Prior[df2$Condition == 1]
Implicit_RT <- df2$RT_Averages[df2$Condition == 1]
summary(lm(Implicit_RT ~ Implicit_MW_Prior))
motor_im_prior <- as.data.frame(cbind(Implicit_MW_Prior, Implicit_RT))
write.csv(motor_im_prior, "motor_im_prior.csv")

Explicit_MW_Prior <- df2$MW_Prior[df2$Condition == 2]
Explicit_RT <- df2$RT_Averages[df2$Condition == 2]
summary(lm(Explicit_RT ~ Explicit_MW_Prior))
motor_ex_prior <- as.data.frame(cbind(Explicit_MW_Prior, Explicit_RT))
write.csv(motor_ex_prior, "motor_ex_prior.csv")

# Just MW
Implicit_MW <- df2$Mindwandering[df2$Condition == 1]
Implicit_RT <- df2$RT_Averages[df2$Condition == 1]
summary(lm(Implicit_RT ~ Implicit_MW))
motor_ex <- as.data.frame(cbind(Implicit_MW, Implicit_RT))
write.csv(motor_ex, "motor_ex.csv")

Explicit_MW <- df2$Mindwandering[df2$Condition == 2]
Explicit_RT <- df2$RT_Averages[df2$Condition == 2]
summary(lm(Explicit_RT ~ Explicit_MW))
motor_im <- as.data.frame(cbind(Explicit_MW, Explicit_RT))
write.csv(motor_im, "motor_im.csv")



# course
u <- t(matrix(c(-3,4,3)))
v <- matrix(c(3,1,5))

x <- c(7921, 5184, 8836, 4761)
sd(x)
5184-6675.5
-1491.5/2009.056

x <- c(89,72,94,69)
sd(x)

8836-4761
-1491.5/4075





