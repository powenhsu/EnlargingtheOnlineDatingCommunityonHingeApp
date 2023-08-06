#No Effect

#Hypothesis 1: Advertisement Placement (Story vs Post)

library(rmarkdown)
library(data.table)
library(DT)
library(dplyr)
set.seed(seed = 329)

#Testing Dataset

n <- 5000
bp.dat <- data.table(Group = c(rep.int(x="Video_Story", times = n/2), 
                               rep.int(x = "Video_Post", times = n/2)))

bp.dat[Group == "Video_Story", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
bp.dat[Group == "Video_Post", click := (x = rbinom(n = 2500, size =1, prob=0.15))]

# data=datatable(data = bp.dat)
data = bp.dat %>%
  group_by(Group) %>%
  summarise(count0 = sum(click == 0), 
            count1 = sum(click ==1))

#Analyze Function

analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- prop.test(x = c(data$count1[1], data$count1[2]),
                        n = c(2500, 2500))
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment(the.dat = data)

##Repeating 1,000 scenarios

B <- 1000
n <- 5000
set.seed(seed = 4172)
Experiment <- 1:B

#Create dataset

Group <- c(rep.int(x="Video_Story", times = n/2), 
             rep.int(x = "Video_Post", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))

#Construct p.test
p.test <- data.frame(effect = rep(NA, 1000), upper_ci = rep(NA,1000), lower_ci = rep(NA, 1000), p = rep(NA, 1000))

#Simulation

for (i in 1:1000) {
  bp.dat <- data.table(Group <- c(rep.int(x="Video_Story", times = n/2), 
                                    rep.int(x = "Video_Post", times = n/2)))
  bp.dat[Group == "Video_Story", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  bp.dat[Group == "Video_Post", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  data = bp.dat %>%
    group_by(V1) %>%
    summarise(count0 = sum(click == 0), 
              count1 = sum(click == 1))
  
  p.test[i, ] <- analyze.experiment(the.dat = data)
}

exp.results <- cbind(sim.dat, p.test)

DT::datatable(data = round(x = exp.results[1:1000, -2],
                           digits = 3), rownames = F)
# Percentage of False positive
exp.results[, mean(p < 0.05)]

# Percentage of True negative
1 - exp.results[, mean(p < 0.05)]

# Summary of P-Value
exp.results[, summary(p)]

# Summary of difference for prop-test
exp.results[, summary(effect)]

# Mean effect of the simulated data
exp.results[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results[, summary(lower_ci)]


#Hypothesis 2: Advertisement Format (Video vs Static Image)

##Repeating 1,000 scenarios

B <- 1000
n <- 5000
set.seed(seed = 4172)
Experiment <- 1:B

#Create dataset

Group2 <- c(rep.int(x="Video_Post", times = n/2), 
           rep.int(x = "Static_Post_Tagline", times = n/2))

sim.dat2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group2))

#Construct p.test
p.test <- data.frame(effect = rep(NA, 1000), upper_ci = rep(NA,1000), lower_ci = rep(NA, 1000), p = rep(NA, 1000))

#Simulation

for (i in 1:1000) {
  bp.dat2 <- data.table(Group <- c(rep.int(x="Video_Post", times = n/2), 
                                  rep.int(x = "Static_Post_Tagline", times = n/2)))
  bp.dat2[Group == "Video_Post", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  bp.dat2[Group == "Static_Post_Tagline", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  data2 = bp.dat2 %>%
    group_by(V1) %>%
    summarise(count0 = sum(click == 0), 
              count1 = sum(click == 1))
  
  p.test[i, ] <- analyze.experiment2(the.dat = data2)
}

exp.results2 <- cbind(sim.dat2, p.test)

DT::datatable(data = round(x = exp.results2[1:1000, -2],
                           digits = 3), rownames = F)

#Analyze Function

analyze.experiment2 <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- prop.test(x = c(data2$count1[1], data2$count1[2]),
                        n = c(2500, 2500))
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment2(the.dat = data2)

# Percentage of False positive
exp.results2[, mean(p < 0.05)]

# Percentage of True negative
1 - exp.results2[, mean(p < 0.05)]

# Summary of P-Value
exp.results2[, summary(p)]

# Summary of difference for prop-test
exp.results2[, summary(effect)]

# Mean effect of the simulated data
exp.results2[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results2[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results2[, summary(lower_ci)]


#Hypothesis 3: Advertisement Type (Brand vs Benefit)

##Repeating 1,000 scenarios

B <- 1000
n <- 5000
set.seed(seed = 4172)
Experiment <- 1:B

#Create dataset

Group3 <- c(rep.int(x="Static_Post_Tagline", times = n/2), 
            rep.int(x = "Static_Post_Benefit", times = n/2))

sim.dat3 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group3))

#Construct p.test
p.test <- data.frame(effect = rep(NA, 1000), upper_ci = rep(NA,1000), lower_ci = rep(NA, 1000), p = rep(NA, 1000))

#Analyze Function

analyze.experiment3 <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- prop.test(x = c(data3$count1[1], data3$count1[2]),
                        n = c(2500, 2500))
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment3(the.dat = data3)

#Simulation

for (i in 1:1000) {
  bp.dat3 <- data.table(Group <- c(rep.int(x="Static_Post_Tagline", times = n/2), 
                                   rep.int(x = "Static_Post_Benefit", times = n/2)))
  bp.dat3[Group == "Static_Post_Tagline", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  bp.dat3[Group == "Static_Post_Benefit", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  data3 = bp.dat3 %>%
    group_by(V1) %>%
    summarise(count0 = sum(click == 0), 
              count1 = sum(click == 1))
  
  p.test[i, ] <- analyze.experiment3(the.dat = data3)
}

exp.results3 <- cbind(sim.dat3, p.test)

DT::datatable(data = round(x = exp.results3[1:1000, -2],
                           digits = 3), rownames = F)


# Percentage of False positive
exp.results3[, mean(p < 0.05)]

# Percentage of True negative
1 - exp.results3[, mean(p < 0.05)]

# Summary of P-Value
exp.results3[, summary(p)]

# Summary of difference for prop-test
exp.results3[, summary(effect)]

# Mean effect of the simulated data
exp.results3[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results3[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results3[, summary(lower_ci)]


#With Effect

#Hypothesis 1: Advertisement Placement (Story vs Post)

##Repeating 1,000 scenarios

B <- 1000
n <- 5000
set.seed(seed = 1031)
Experiment <- 1:B

#Create dataset

Group <- c(rep.int(x="Video_Post", times = n/2), 
           rep.int(x = "Video_Story", times = n/2))

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))

#Construct p.test
p.test <- data.frame(effect = rep(NA, 1000), upper_ci = rep(NA,1000), lower_ci = rep(NA, 1000), p = rep(NA, 1000))

#Analyze Function

analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- prop.test(x = c(data$count1[1], data$count1[2]),
                        n = c(2500, 2500))
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment(the.dat = data)

#Simulation

for (i in 1:1000) {
  bp.dat <- data.table(Group <- c(rep.int(x="Video_Post", times = n/2), 
                                  rep.int(x = "Video_Story", times = n/2)))
  bp.dat[Group == "Video_Story", click := (x = rbinom(n = 2500, size =1, prob=0.15))]
  bp.dat[Group == "Video_Post", click := (x = rbinom(n = 2500, size =1, prob=0.20))]
  data = bp.dat %>%
    group_by(V1) %>%
    summarise(count0 = sum(click == 0), 
              count1 = sum(click == 1))
  
  p.test[i, ] <- analyze.experiment(the.dat = data)
}

exp.results <- cbind(sim.dat, p.test)

DT::datatable(data = round(x = exp.results[1:1000, -2],
                           digits = 3), rownames = F)
# Percentage of True positive
exp.results[, mean(p < 0.05)]

# Percentage of False negative
1 - exp.results[, mean(p < 0.05)]

# Summary of P-Value
exp.results[, summary(p)]

# Summary of difference for prop-test
exp.results[, summary(effect)]

# Mean effect of the simulated data
exp.results[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results[, summary(lower_ci)]

#Hypothesis 2: Advertisement Format (Video vs Static Image)

##Repeating 1,000 scenarios

B <- 1000
n <- 5000
set.seed(seed = 4172)
Experiment <- 1:B

#Create dataset

Group2 <- c(rep.int(x="Video_Post", times = n/2), 
            rep.int(x = "Static_Post_Tagline", times = n/2))

sim.dat2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group2))

#Construct p.test
p.test <- data.frame(effect = rep(NA, 1000), upper_ci = rep(NA,1000), lower_ci = rep(NA, 1000), p = rep(NA, 1000))

#Analyze Function

analyze.experiment2 <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- prop.test(x = c(data2$count1[1], data2$count1[2]),
                        n = c(2500, 2500))
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment2(the.dat = data2)

#Simulation

for (i in 1:1000) {
  bp.dat2 <- data.table(Group <- c(rep.int(x="Video_Post", times = n/2), 
                                   rep.int(x = "Static_Post_Tagline", times = n/2)))
  bp.dat2[Group == "Video_Post", click := (x = rbinom(n = 2500, size =1, prob=0.20))]
  bp.dat2[Group == "Static_Post_Tagline", click := (x = rbinom(n = 2500, size =1, prob=0.25))]
  data2 = bp.dat2 %>%
    group_by(V1) %>%
    summarise(count0 = sum(click == 0), 
              count1 = sum(click == 1))
  
  p.test[i, ] <- analyze.experiment2(the.dat = data2)
}

exp.results2 <- cbind(sim.dat2, p.test)

DT::datatable(data = round(x = exp.results2[1:1000, -2],
                           digits = 3), rownames = F)
# Percentage of True positive
exp.results2[, mean(p < 0.05)]

# Percentage of False negative
1 - exp.results2[, mean(p < 0.05)]

# Summary of P-Value
exp.results2[, summary(p)]

# Summary of difference for prop-test
exp.results2[, summary(effect)]

# Mean effect of the simulated data
exp.results2[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results2[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results2[, summary(lower_ci)]

#Hypothesis 3: Advertisement Type (Brand vs Benefit)

##Repeating 1,000 scenarios

B <- 1000
n <- 5000
set.seed(seed = 4172)
Experiment <- 1:B

#Create dataset

Group3 <- c(rep.int(x="Static_Post_Tagline", times = n/2), 
            rep.int(x = "Static_Post_Benefit", times = n/2))

sim.dat3 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group3))

#Construct p.test
p.test <- data.frame(effect = rep(NA, 1000), upper_ci = rep(NA,1000), lower_ci = rep(NA, 1000), p = rep(NA, 1000))

#Analyze Function

analyze.experiment3 <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- prop.test(x = c(data3$count1[1], data3$count1[2]),
                        n = c(2500, 2500))
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment3(the.dat = data3)

#Simulation

for (i in 1:1000) {
  bp.dat3 <- data.table(Group <- c(rep.int(x="Static_Post_Tagline", times = n/2), 
                                   rep.int(x = "Static_Post_Benefit", times = n/2)))
  bp.dat3[Group == "Static_Post_Tagline", click := (x = rbinom(n = 2500, size =1, prob=0.25))]
  bp.dat3[Group == "Static_Post_Benefit", click := (x = rbinom(n = 2500, size =1, prob=0.30))]
  data3 = bp.dat3 %>%
    group_by(V1) %>%
    summarise(count0 = sum(click == 0), 
              count1 = sum(click == 1))
  
  p.test[i, ] <- analyze.experiment3(the.dat = data3)
}

exp.results3 <- cbind(sim.dat3, p.test)

DT::datatable(data = round(x = exp.results3[1:1000, -2],
                           digits = 3), rownames = F)
# Percentage of True positive
exp.results3[, mean(p < 0.05)]

# Percentage of False negative
1 - exp.results3[, mean(p < 0.05)]

# Summary of P-Value
exp.results3[, summary(p)]

# Summary of difference for prop-test
exp.results3[, summary(effect)]

# Mean effect of the simulated data
exp.results3[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results3[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results3[, summary(lower_ci)]


