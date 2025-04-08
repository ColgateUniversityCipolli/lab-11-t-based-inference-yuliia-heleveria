#make sure it runs from scratch

################################################################################
# LAB 11 R CODE
# YULIIA HELEVERIA
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# Load libraries
################################################################################
library(pwr)
library(tidyverse)
library(effectsize)

################################################################################
# Task 1: using pwr package for R
################################################################################
#conduct power analysis
d <- 0.65 #effect size
type <- "one.sample" 
alternative <- "two.sided" 
alpha <- 0.05
power <- 0.80
power.analysis <- pwr.t.test(n = NULL, d=d, sig.level = alpha, power = power,
                             type = type, alternative = alternative)

#extract n argument
num.obseravtions <- power.analysis$n

################################################################################
# Task 2: collecting data for Figure 2
################################################################################
#load the file with deleted sheets
fig.data <- read_csv("Fig2Dat.csv")
#mutate data to get the difference
fig.data <- fig.data|>
  mutate("difference" = closer_vals - further_vals)

################################################################################
# Task 3: summarize the data
################################################################################
#part a - summarize further data
further.dat <- fig.data$further_vals

#do numerical summary for the data
further.summary <- tibble(
  mean = mean(further.dat),
  sd = sd(further.dat),
  min = min(further.dat),
  q1 = quantile(further.dat, probs = 0.25),
  median = quantile(further.dat, probs = 0.50),
  q3 = quantile(further.dat, probs = 0.75),
  max = max(further.dat)
)

#do graphical summary for the data
furthest.boxplot <- ggplot(data = tibble(further.dat))+
  geom_boxplot(aes(x = "", y = further.dat*100),
               fill = "lightblue")+ #make the boxplot for the data
  theme_bw()+
  xlab("Further Data")+
  ylab("Dopanime percentage change")


#part b - summarize closer data
closer.dat <- fig.data$closer_vals

#do numerical summary for the data
closer.summary <- tibble(
  mean = mean(closer.dat),
  sd = sd(closer.dat),
  min = min(closer.dat),
  q1 = quantile(closer.dat, probs = 0.25),
  median = quantile(closer.dat, probs = 0.50),
  q3 = quantile(closer.dat, probs = 0.75),
  max = max(closer.dat)
)

#do graphical summary for the data
closer.boxplot <- ggplot(data = tibble(closer.dat))+
  geom_boxplot(aes(x = "", y = closer.dat*100),
               fill = "lightblue")+ #make the boxplot for the data
  theme_bw()+
  xlab("Closer Data")+
  ylab("Dopanime percentage change")

#part c - summarize paired difference
diff.dat <- fig.data$difference

#do numerical summary for the data
diff.summary <- tibble(
  mean = mean(diff.dat),
  sd = sd(diff.dat),
  min = min(diff.dat),
  q1 = quantile(diff.dat, probs = 0.25),
  median = quantile(diff.dat, probs = 0.50),
  q3 = quantile(diff.dat, probs = 0.75),
  max = max(diff.dat)
)

#do graphical summary for the data
diff.boxplot <- ggplot(data = tibble(diff.dat))+
  geom_boxplot(aes(x = "", y = diff.dat*100),
               fill = "lightblue")+ #make the boxplot for the data
  theme_bw()+
  xlab("Paired difference")+
  ylab("Dopanime percentage change")

################################################################################
# Task 4: conduct the inferences
################################################################################
conf.level = 0.95
mu0 <- 0
#part a - conduct t-test for close responses
p.val.close <- t.test(x=closer.dat, mu = mu0, alternative = "greater")
#get values for parenthesis
conf.int.close <- p.val.close$conf.int #get the confidence interval
conf.close.beg <- conf.int.close[1]
conf.close.end <- conf.int.close[2]
t.close <- p.val.close$statistic #get t 
g.close <- hedges_g(x = closer.dat, mu = mu0, alternative = "greater") #get g
#get p-value
p.val.close <- p.val.close$p.value

#part b - conduct t-test for far responses
p.val.far <- t.test(x=further.dat, mu = mu0, alternative = "less")
#get values for parenthesis
conf.int.far <- p.val.far$conf.int #get the confidence interval
conf.far.beg <- conf.int.far[1]
conf.far.end <- conf.int.far[2]
t.far <- p.val.far$statistic #get t 
g.far <- hedges_g(x = further.dat, mu = mu0, alternative = "less") #get g
#get p-value
p.val.far <- p.val.far$p.value

#part c - conduct t-test for difference
p.val.diff <- t.test(x=diff.dat, mu = mu0, alternative = "two.sided")
#get values for parenthesis
conf.int.diff <- p.val.diff$conf.int #get the confidence interval
conf.diff.beg <- conf.int.diff[1]
conf.diff.end <- conf.int.diff[2]
t.diff <- p.val.diff$statistic #get t 
g.diff <- hedges_g(x = diff.dat, mu = mu0, alternative = "two.sided") #get g
#get p-value
p.val.diff <- p.val.diff$p.value

################################################################################
# Task 5: create hypothesis testing plots
################################################################################
#plot for part a - close responses



