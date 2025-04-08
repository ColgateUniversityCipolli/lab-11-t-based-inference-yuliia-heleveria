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
furthest.summary <- tibble(
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


