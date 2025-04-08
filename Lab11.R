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
