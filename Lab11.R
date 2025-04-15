#why is boxplot in between code

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

#extract n argument and get integer value (rounded up)
num.obseravtions <- ceiling(power.analysis$n)

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
df.close <- p.val.close$parameter #get df
g.close <- hedges_g(x = closer.dat, mu = mu0, alternative = "greater") #get g
n.close <- p.val.close$parameter + 1
s.close <- p.val.close$stderr * sqrt(n.close)
#get p-value
p.val.close <- p.val.close$p.value

#part b - conduct t-test for far responses
p.val.far <- t.test(x=further.dat, mu = mu0, alternative = "less")
#get values for parenthesis
conf.int.far <- p.val.far$conf.int #get the confidence interval
conf.far.beg <- conf.int.far[1]
conf.far.end <- conf.int.far[2]
t.far <- p.val.far$statistic #get t 
df.far <- p.val.far$parameter #get df
g.far <- hedges_g(x = further.dat, mu = mu0, alternative = "less") #get g
n.far <- p.val.far$parameter + 1
s.far <- p.val.far$stderr * sqrt(n.far)
#get p-value
p.val.far <- p.val.far$p.value

#part c - conduct t-test for difference
p.val.diff <- t.test(x=diff.dat, mu = mu0, alternative = "two.sided")
#get values for parenthesis
conf.int.diff <- p.val.diff$conf.int #get the confidence interval
conf.diff.beg <- conf.int.diff[1]
conf.diff.end <- conf.int.diff[2]
t.diff <- p.val.diff$statistic #get t 
df.diff <- p.val.diff$parameter #get df
g.diff <- hedges_g(x = diff.dat, mu = mu0, alternative = "two.sided") #get g
n.diff <- p.val.diff$parameter + 1
s.diff <- p.val.diff$stderr * sqrt(n.diff)
#get p-value
p.val.diff <- p.val.diff$p.value

################################################################################
# Task 5: create hypothesis testing plots
################################################################################
#part a - plot for close responses
# For plotting the null distribution
ggdat.t.close <- tibble(t=seq(-10,10,length.out=1000))|>
  mutate(pdf.null = dt(x=t, df=df.close))
# For plotting the observed point
ggdat.obs.close <- tibble(t = t.close, 
                          y = 0) # to plot on x-axis
t.breaks <- c(-5, qt(p = 1-0.05, df = df.close), # rejection region (left)
              0, 5, t.close)                  # t-statistic observed
t.breaks <- sort(unique(round(t.breaks, 2)))
xbar.breaks <- t.breaks * s.close/sqrt(n.close) + mu0
#plot for part a - close responses
close.plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t.close, 
            aes(x=t, y=pdf.null), color = "black")+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t.close, t>=qt(p = 1-0.05, df=df.close)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="gray", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t.close, t>=t.close), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="red", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs.close, aes(x=t, y=y), color="red")+
  theme_bw()+
  ylab("Density")+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ggtitle("T-Test for Mean Dopamine Level of Close Responses for Zebra Finches",
          subtitle=bquote(H[0]==0*";"~H[a]>0))

#part b - plot for far responses
# For plotting the null distribution
ggdat.t.far <- tibble(t=seq(-10,10,length.out=1000))|>
  mutate(pdf.null = dt(x=t, df=df.far))
# For plotting the observed point
ggdat.obs.far <- tibble(t = t.far, 
                          y = 0) # to plot on x-axis
t.breaks <- c(-5, qt(p = 0.05, df = df.far), # rejection region (left)
              0, 5, t.far)                  # t-statistic observed
t.breaks <- sort(unique(round(t.breaks, 2)))
xbar.breaks <- t.breaks * s.far/sqrt(n.far) + mu0
#plot for part a - far responses
far.plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t.far, 
            aes(x=t, y=pdf.null), color = "black")+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t.far, t<=qt(p = 0.05, df=df.far)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="gray", alpha=0.5)+
  # plot observation point
  geom_point(data=ggdat.obs.far, aes(x=t, y=y), color="red")+
  theme_bw()+
  ylab("Density")+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ggtitle("T-Test for Mean Dopamine Level of Far Responses for Zebra Finches",
          subtitle=bquote(H[0]==0*";"~H[a]<0))

#part c - plot for difference in responses
# For plotting the null distribution
ggdat.t.diff <- tibble(t=seq(-10,10,length.out=1000))|>
  mutate(pdf.null = dt(x=t, df=df.diff))
# For plotting the observed point
ggdat.obs.diff <- tibble(t = t.diff, 
                        y = 0) # to plot on x-axis
t.breaks <- c(-5, qt(p = 0.025, df = df.diff), # rejection region (left)
              0, qt(p = 1-0.025, df = df.diff), 5, t.diff)                  # t-statistic observed
t.breaks <- sort(unique(round(t.breaks, 2)))
xbar.breaks <- t.breaks * s.diff/sqrt(n.diff) + mu0
#plot for part a - diff responses
diff.plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t.diff, 
            aes(x=t, y=pdf.null), color = "black")+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t.diff, t<=qt(p = 0.025, df=df.diff)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="gray", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t.diff, t>=qt(0.975, df=df.diff)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot observation point
  geom_point(data=ggdat.obs.diff, aes(x=t, y=y), color="red")+
  theme_bw()+
  ylab("Density")+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ggtitle("T-Test for Mean Dopamine Level of Difference in Responses for Zebra Finches",
          subtitle=bquote(H[0]==0*";"~H[a] != 0))
