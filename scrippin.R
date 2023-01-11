# packages 

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(patchwork)
library(hrbrthemes)

# data set

THECAPDATA <- read.csv("~/documents/THECAPDATA.csv", header=TRUE)

##summary stats

ii <- mean(na.omit(THECAPDATA$Enrollment.2008))
it <- sd(na.omit(THECAPDATA$Enrollment.2008))
ie <- median(na.omit(THECAPDATA$Enrollment.2008))

ti <- mean(na.omit(THECAPDATA$Apps.2008))
tt <- sd(na.omit(THECAPDATA$Apps.2008))
te <- median(na.omit(THECAPDATA$Apps.2008))

ei <- mean(na.omit(THECAPDATA$ACT.2008))
et <- sd(na.omit(THECAPDATA$ACT.2008))
ee <- median(na.omit(THECAPDATA$ACT.2008))

THECAPDATA$

qi <- mean(na.omit(THECAPDATA$Netprice.2008))
qt <- sd(na.omit(THECAPDATA$Netprice.2008))
qe <- median(na.omit(THECAPDATA$Netprice.2008))

quints <- quantile(na.omit(THECAPDATA$ACT.2008))

sumstats <- data.frame(stats = c("mean","Sd","Median"),
                       Applications = c(ti,tt,te),
                       enrollment = c(ii, it, ie),
                       Avg.ACT = c(ei,et,ee),
                       Net.Price = c(qi,qt,qe))

sumstats

## subset by ACT score in 2008 

bottom75 <- subset(THECAPDATA, ACT.2008 <= 27.9999)
topper25 <- subset(THECAPDATA, ACT.2008 >= 28)

# panel conversion --------------------------------------------------------

## panel data conversion

full <- THECAPDATA %>% 
  pivot_longer(-School.01,
               names_to = c(".value","year"),
               names_pattern = "([A-Za-z]+).(\\d+)")

full$exwin <- as.numeric(recode(full$Seed,
                     '0'='0',
                     '1'='4',
                     '2'='3',
                     '3'='2',
                     '4'='2',
                     '5'='1',
                     '6'='1',
                     '7'='1',
                     '8'='1',
                     '9'='0',
                     '10'='0',
                     '11'='0',
                     '12'='0',
                     '13'='0',
                     '14'='0',
                     '15'='0',
                     '16'='0'))

bot75 <- bottom75 %>% 
  pivot_longer(-School.01,
               names_to = c(".value","year"),
               names_pattern = "([A-Za-z]+).(\\d+)")

bot75$exwin <- as.numeric(recode(bot75$Seed,
                                '0'='0',
                                '1'='4',
                                '2'='3',
                                '3'='2',
                                '4'='2',
                                '5'='1',
                                '6'='1',
                                '7'='1',
                                '8'='1',
                                '9'='0',
                                '10'='0',
                                '11'='0',
                                '12'='0',
                                '13'='0',
                                '14'='0',
                                '15'='0',
                                '16'='0'))

top25 <- topper25 %>% 
  pivot_longer(-School.01,
               names_to = c(".value","year"),
               names_pattern = "([A-Za-z]+).(\\d+)")

top25$exwin <- as.numeric(recode(top25$Seed,
                                '0'='0',
                                '1'='4',
                                '2'='3',
                                '3'='2',
                                '4'='2',
                                '5'='1',
                                '6'='1',
                                '7'='1',
                                '8'='1',
                                '9'='0',
                                '10'='0',
                                '11'='0',
                                '12'='0',
                                '13'='0',
                                '14'='0',
                                '15'='0',
                                '16'='0'))
################

# adding success metric 

full$succ <- ifelse(full$Seed > 0,1+((full$Wins-full$exwin)/(full$exwin+1)),0)

full$performance <- full$Wins-full$exwin

## lag t_1

full$seedt_1=c(NA,full$Seed[-length(full$Seed)])

full$succt_1=c(NA,full$succ[-length(full$succ)])

full$performancet_1=c(NA,full$performance[-length(full$performance)])

#### team-years that made the big_dance 

dancers <- subset(full, Seed >= 1)

#### selective and non-selective schools 

top <- subset(dancers, ACT >= 32)
bot <- subset(dancers, ACT <= 31.99)

## OLS 

ggplot(dancers,aes(performancet_1,Percdap))+
  geom_point()

ols <- lm(Percdap ~ performancet_1, data = dancers)
summary(ols)

## fixed effect models V2

### all NCAA tournament players

ffmodel1 <- lm(Percdap ~ performancet_1 + 
                 factor(School.01) + factor(year), data = dancers)

summary(ffmodel1)

ffmodel2 <- lm(Percdap ~ performancet_1 + Netprice + Seed + Enrollment + 
                factor(School.01) + factor(year), data = dancers)

summary(ffmodel2)

### selective

ffmodel3 <- lm(Percdap ~ performancet_1 + 
                 factor(School.01) + factor(year), data = top)

summary(ffmodel3)

ffmodel4 <- lm(Percdap ~ performancet_1 + Netprice + Seed + Enrollment + 
                 factor(School.01) + factor(year), data = top)

summary(ffmodel4)

### NON-SELECTIVE 

ffmodel5 <- lm(Percdap ~ performancet_1 + 
                 factor(School.01) + factor(year), data = bot)

summary(ffmodel5)

ffmodel6 <- lm(Percdap ~ performancet_1 + Netprice + Seed + Enrollment + 
                 factor(School.01) + factor(year), data = bot)

summary(ffmodel6)

### dummy for making the tournament 

full$berth <- ifelse(full$Seed == 0,0,1)

full$bertht_1=c(NA,full$berth[-length(full$berth)])

full$exwint_1=c(NA,full$exwin[-length(full$exwin)])

full32 <- subset(full,ACT <= 31.99)

dumpy <- lm(Percdap ~ bertht_1 + factor(School.01)+ factor(year),data = full32)
summary(dumpy)


#REGRESSION RESULTS TABLE. 

library(stargazer)

stargazer(ffmodel1,ffmodel2,ffmodel5,ffmodel6,
          keep = c("performancet_1","Seed","Netprice","Enrollment"),
          column.labels = c("Main Regression","Add. Controls","ACT < 32","Add. Controls"),
          dep.var.labels = ("Percent Change in Application"),
          type = "text")

stargazer(dumpy,
          keep = c("bertht_1"),
          dep.var.labels = ("Percent Change in Application"),
          type = "text")

# Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.3. https://CRAN.R-project.org/package=stargaze
