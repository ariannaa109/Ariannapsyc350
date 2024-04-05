#Arianna Aguilar
#Psyc 350 Spring 2024
#Data Analysis Script


#creating a fake dataset 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
install.packages(lme4)
library(lme4)
install.packages("sjplots")

#creating a data set with 30 IDs and their age in months, each assigned to group 1 or 2
practice_data <- data.frame(ID = 1:30, Age = sample (36:60, 30, replace = TRUE), Group = sample(1:2, 30, replace = TRUE))

as.numeric(practice_data$ID:practice_data$Group)

#creating columns for each rae tq
practice_data$rae1_tq1 <-sample(0:1, 30, replace = TRUE)
practice_data$rae1_tq2 <-sample(0:1, 30, replace = TRUE)
practice_data$rae2_tq1 <-sample(0:1, 30, replace = TRUE)
practice_data$rae2_tq2 <-sample(0:1, 30, replace = TRUE)
practice_data$rae3_tq1 <-sample(0:1, 30, replace = TRUE)
practice_data$rae3_tq2 <-sample(0:1, 30, replace = TRUE)
practice_data$rae4_tq1 <-sample(0:1, 30, replace = TRUE)
practice_data$rae4_tq2 <-sample(0:1, 30, replace = TRUE)


#creating a total rae score 
practice_data <- practice_data %>% mutate(total_rae = rae1_tq1 + rae1_tq2 
+ rae2_tq1 + rae2_tq2 + rae3_tq1 + rae3_tq2 + rae4_tq1 + rae4_tq2)


#creating columns for each cafe face
practice_data$f1 <- sample(0:1, 30, replace = TRUE)
practice_data$f2 <- sample(0:1, 30, replace = TRUE)
practice_data$f3 <- sample(0:1, 30, replace = TRUE)
practice_data$f4 <- sample(0:1, 30, replace = TRUE)
practice_data$f5 <- sample(0:1, 30, replace = TRUE)
practice_data$f6 <- sample(0:1, 30, replace = TRUE)
practice_data$f7 <- sample(0:1, 30, replace = TRUE)
practice_data$f8 <- sample(0:1, 30, replace = TRUE)
practice_data$f9 <- sample(0:1, 30, replace = TRUE)
practice_data$f10 <- sample(0:1, 30, replace = TRUE)
practice_data$f11 <- sample(0:1, 30, replace = TRUE)
practice_data$f12 <- sample(0:1, 30, replace = TRUE)
practice_data$f13 <- sample(0:1, 30, replace = TRUE)
practice_data$f14 <- sample(0:1, 30, replace = TRUE)
practice_data$f15 <- sample(0:1, 30, replace = TRUE)
practice_data$f16 <- sample(0:1, 30, replace = TRUE)
practice_data$f17 <- sample(0:1, 30, replace = TRUE)
practice_data$f18 <- sample(0:1, 30, replace = TRUE)
practice_data$f19 <- sample(0:1, 30, replace = TRUE)
practice_data$f20 <- sample(0:1, 30, replace = TRUE)
practice_data$f21 <- sample(0:1, 30, replace = TRUE)
practice_data$f22 <- sample(0:1, 30, replace = TRUE)
practice_data$f23 <- sample(0:1, 30, replace = TRUE)
practice_data$f24 <- sample(0:1, 30, replace = TRUE)
practice_data$f25 <- sample(0:1, 30, replace = TRUE)
practice_data$f26 <- sample(0:1, 30, replace = TRUE)

#column for total cafe score
practice_data <- practice_data %>% mutate (total_cafe = f1 + f2 + f3 + f4 + f5 + f6
                          + f7 + f8 + f9 + f10 + f11 + f12 + f13 + 
                            f14 + f15 + f16 + f17 + f18 + f19 + f20 
                          + f21 + f22 + f23 + f24 + f25 + f26)

#columns for each SPP observation 
practice_data$spp1 <- sample(0:1, 30, replace = TRUE)
practice_data$spp2 <- sample(0:1, 30, replace = TRUE)
practice_data$spp3 <- sample(0:1, 30, replace = TRUE)
practice_data$spp4 <- sample(0:1, 30, replace = TRUE)
practice_data$spp5 <- sample(0:1, 30, replace = TRUE)
practice_data$spp6 <- sample(0:1, 30, replace = TRUE)
practice_data$spp7 <- sample(0:1, 30, replace = TRUE)
practice_data$spp8 <- sample(0:1, 30, replace = TRUE)
practice_data$spp9 <- sample(0:1, 30, replace = TRUE)
practice_data$spp10 <- sample(0:1, 30, replace = TRUE)

#column for total SPP
practice_data <- practice_data %>% mutate (total_spp = spp1 + spp2 + spp3 + spp4
                          + spp5 + spp6 + spp7 + spp8 + spp9 + 
                            spp10)

#practice analyses 
#correlations between rae, spp, cafe, and age
cor_table <- cor(practice_data [, c("total_rae", "total_cafe", "total_spp", "Age")])
plot(practice_data$total_rae, practice_data$total_spp)
groupcolor <- ifelse(practice_data$Group == 1, "orange",
                      ifelse(practice_data$Group == 2, "blue", NA))
pairs(~ total_rae + total_spp + total_cafe + Age, data = practice_data, panel=function(x,y){
  points(x,y) 
  abline(lm(y~x), col='red') 
  points(x, y, col= groupcolor)})

m<-lmer(DV ~ age + 1|ID), data=qc2)
#m<-lmer(DV ~ age + (1|ID), data=df)
#m2<-lmer(DV ~dage+cafe+rae+ (1|ID), data =df)
#anova(m,m2)
#tab_model(m)
#plot_model(m, type = pred)