#### Script to Evaluate Univariate Data with ANOVA ####
## Author: Serena Hackerott, February 2025

## DataFrame should include a response variable and meta data experimental design variables
## Experimental design variables should include 1-2 predictor variables of interest


#### Load Required Packages ####
if (!require("onewaytests")) install.packages("onewaytests")
if (!require("DescTools")) install.packages("DescTools")
if (!require("FSA")) install.packages("FSA")

library(onewaytests) #Helpful for assumption testing
library(DescTools) #Required to perform Dunnett's test
library(FSA) #Required to perform Dunnett's test


#### Check Assumptions ####
#Replace "ResponseVar" with the response variable of interest
#Replace "MetaVar1" with meta data predictor variable of interest

##Check normality overall
#Replace "ResponseVar" with the response variable of interest
hist(DataFrame$Reponse_Var)
qqnorm(DataFrame$Reponse_Var)
shapiro.test(DataFrame$Reponse_Var)

##Check normality by group
#Uses Shapiro-Wilk test and plots both qqplots and histograms
nor.test(Reponse_Var~MetaVar1, data=DataFrame, method=c('SW'), alpha=0.05, plot=c("qqplot-histogram"))

##Check homogeneity of variances
leveneTest(DataFrame$Reponse_Var, DataFrame$MetaVar1)

##Option with OneWayTests
homog.test(Reponse_Var~MetaVar1, data=DataFrame, method=c("Levene"))


#### One Way ANOVA Model ####
#If data meets assumptions, proceed with ANOVA
#Replace "ResponseVar" with the response variable of interest
#Replace "MetaVar1" with meta data predictor variable of interest
aov.mod<-aov(ResponseVar~MetaVar1, data=DataFrame)

##Model results
summary(aov.mod)

#### Pairwise Comparison ####

##Pairwise comparison of each group
TukeyHSD(aov.mod)

##Pairwise comparison of each treatment group to a control group
#Replace "Control" with the exact match of the control level within MetaVar1
DunnettTest(ResponseVar~MetaVar1, data=DataFrame, control="Control")

#Note, see p.adjust function for options to adjust for multiple comparisons

#### Nonparametric Kruskal Wallis Test ####
#Nonparametric equivalent of one-way ANOVA
kruskal.test(ResponseVar~MetaVar1, data=DataFrame)

##Post-Hoc Pairwise Comparison 
#Uses Bonferroni correction for multiple comparisons but see other options
dunnTest(ResponseVar~MetaVar1, data=DataFrame, method="bonferroni")


#### Two Way ANOVA Model ####



#### Suggested Figures ####
#Visualize Univariate responses with Boxplots, Mean and Error plots, etc as most appropriate for the data