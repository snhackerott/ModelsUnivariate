#### Script to Compare Two Samples ####
## Author: Serena Hackerott, February 2025

## DataFrame should include a response variable and meta data experimental design variables
## Experimental design variables should include 1 predictor or grouping variable of interest 
## Appropriate for predictors with two groups (levels)

#### Load Required Packages ####
if (!require("onewaytests")) install.packages("onewaytests")


library(onewaytests) #Helpful for assumption testing


#### Check Assumptions ####
#Replace "ResponseVar" with the response variable of interest
#Replace "MetaVar" with meta data predictor variable of interest

##Check normality overall
#Replace "ResponseVar" with the response variable of interest
hist(DataFrame$ResponseVar)
qqnorm(DataFrame$ResponseVar)
shapiro.test(DataFrame$ResponseVar)

##Check normality by group
#Uses Shapiro-Wilk test and plots both qqplots and histograms
nor.test(ResponseVar~MetaVar, data=DataFrame, method=c('SW'), alpha=0.05, plot=c("qqplot-histogram"))

##Check homogeneity of variances
leveneTest(DataFrame$ResponseVar, DataFrame$MetaVar)

##Option with OneWayTests
homog.test(ResponseVar~MetaVar, data=DataFrame, method=c("Levene"))

#### t-test ####
#If data meets assumptions, proceed with t-test
#If variances are not equal, the Welch t-test can be used

##Classic t-test
t.test(ResponseVar~MetaVar, data=DataFrame, var.equal=TRUE)

##Welch t-test
t.test(ResponseVar~MetaVar, data=DataFrame, var.equal=FALSE)

##One-sided t-tests
#Is the mean of Group A < the mean of Group B?
t.test(ResponseVar~MetaVar, data=DataFrame, var.equal=TRUE, alternative = "less")

#Is the mean of Group A > the mean of Group B?
t.test(ResponseVar~MetaVar, data=DataFrame, var.equal=TRUE, alternative = "greater")


#### Wilcoxon Rank Sum Test ####
#Nonparametric equivalent of t-test
#Also known as Mann-Whitney test
#Can also adjust alternative = "less" or "greater" as appropriate for the study
wilcox.test(ResponseVar~MetaVar, data=DataFrame)


#### Suggested Figures ####
#Visualize Univariate responses with Boxplots, Mean and Error plots, etc as most appropriate for the data