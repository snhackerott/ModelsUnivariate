#### Script to Evaluate Univariate Data with lmer ####
## Author: Serena Hackerott, November 2024

## DataFrame should include a response variable and meta data experimental design variables
## Experimental design variables should include a variable to be included as a random effect


#### Load Required Packages ####
if (!require("lme4")) install.packages("lme4")
if (!require("lmerTest")) install.packages("lmerTest")
if (!require("DHARMa")) install.packages("DHARMa")
if (!require("emmeans")) install.packages("emmeans")
if (!require("effectsize")) install.packages("effectsize")
if (!require("dplyr")) install.packages("dplyr")

library(lme4) #Required for mixed effects modeling
library(lmerTest) #Required for p values with lme4 model summaries
library(DHARMa) #Required to check residuals of mixed effects models
library(emmeans) #Required for pairwise comparisons
library(effectsize) #Required for effect sizes
library(dplyr) #Required for data organization

#### Check Assumptions ####

##Check normality
#Replace "ResponseVar" with the response variable of interest
hist(DataFrame$Reponse_Var)
shapiro.test(DataFrame$Reponse_Var)

##Optional transformations for non-normal response data
#However, the normality of the model residuals are most important

##Compare with log(+1) transformation for non-normal right-skewed data
#If normal, use transformed response in the model
hist(log(DataFrame$Reponse_Var+1))
shapiro.test(log(DataFrame$Reponse_Var+1))

##Compare with squared transformation for non-normal left-skewed data
#If normal, use transformed response in the model
hist(DataFrame$Reponse_Var^2)
shapiro.test(DataFrame$Reponse_Var^2)


#### Linear Mixed Effects Model ####

##Run Model
#Replace "ResponseVar" with the response variable of interest
#Replace "MetaVar1" through "MetaVar3" with meta data predictor variables for main effects
#Replace "MetaVar4" with meta data predictor variable for random effect
lme.mod<-lmer(ResponseVar~MetaVar1 + MetaVar2 + MetaVar3 + (1|MetaVar4), data=DataFrame)


#### Model Residuals ####

##Check Residuals
lme.mod_re <- simulateResiduals(fittedModel = lme.mod, plot = F)
plot(lme.mod_re)

#### Model Results ####

##Results summary
summary(lme.mod)

##Optional ANOVA for overall effects
#Appropriate when a main effect variable has more than two levels
anova(lme.mod)


#### Effect Size ####

##Effect size for main effects
eta_squared(lme.mod)

##Save model results
lme.mod.res<-data.frame(anova(lme.mod))
lme.mod.res$EtaSq<-c(eta_squared(lme.mod)$Eta2)

#Option to add Response and Predictor columns
#Replace "Multivariate Response" with more meaningful description of response data
lme.mod.res$Response<-"Univariate Response"

#### Pairwise Comparison ####

##Pairwise comparisons across a single main effect variable
#Replace "MetaVar1" with meta data predictor variable of interest
lme.mod.pair<-emmeans(lme.mod, pairwise~MetaVar1)
lme.mod.pair

##Optional pairwise comparisons across a two main effect variables
#May be appropriate in the case of significant interactions
#Replace "MetaVar1" with meta data predictor variable of interest
#Replace "MetaVar2" with meta data predictor variable in which each comparison of MetaVar1 will be made
#Compares across MetaVar1 within the first level of MetaVar2 and also within the second level of MetaVar2
emmeans(lme.mod, pairwise~MetaVar1 | MetaVar2)

##Option to add a third stratifying predictor variable
#Only use if appropriate to the study design and research question
emmeans(lme.mod, pairwise~MetaVar1 | MetaVar2*MetaVar3)

##Standardized effect size for pairwise contrasts
lme.mod.pair.es<-data.frame(eff_size(lme.mod.pair, sigma=sigma(lme.mod), edf=df.residual(lme.mod)))
lme.mod.pair.es

#Clarify column names for effect size
lme.mod.pair.es<-lme.mod.pair.es %>% dplyr::rename(contrast.es = contrast, SE.es = SE, df.es = df)

##Save pairwise results
lme.mod.pair.res<-merge(data.frame(lme.mod.pair$contrasts), lme.mod.pair.es[,-c(1)])
lme.mod.pair.res$Response<-"Univariate Response"


#### Suggested Figures ####
#Visualize Univariate responses with Boxplots, Mean and Error plots, etc as most appropriate for the data