#### Script to Compare Proportions ####
## Author: Serena Hackerott, February 2025


#### Organize Proportion Dataframe ####

##DataFrame should include:
# 1) a column of the number of instances of successes per each group of interest
# 2) a column of the total number of trials per each group of interest

DataFrame$Successes<-c(NumberinGroup1, NumberinGroup2, ...)
DataFrame$Trials<-c(NumberinGroup1, NumberinGroup2, ...)

##Options to Create from a Larger Dataframe
#Replace Response with Variable being assessed relative to a certain threshold to determine "success"
#Replace Group with the Grouping Variable of interest
#Replace the ThresholdValue or the criteria within length() as appropriate for the research question
DataFrame<-aggregate(LargerDataFrame$Response, list(LargerDataFrame$Group), 
                     function (x) length(which(x>ThresholdValue)))
names(DataFrame)<-c("Group", "Successes")

#Add the total Trials column
DataFrame$Trials<-aggregate(LargerDataFrame$Response, list(LargerDataFrame$Group), 
                            function (x) length(x))[,2]


#### Compare Proportions Across Groups ####
#If two groups, two-proportion z test
#If 3+ groups, chi-squared proportion test

##Test for differences in Proportions between Groups
prop.test(DataFrame$Successes, DataFrame$Trials)

#Option to add direction in two-proportion z test
#Add within prop.test function
#alternative = "less" #Is the proportion in Group A < Group B ?
#alternative = "greater" #Is the proportion in Group A > Group B ?


#### Pairwise Comparison ####
##Appropriate if there is a significant difference identified across 3+ groups
#Uses Bonferroni correction for multiple comparisons but see other options
pairwise.prop.test(DataFrame$Successes, DataFrame$Trials, p.adjust.method="bonferroni")


#### Suggested Figures ####
#Visualize Univariate responses with Boxplots, Mean and Error plots, etc 
#Likely add a threshold value to a ggplot
#As a horizontal line:
+geom_hline(yintercept=ThresholdValue,linetype=1, color="grey", size=1)
#Or as a shaded region:
+annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=ThresholdValue, alpha=0.8, fill="grey")