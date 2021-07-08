#Set working directory 
setwd("C:\\Users\\kapil\\Desktop\\GitHub\\Two way anova test")
getwd()

#Load Some libraries
library(car)
library(MASS)
library(ggplot2)
library(dplyr)

#Load the internal dataset birthweight 
Data <- birthwt

#Check the column names 
colnames(Data)


#Dplyr function for renaming the columns and selecting the columns you need 
Data.2 <- Data %>% 
  rename(Mother.smokes = "smoke", 
         Hypertension = "ht", 
         Physician.visits = "ftv",
         Ethinicity = "race",
         Birth.weight = "bwt") %>% 
  select(Mother.smokes, Hypertension, 
         Physician.visits, Ethinicity,
         Birth.weight)

#Mother sokes or not in a tabular 
table(Data.2$Mother.smokes)

#Boxplot to check the effect of mother smokes or not on birthweight of the child 
qplot(factor(x = Mother.smokes), y = Birth.weight,
             geom = "boxplot",
             main = "Birth weight by Mother Smokes or not",
             data = Data.2,
             xlab = "Mother Smokes",
             ylab = "Birth weight in Grams",
             fill = I("288BA8"))


#Boxplot to check the effect of ethnicity on birthweight of the child
qplot(factor(x = Ethinicity), y = Birth.weight,
      geom = "boxplot",
      main = "Birth weight by Ehninicity",
      data = Data.2,
      xlab = "Race",
      ylab = "Birth weight in Grams",
      fill = I("red"))

#First Anova model with No interaction between ethnicity and Mother smokes or not 
twowayANOVA_noInteraction <- aov(Birth.weight ~ 
                                   factor(Mother.smokes) +
                                   factor(Ethinicity),data = Data.2)


#Summary of twowayANOVA_noInteraction model 
#As both factors have **
#it meanse both are significant factor in determining the weight of the child 
summary(twowayANOVA_noInteraction)


#Second Anova model with interaction between ethnicity and Mother smokes or not
 twowayANOVA_Interaction <- aov(Birth.weight ~ 
                                 factor(Mother.smokes) *
                                 factor(Ethinicity),data = Data.2)



#Summary of twowayANOVA_Interaction model
#This time we assume some kind of interaction exist or not 
#The results tells us the interaction between smokes and ethnicity 
#is not signicant, but individually they plays a significant role
summary(twowayANOVA_Interaction)


#Tukey Honest significant test, to see which level has siginificant on Birth weight of  the child
#As our p.value shows more than 0.05 for combination no. 2-3 
#From the result the combination of 2-3 ethnicity shows no significant relation
TukeyHSD(twowayANOVA_Interaction, which = 'factor(Ethinicity)')


#Levene Test is use to check the equality of variances across diffrent categories
#As p.value more than 0.05 
#Results from the Levene Test shows the equality of variances in diffrent categories is present 
leveneTest(Birth.weight ~ factor(Mother.smokes) * factor(Ethinicity),
           data = Data.2)

#PLot to check whether our residuals in Data is normally distributed or not 
#From the plot - it shows our data is normally distributed 
plot(twowayANOVA_Interaction, 2, 
     col = "red", cex = 1.5) 


#Now we will check normality in our residuals with the help of shapiro test
#First extracts  residulas from the first model
aov_residuals <- residuals(object = twowayANOVA_Interaction)

#Now we will use the shapiro wilk test on residuals to check whether our data is normally distributed or not 
#AS you  can see the p.value is more than 0.05  it mmeanse our dataset i snot distributed 
shapiro.test(x = aov_residuals)

#Last Anova test from car package 
#we will use 'III' type, which  says unequal no. of samples in our dataset
#from this test also shows thhe same previous result 
#That each factor is significant in determining the birth weight of the child 
#But not the interacction of ethnicity and mother smokes or not 
Anova(twowayANOVA_Interaction, type = 'III')
