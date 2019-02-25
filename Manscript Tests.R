#loading packages to perform GLMMs
library(glmmADMB)

#Modules testing interactions and colinearity of walking and behavioural cat - HR AVG
GLMMI1 <- glmmadmb(mean10s ~ Experiment + Test.Period + Experiment*Walking_10 + Experiment*NewCat + (1|Name), data = DataManu, zeroInflation = FALSE, family ="gaussian")
summary(GLMMI1)
vif(GLMMI1)

#Modules testing interactions and colinearity of walking and behavioural cat - HR INCREASE
GLMMI2 <- glmmadmb(HR_Increase~ Experiment + Test.Period + Experiment*Walking_10 + Experiment*NewCat + (1|Name), data = DataManuHRI , zeroInflation = FALSE, family ="gaussian")
summary(GLMMI2)
vif(GLMMI2)


#Random effect analysis - Anova - Test the effect of the random effect on the model, by comparing model without the random effect to the model with the random effect
GLMMI1o <- glmmadmb(mean10s ~ Experiment + Test.Period + Experiment*Walking_10 + Experiment*NewCat , data = DataManu, zeroInflation = FALSE, family ="gaussian")
anova(GLMMI1o, GLMMI1) #mean 10s HR

GLMMI2o <- glmmadmb(HR_Increase~ Experiment + Test.Period + Experiment*Walking_10 + Experiment*NewCat , data = DataManuHRI , zeroInflation = FALSE, family ="gaussian")
anova(GLMMI2o, GLMMI2) #HR increase

#Post Hoc Tests (Tukey) - Testing differences in mean 10s HR between experimental conditions (NO, PF and Control)
library (multcomp)
summary(glht(GLMMI1, mcp(Experiment="Tukey")))


#Repeatability Analysis - Load Packages 
library(rptR)

#Subsetting the data in Novel object and Pre-feeding datasets
DataManuNO <- subset(DataManu, Experiment == 'Novel_Object')
DataManuPF <- subset(DataManu, Experiment == 'Pre_Feeding')

#mean 10s HR repeatability Novel Object test
rep1 <- rpt(mean10s ~ (1 | Name), grname = "Name", data = DataManuNO, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep1)


#HR Increase repeatability Novel Object test
rep2 <- rpt(HR_Increase ~ (1 | Name), grname = "Name", data = DataManuNO, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep2)


#Behaviour repeatability Novel Object test
rep3 <- rpt(CodeCat ~ (1 | Name), grname = "Name", data = DataManuNO, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep3)

#mean 10s HR repeatability pre-feeding test 
rep4 <- rpt(mean10s ~ (1 | Name), grname = "Name", data = DataManuPF, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep4)

# HR increase repeatability pre-feeding test
rep5 <- rpt(HR_Increase ~ (1 | Name), grname = "Name", data = DataManuPF, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep5)

#Behaviour repeatability pre-feeding test Test
rep6 <- rpt(CodeCat ~ (1 | Name), grname = "Name", data = DataManuPF, 
             datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep6)




