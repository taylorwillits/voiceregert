#Attach cleaned data file with mean scores 
library(readxl)
Thesis_Data_04_12 <- read_excel("Desktop/Thesis/Thesis Data/Thesis Data 04.12.xlsx")
attach(Thesis_Data_04_12)
data <- Thesis_Data_04_12

library(lavaan)

#Restaurant scenario (S1)
S1model.regret <- 'S1_Intent ~ S1_ActRegret
                  S1_ActRegret+S1_InRegret+S1_Intent ~ Condition
                  S1_Intent ~ S1_InRegret'
                
fit <- sem(S1model.regret, data=data)                
summary(fit)              
summary(fit, standardized=T, fit.measures=T, rsq=T)           

#Calculating indirect and total effects
#:= is used when a new variable that is a function of variables in the model, but not in the dataset
S1model.regret2 <- 'S1_Intent ~ b1*S1_ActRegret
                  S1_ActRegret ~ a1*Condition
                  S1_InRegret ~ a2*Condition
                  S1_Intent ~ cprime*Condition
                  S1_Intent ~ b2*S1_InRegret
                  ActRegret.Ind:=a1*b1
                  InRegret.Ind:=a2*b2
                  delta:=ActRegret.Ind - InRegret.Ind
                  total:= cprime + ActRegret.Ind + InRegret.Ind'

fit.S1 <- sem(S1model.regret2, data=data, se="bootstrap", bootstrap=10000, missing="ML")
S1model.output <- summary(fit.S1, standardized=T, fit.measures=T, rsq=T, ci=TRUE) 

#Retail scenario (S2)
S2model.regret <- 'S2_Intent ~ b1*S2_ActRegret
                  S2_ActRegret ~ a1*Condition
                  S2_InRegret ~ a2*Condition
                  S2_Intent ~ cprime*Condition
                  S2_Intent ~ b2*S2_InRegret
                  ActRegret.Ind:=a1*b1
                  InRegret.Ind:=a2*b2
                  delta:=ActRegret.Ind - InRegret.Ind
                  total:= cprime + ActRegret.Ind + InRegret.Ind'

fit.S2 <- sem(S2model.regret, data=data, se="bootstrap", bootstrap=10000, missing="ML")
S2model.output <- summary(fit.S2, standardized=T, fit.measures=T, rsq=T, ci=TRUE)
