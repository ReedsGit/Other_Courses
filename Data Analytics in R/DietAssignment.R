#libraries
library(MASS)
library(leaps)
############################################################################
#Access the data
load("project_V2_data.Rdata")
head(diet)
#No. of observations - 996
nrow(diet)
#Adding 3 to all data value due to problems transforming -ve values
min(diet)
diet2 <- diet+3
diet2$Patient <- diet$Patient
diet2$Week <- diet$Week  

#Attach the data for ease
attach(diet2)
#Make an object holding the variables and an object holding the correlation coefficient
variables <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", 
               "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", 
               "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27")

correlation <- c(cor(D1, CHOL),
                 cor(D2, CHOL), cor(D3, CHOL), cor(D4, CHOL), cor(D5, CHOL), cor(D6, CHOL), 
                 cor(D7, CHOL), cor(D8, CHOL), cor(D9, CHOL), cor(D10, CHOL), cor(D11, CHOL), 
                 cor(D12, CHOL), cor(D13, CHOL), cor(D14, CHOL), cor(D15, CHOL), cor(D16, CHOL), 
                 cor(D17, CHOL), cor(D18, CHOL), cor(D19, CHOL), cor(D20, CHOL), cor(D21, CHOL), 
                 cor(D22, CHOL), cor(D23, CHOL), cor(D24, CHOL), cor(D25, CHOL), cor(D26, CHOL), 
                 cor(D27, CHOL))
correlation <- round(correlation, digits=3)

#Make a data frame to hold the correlation data
corr_data <- data.frame(variables, correlation)
#New data frame with ordered correlations
ordered_corr <- corr_data[order(-correlation),]
#Find the dietary components with the strongest correlation with cholesterol
ordered_corr
par(mfrow=c(1,1))
#Plot the correlation coeff against the dietary components
barplot(ordered_corr$correlation, names=ordered_corr$variables, xlab="Dietary Component", 
        ylab="Sample Correlation Coefficient", ylim=c(-1,1), cex.lab = 1.3, cex.main=2, cex.axis = 1.5,
        main = "Sample Correlation Coefficient between Cholesterol and each Dietary Component")

#Fit a linear regression for CHOL based on D14, D19, D5, D4, D26. 
#c_mode is linear model based on variables chosen by highest absolute correlation coefficients
c_mod <- lm(CHOL ~ D14 + D19 + D5 + D4 + D26, data=diet2)
summary(c_mod)
Press_cmod <- sum(c_mod$resid^2/(1-hatvalues(c_mod))^2)
Press_cmod
#confint(c_mod, level=0.95)
anova(c_mod)

#Build ANOVA table
SSreg <- sum((c_mod$fitted.values-mean(diet2$CHOL))^2)  #SS_regression
SSres <- sum((diet2$CHOL-c_mod$fitted.values)^2)  #SS_residual
SStot <- sum((diet2$CHOL-mean(diet2$CHOL))^2)  #SS_total
MSreg <- (sum((c_mod$fitted.values-mean(diet2$CHOL))^2))/5  #Mean Square of regression
MSres <- (sum((diet2$CHOL-c_mod$fitted.values)^2))/990  #Mean square of residuals
Fstat <- MSreg/MSres  #F statistic of linear regression model
fdist <- qf(0.95, 5, 990)  #Critical value of F-distribution

#Plot cholesterol against each of D14, D19, D5, D4, D26
par(mfrow=c(3,2))
plot(D14, CHOL, xlab = "D14", ylab = "CHOL", cex.lab = 1.4, cex.main=1.8, cex.axis = 1.4, main = "Cholesterol against D14")
plot(D19, CHOL, xlab = "D19", ylab = "CHOL", cex.lab = 1.4, cex.main=1.8, cex.axis = 1.4, main = "Cholesterol against D19")
plot(D5, CHOL, xlab = "D5", ylab = "CHOL", cex.lab = 1.4, cex.main=1.8, cex.axis = 1.4, main = "Cholesterol against D5")
M5 <- lm(CHOL~D5, data=diet2)
abline(M5, lty=2)
legend(1.3, 4.8, "Regression line", lty=2)
plot(D4, CHOL, xlab = "D4", ylab = "CHOL", cex.lab = 1.4, cex.main=1.8, cex.axis = 1.4, main = "Cholesterol against D4")
plot(D26, CHOL, xlab = "D26", ylab = "CHOL", cex.lab = 1.4, cex.main=1.8, cex.axis = 1.4, main = "Cholesterol against D26")

#############################################################################
# Look at all the data for Variable selection

#Do the different patients have similar cholesterol levels
par(mfrow=c(1,1))
boxplot(CHOL~Patient, main="Cholesterol levels of each patient", ylab = "Cholesterol level", 
        cex.lab = 1.3, cex.main=1.5, cex.axis = 1.3)

#Plot cholesterol against all the independent variables
pdf("plots.pdf", width=14, height=20)
par(mfrow=c(7,4))
plot(Week, CHOL, xlab = "Week", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D1, CHOL, xlab = "D1", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D2, CHOL, xlab = "D2", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D3, CHOL, xlab = "D3", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D4, CHOL, xlab = "D4", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D5, CHOL, xlab = "D5", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D6, CHOL, xlab = "D6", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D7, CHOL, xlab = "D7", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D8, CHOL, xlab = "D8", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D9, CHOL, xlab = "D9", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D10, CHOL, xlab = "D10", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D11, CHOL, xlab = "D11", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D12, CHOL, xlab = "D12", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D13, CHOL, xlab = "D13", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D14, CHOL, xlab = "D14", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D15, CHOL, xlab = "D15", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D16, CHOL, xlab = "D16", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D17, CHOL, xlab = "D17", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D18, CHOL, xlab = "D18", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D19, CHOL, xlab = "D19", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D20, CHOL, xlab = "D20", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D21, CHOL, xlab = "D21", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D22, CHOL, xlab = "D22", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D23, CHOL, xlab = "D23", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D24, CHOL, xlab = "D24", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D25, CHOL, xlab = "D25", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D26, CHOL, xlab = "D26", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
plot(D27, CHOL, xlab = "D27", ylab = "CHOL", cex.lab = 1.5, cex.axis = 1.5)
dev.off()

###################################################################################
###### Investigate some transformations  #######
#Cycled over this code with minor tweaks to consider variable transformation:

#try lower powers for each variable of interest that may want transformed
par(mfrow=c(2,2))
plot((D23), CHOL)
plot(sqrt(D23), CHOL)
plot(log(D23), CHOL)
plot((1/(D23)), CHOL)
plot((D23)^(1/3), CHOL)

#try higher powers for each variable of interest that may want transformed
par(mfrow=c(2,2))
plot((D23), CHOL)
plot((D23)^2, CHOL)
plot(((D23)^3), CHOL)

detach(diet2)

####################################################################################
######## Check correlation coefficients between the variables for high colinearity  ###########
#correlation coefficients between the variables
corr_matrix <- cor(diet2)
corr_matrix
apply(corr_matrix, 1, function(x) which (abs(x)>0.85)) #Remove D21, D22, Week

#Remove patient from data set as similar results between the patients.
#D21 and D9 are identical as they have a correlation coefficient of 1 so need 
#Remove Week as highly correlated with a few of them and D22 as D10 has higher PRESS (see PRESS plot)
diet3 <- subset(diet2, select = -c(D21, Patient, D22, D2, D16))
#Remove also: D1, D6, D15, D20 from checking plots and press graph
diet3 <- subset(diet2, select = -c(D21, Patient, D22, D2,D16, D1, D6, D15, D20))

####### PRESS of full model from diet3 (excludes the newly dropped covariates)   ######
full.lm <- lm(CHOL~ Week+D3+D4+D5+D7+D8+D9+D10+D11+D12+D13+D14+D17+D18+D19+D23+D24+D25+D26+D27, 
              data = diet3)
press_full <- sum(full.lm$resid^2/(1-hatvalues(full.lm))^2)
press_full  #Press=28.92503

#################### Try model selection  #########################################
s1 <- lm(CHOL~1, data = diet3)

#AIC step-wise on diet3
step(s1, scope=~Week+D3+D4+D5+D7+D8+D9+D10+D11+D12+D13+D14+D17+D18+D19+D23+D24+D25+D26+D27, 
     direction = "both")
# Gives
stp1.lm <- lm((CHOL) ~ Week + D11 + D27 + D26 + D25 + D19 + D9 + D4 + D14 + D8 + 
                D12 + D7 + D13 + D5 + D17 + D18, data = diet3)
prs_stp1 <- sum(stp1.lm$resid^2/(1-hatvalues(stp1.lm))^2)
prs_stp1  #Press=28.74969

# AIC forwards on diet3
step(s1, scope=~Week+D3+D4+D5+D7+D8+D9+D10+D11+D12+D13+D14+D17+D18+D19+D23+D24+D25+D26+D27, 
     direction = "forward")   #Gives same as stepwise

#AIC backwards on diet3
step(full.lm, scope=~Week+D3+D4+D5+D7+D8+D9+D10+D11+D12+D13+D14+D17+D18+D19+D23+D24+D25+D26+D27, 
     direction = "backward")
#Gives
stp2.lm <- lm((CHOL) ~ Week + D3 + D4 + D5 + D7 + D9 + D11 + D12 + 
     D13 + D14 + D17 + D18 + D19 + D25 + D26 + D27, data = diet3)
prs_stp2 <- sum(stp2.lm$resid^2/(1-hatvalues(stp2.lm))^2)
prs_stp2    #Press=28.71744

par(mfrow=c(1,1))

#Box-cox
pdf("box.pdf", width=7, height=3.5)
par(cex.lab = 0.7, cex.main=0.9, cex.axis = 0.7)
boxcox(stp2.lm, plotit=TRUE) #check box-cox for full model
title("Box-cox Transformation Graph")
dev.off()
######## Sqrt CHOL ########
stp2t.lm <- lm(sqrt(CHOL) ~ Week + D3 + D4 + D5 + D7 + D9 + D11 + D12 + 
                D13 + D14 + D17 + D18 + D19 + D25 + D26 + D27, data = diet3)
summary(stp2.lm)
prs_stp2t <- sum((diet3$CHOL - (stp2t.lm$fitted.values)^2)^2/(1-hatvalues(stp2t.lm))^2)
prs_stp2t    #Press=27.89734

#Remove D17 then D13 then D18 as not significant in 
stp3t.lm <- lm(sqrt(CHOL) ~ Week + D3 + D4 + D5 + D7 + D9 + D11 + D12 + 
                 D14 + D19 + D25 + D26 + D27, data = diet3)
summary(stp3t.lm)
prs_stp3t <- sum((diet3$CHOL - (stp3t.lm$fitted.values)^2)^2/(1-hatvalues(stp3t.lm))^2)
prs_stp3t  #PRESS = 28.03202


## LEAPS  ######################
##### R-sq leaps on CHOL
leap_R1 <- leaps(x=diet3[, c(1,3:21)], y=diet3[,2], nbest = 10, method = "adjr2",
                 names = colnames(diet3[, -2]))

df1 <- data.frame(size=leap_R1$size, AdjR2=round(leap_R1$adjr2, 3),   #LnB R-sq on CHOL
                  leap_R1$which, row.names = NULL)

#Plot Leap AdjR-sq
pdf("leap.pdf", width=14, height=7)
plot(leap_R1$size, leap_R1$adjr2, ylab="Adjusted R-squared", 
     xlab = "Number of parameters, (including intercept)", 
     main = "Plot of Adjusted R-squared against the Number of Parameters", cex.lab = 1.5, cex.main=1.8, cex.axis = 1.5)
dev.off()
##################### leap_Cp #####
leap_Cp <- leaps(x=diet3[, c(1,3:21)], y=diet3[,2], nbest = 10, method = "Cp",
                 names = colnames(diet3[, -c(2,22)]))

df1cp <- data.frame(size=leap_Cp$size, Cp=leap_Cp$Cp,   #LnB Cp on diet3
                    leap_Cp$which, row.names = NULL)

############# Transformed CHOL  ##################
#### transformed CHOL leaps R-squared
diet3$CHOLsqrt <- sqrt(diet3$CHOL)
leap_R1t <- leaps(x=diet3[, c(1,3:22)], y=diet3[,22], nbest = 15, method = "adjr2",
                 names = colnames(diet3[, -c(2, 22)]))

df1t <- data.frame(size=leap_R1t$size, AdjR2=round(leap_R1t$adjr2, 3), #LnB R-sq on sqrt(CHOL)
                  leap_R1t$which, row.names = NULL)

R1t.lm <- lm(CHOLsqrt ~ Week + D4 + D9 + D11 + D14 + D19 + D25 + D26 + D27, data = diet3)
prs_R1t <- sum((diet3$CHOL - (R1t.lm$fitted.values)^2)^2/(1-hatvalues(R1t.lm))^2)
prs_R1t #PRESS = 28.75727

####################################
# Trial 9 covariate model with transformations
#Current model
a.lm <- lm(CHOLsqrt ~ Week + D4 + D9 + D11 + D14 + D19 + D25 + D26 + D27, data=diet3)
summary(a.lm) #All significant
prs_a <- sum((diet3$CHOL - (a.lm$fitted.values)^2)^2/(1-hatvalues(a.lm))^2)
prs_a  #PRESS 28.75727

#Look at residual plots
par(mfrow=c(2,2))
plot(a.lm)
par(mfrow=c(1,1))
plot(CHOLsqrt~D19, data = diet3)

#transform D19 to 1/D19
diet3$D19i <- 1/diet3$D19
a2.lm <- lm(CHOLsqrt ~ Week + D4 + D9 + D11 + D14 + D19i + D25 + D26 + D27, data=diet3)
summary(a2.lm) # all signif
prs_a2 <- sum((diet3$CHOL - (a2.lm$fitted.values)^2)^2/(1-hatvalues(a2.lm))^2)
prs_a2  #PRESS 25.91941
#Check diagnostic plots
par(mfrow=c(2,2))
plot(a2.lm)

#Try poly of D19 instead 
a2.2.lm <- lm(CHOLsqrt ~ Week + D4 + D9 + D11 + D14 + poly(D19,3) + D25 + D26 + D27, data=diet3)
summary(a2.2.lm) # all signif
prs_a2.2 <- sum((diet3$CHOL - (a2.2.lm$fitted.values)^2)^2/(1-hatvalues(a2.2.lm))^2)
prs_a2.2  #PRESS 25.72365
plot(a2.2.lm)

#add polynomials to D4
a3.lm <- lm(CHOLsqrt ~ Week + poly(D4,3) + D9 + D11 + D14 + D19i + D25 + D26 + D27, data=diet3)
summary(a3.lm) 
prs_a3 <- sum((diet3$CHOL - (a3.lm$fitted.values)^2)^2/(1-hatvalues(a3.lm))^2)
prs_a3 #PRESS 24.83159
plot(a3.lm)

#add polynomials to D19
a3.2.lm <- lm(CHOLsqrt ~ Week + poly(D4,3) + D9 + D11 + D14 + poly(D19) + D25 + D26 + D27, data=diet3)
summary(a3.lm) 
prs_a3.2 <- sum((diet3$CHOL - (a3.2.lm$fitted.values)^2)^2/(1-hatvalues(a3.2.lm))^2)
prs_a3.2 #PRESS 26.68093
plot(a3.2.lm)

###########Try forwards selection using hypothesis testing#########
diet2$D4s2 <- (diet2$D4)^2
diet2$D4s3 <- (diet2$D4)^3
diet2$D5s2 <- (diet2$D5)^2
diet2$D5s3 <- (diet2$D5)^3
diet2$D5s4 <- (diet2$D5)^4
diet2$D19s2 <- (diet2$D19)^2
diet2$D19s3 <- (diet2$D19)^3
diet2$CHOLsqrt <- sqrt(diet2$CHOL)
diet2$D19i <- 1/diet2$D19

forw.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D9 + D11 + D14 + D19i + D25 + D26 + D27, data=diet2)
add1(forw.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19i+D20+D22+D23+D24+D25+D26+D27)
#add D7
forw2.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D7 +D9 + D11 + D14 + D19i + D25 + D26 + D27, data=diet2)
prs_f2 <- sum((diet2$CHOL - (forw2.lm$fitted.values)^2)^2/(1-hatvalues(forw2.lm))^2)
prs_f2 #PRESS 24.91256
add1(forw2.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19i+D20+D22+D23+D24+D25+D26+D27)
#Add to scope: poly to D19 instead of D19i then add to formula
forw3.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 +D7 +D9 + D11 + D14 + D19 + D19s2 + 
                       D19s3 + D25 + D26 + D27, data=diet2)
prs_f3 <- sum((diet2$CHOL - (forw3.lm$fitted.values)^2)^2/(1-hatvalues(forw3.lm))^2)
prs_f3 #PRESS 24.51653
summary(forw3.lm)

add1(forw3.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D20
forw4.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D7 +D9 + D11 + D14 + D19 + D19s2 + 
                       D19s3 + D20 +D25 + D26 + D27, data=diet2)
prs_f4 <- sum((diet2$CHOL - (forw4.lm$fitted.values)^2)^2/(1-hatvalues(forw4.lm))^2)
prs_f4 #PRESS 24.42786
summary(forw4.lm)

add1(forw4.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add polys to D5
forw5.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + D11 + D14 + D19 + D19s2 + 
                       D19s3 + D20 +D25 + D26 + D27, data=diet2)
prs_f5 <- sum((diet2$CHOL - (forw5.lm$fitted.values)^2)^2/(1-hatvalues(forw5.lm))^2)
prs_f5 #PRESS 24.0708
summary(forw5.lm)

add1(forw5.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D18
forw6.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + D11 + D14 + D18 +D19 + D19s2 + 
                       D19s3 + D20 +D25 + D26 + D27, data=diet2)
prs_f6 <- sum((diet2$CHOL - (forw6.lm$fitted.values)^2)^2/(1-hatvalues(forw6.lm))^2)
prs_f6 #PRESS 23.56538
summary(forw6.lm)

#Remove D5 from forw6.lm due to summary
add1(forw6.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D12
forw7.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + D11 +D12+ D14 + D18 +D19 + D19s2 + 
                       D19s3 + D20 +D25 + D26 + D27, data=diet2)
prs_f7 <- sum((diet2$CHOL - (forw7.lm$fitted.values)^2)^2/(1-hatvalues(forw7.lm))^2)
prs_f7 #PRESS 23.19508
add1(forw7.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)

plot(forw7.lm) # looking fine
#add D22
forw8.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + D11 +D12+ D14 + D18 +D19 + D19s2 + 
                       D19s3 + D20 + D22 +D25 + D26 + D27, data=diet2)
prs_f8 <- sum((diet2$CHOL - (forw8.lm$fitted.values)^2)^2/(1-hatvalues(forw8.lm))^2)
prs_f8 #PRESS 23.18523
add1(forw8.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)

#add D13
forw9.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + 
                       D11 +D12+ D13 +D14 + D18 +D19 + D19s2 + D19s3 + D20 + D22 +D25 + 
                       D26 + D27, data=diet2)
prs_f9 <- sum((diet2$CHOL - (forw9.lm$fitted.values)^2)^2/(1-hatvalues(forw9.lm))^2)
prs_f9 #PRESS 23.09158
add1(forw9.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)

#add D24 --> this is worse so try adding next best one instead D16
forw10.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + 
                        D11 +D12+ D13 +D14 +D16 + D18 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + 
                        D26 + D27, data=diet2)
prs_f10 <- sum((diet2$CHOL - (forw10.lm$fitted.values)^2)^2/(1-hatvalues(forw10.lm))^2)
prs_f10 #PRESS 23.00979
add1(forw10.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D24
forw11.lm <- lm(CHOLsqrt ~ Week + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + 
                        D11 +D12+ D13 +D14 +D16 + D18 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
                + D26 + D27, data=diet2)
prs_f11 <- sum((diet2$CHOL - (forw11.lm$fitted.values)^2)^2/(1-hatvalues(forw11.lm))^2)
prs_f11 #PRESS 22.95145
add1(forw11.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D1
forw12.lm <- lm(CHOLsqrt ~ Week + D1 + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + 
                        D11 +D12+ D13 +D14 +D16 + D18 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
                + D26 + D27, data=diet2)
prs_f12 <- sum((diet2$CHOL - (forw12.lm$fitted.values)^2)^2/(1-hatvalues(forw12.lm))^2)
prs_f12 #PRESS 22.5806
add1(forw12.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D3
forw13.lm <- lm(CHOLsqrt ~ Week + D1 + D3 + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D9 + 
                        D11 +D12+ D13 +D14 +D16 + D18 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
                + D26 + D27, data=diet2)
prs_f13 <- sum((diet2$CHOL - (forw13.lm$fitted.values)^2)^2/(1-hatvalues(forw13.lm))^2)
prs_f13 #PRESS 22.2821
add1(forw13.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#add D8
forw14.lm <- lm(CHOLsqrt ~ Week + D1 + D3 + D4 + D4s2 + D4s3 + D5s4 + D5s3 +D5s2 + D5 +D7 +D8 +D9 + 
                        D11 +D12+ D13 +D14 +D16 + D18 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
                + D26 + D27, data=diet2)
prs_f14 <- sum((diet2$CHOL - (forw14.lm$fitted.values)^2)^2/(1-hatvalues(forw14.lm))^2)
prs_f14 #PRESS 22.03144
add1(forw14.lm, test = "F", scope = ~Week+D1+D2+D3+D4+D4s2+D4s3+D5+D5s2+D5s3+D5s4+D6+D7+D8+D9+D10+D11+D12+D13
     +D14+D15+D16+D17+D18+D19+D19s2+D19s3+D20+D22+D23+D24+D25+D26+D27)
#No more are significant
plot(forw14.lm)
summary(forw14.lm)

#Remove D5
stop1.lm <- lm(CHOLsqrt ~ Week + D1 + D3 + D4 + D4s2 + D4s3 +D7 +D8 +D9 + 
                       D11 +D12+ D13 +D14 +D16 + D18 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
               + D26 + D27, data=diet2)
st1 <- sum((diet2$CHOL - (stop1.lm$fitted.values)^2)^2/(1-hatvalues(stop1.lm))^2)
st1 #PRESS 22.81513
summary(stop1.lm)

#Remove D18
stop2.lm <- lm(CHOLsqrt ~ Week + D1 + D3 + D4 + D4s2 + D4s3 +D7 +D8 +D9 + 
                       D11 +D12+ D13 +D14 +D16 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
               + D26 + D27, data=diet2)
st2 <- sum((diet2$CHOL - (stop2.lm$fitted.values)^2)^2/(1-hatvalues(stop2.lm))^2)
st2 #PRESS 22.80016
summary(stop2.lm)

#Remove D8
stop3.lm <- lm(CHOLsqrt ~ Week + D1 + D3 + D4 + D4s2 + D4s3 +D7 +D9 + 
                       D11 +D12+ D13 +D14 +D16 +D19 + D19s2 + D19s3 + D20 + D22  +D25 + D24
               + D26 + D27, data=diet2)
st3 <- sum((diet2$CHOL - (stop3.lm$fitted.values)^2)^2/(1-hatvalues(stop3.lm))^2)
st3 #PRESS 22.94793
summary(stop3.lm)
#All coefficients are now significant

############## Models of interest ##############
# 9 covariate model PRESS 24.83159
#a3.lm <- lm(CHOLsqrt ~ Week + poly(D4,3) + D9 + D11 + D14 + D19i + D25 + D26 + D27, data=diet2)
#prs_a3 <- sum((diet2$CHOL - (a3.lm$fitted.values)^2)^2/(1-hatvalues(a3.lm))^2)
#prs_a3 
#plot(a.lm$residuals ~ diet2$D9)

# 9 covariate model, poly D19     PRESS 24.63983
a3p.lm <- lm(CHOLsqrt ~ Week + poly(D4,3) + D9 + D11 + D14 + poly(D19, 3) + D25 + D26 + D27, data=diet2)
prs_a3p <- sum((diet2$CHOL - (a3p.lm$fitted.values)^2)^2/(1-hatvalues(a3p.lm))^2)
prs_a3p
summary(a3p.lm) # all signif

# 18 covariate model  PRESS 22.94793
stop3.lm <- lm(CHOLsqrt ~ Week + D1 + D3 + poly(D4,3) +D7 +D9 + 
                 D11 +D12+ D13 +D14 +D16 +poly(D19,3) + D20 + D22 + D24 +D25 
               + D26 + D27, data=diet2)
st3 <- sum((diet2$CHOL - (stop3.lm$fitted.values)^2)^2/(1-hatvalues(stop3.lm))^2)
st3 


#Check residual plots
par(mfrow=c(2,4))
plot(a3p.lm, main = "9-Covariate Model", cex.lab = 1.5, cex.main=1.5, cex.axis = 1.5)
plot(stop3.lm, main = "18-Covariate Model", cex.lab = 1.5, cex.main=1.5, cex.axis = 1.5)

#Remove the CHOL transformation to re-check best option for CHOL transformation
#with box-cox
par(mfrow=c(1,1))
a3u.lm <- lm(CHOL ~ Week + poly(D4,3) + D9 + D11 + D14 + poly(D19, 3) + D25 + D26 + 
              D27, data=diet2)
boxcox(a3u.lm, plotit=TRUE)

#cube root CHOL
diet2$CHOLcub <- (diet2$CHOL)^(1/3)
a3t.lm <- lm(CHOLcub ~ Week + poly(D4,3) + D9 + D11 + D14 + poly(D19, 3) + D25 + D26 + 
               D27, data=diet2)
prs_a3t <- sum((diet2$CHOL - (a3t.lm$fitted.values)^3)^2/(1-hatvalues(a3t.lm))^2)
prs_a3t
par(mfrow=c(2,2))
plot(a3t.lm)

#Check model residuals against each covariate
par(mfrow=c(1,1))
plot(a3p.lm$residuals ~ diet2$D9)

#try poly transformation on D9 

b.lm <- lm(CHOLsqrt ~ Week + poly(D4,3)+ poly(D9,4) + D11 + D14 + poly(D19,3) + D25 + D26 + D27, 
            data=diet2)
summary(b.lm)
prs_b <- sum((diet2$CHOL - (b.lm$fitted.values)^2)^2/(1-hatvalues(b.lm))^2)
prs_b #21.65254
par(mfrow=c(2,2))
plot(b.lm)

#Check sqrt is the right transformation of CHOL now
par(mfrow=c(1,1))
bu.lm <- lm(CHOL ~ Week + poly(D4,3)+ D9 + D11 + D14 + poly(D19,3) + D25 + D26 + D27, 
           data=diet2)
boxcox(bu.lm, plotit=TRUE)

a3p.lm <- lm(CHOLsqrt ~ Week + poly(D4,3) + D9 + D11 + D14 + poly(D19, 3) + D25 + D26 + D27, data=diet2)
prs_a3p <- sum((diet2$CHOL - (a3p.lm$fitted.values)^2)^2/(1-hatvalues(a3p.lm))^2)
prs_a3p
summary(a3p.lm) # all signif
confint(a3p.lm) #confidence intervals

# Press graph through time
all.lm <- lm(CHOL~Patient+Week+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13
             +D14+D15+D16+D17+D18+D19+D20+D21+D22+D23+D24+D25+D26+D27, data=diet)
Press_all <- sum(all.lm$resid^2/(1-hatvalues(all.lm))^2)
Press_all

p <- c(27.44054, 28.92503, 28.74969, 28.71744, 27.89734, 28.03202, 28.75727, 28.75727, 
       25.91941, 25.72365, 24.83159, 26.68093, 24.91256, 24.91256, 24.42786, 24.0708, 
       23.56538, 23.19508, 23.18523, 23.09158, 23.00979, 22.95145, 22.5806, 22.2821, 
       22.03144, 22.81513, 22.80016, 22.94793, 24.63983)
t <- 1:29
plot(t, p, main="Model PRESS values", xlab = "No. of model tested", ylab = "PRESS statistic", pch=7)

