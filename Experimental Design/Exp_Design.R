library(lme4)
library(nlme)
#library(multcomp)
library(emmeans)

datafull <- read.csv("ExpDesign-Project-Data.csv", header=T)
data <- subset(datafull, Student!=202078239 | Student %in% c(NA)) #My data
data <- data[,1:4]
summary(data)
#Organise data appropriately
#3 treatments: Put FBZ first as DOR and IVM are made similarly
data$Treatment <- factor(data$Treatment, levels = c("FBZ", "DOR", "IVM"))  
data$AnimalID <- factor(data$AnimalID)  #31 cows/treatment
data$Day <- factor(data$Day)  #3 times to collect data
data$cow <- as.factor(paste(data$Treatment, data$AnimalID, sep="-"))
data$cow <- factor(data$cow, levels=c("FBZ-1", "FBZ-2", "FBZ-3", "FBZ-4", "FBZ-5", 
                                     "FBZ-6", "FBZ-7", "FBZ-8", "FBZ-9", "FBZ-10", 
                                     "FBZ-11", "FBZ-12", "FBZ-13", "FBZ-14", "FBZ-15", 
                                     "FBZ-16", "FBZ-17", "FBZ-18", "FBZ-19", "FBZ-20", 
                                     "FBZ-21", "FBZ-22", "FBZ-23", "FBZ-24", "FBZ-25", 
                                     "FBZ-26", "FBZ-27", "FBZ-28", "FBZ-29", "FBZ-30", "FBZ-31", 

                                     "DOR-32", "DOR-33", "DOR-34", "DOR-35", "DOR-36", "DOR-37", "DOR-38", 
                                     "DOR-39", "DOR-40", "DOR-41", "DOR-42", "DOR-43", "DOR-44", 
                                     "DOR-45", "DOR-46", "DOR-47", "DOR-48", "DOR-49", "DOR-50", 
                                     "DOR-51", "DOR-52", "DOR-53", "DOR-54", "DOR-55", "DOR-56", 
                                     "DOR-57", "DOR-58", "DOR-59", "DOR-60", "DOR-61", "DOR-62",
                                     
                                     "IVM-63", "IVM-64", "IVM-65", "IVM-66", "IVM-67", "IVM-68", 
                                     "IVM-69", "IVM-70", "IVM-71", "IVM-72", "IVM-73", "IVM-74", 
                                     "IVM-75", "IVM-76", "IVM-77", "IVM-78", "IVM-79", "IVM-80", 
                                     "IVM-81", "IVM-82", "IVM-83", "IVM-84", "IVM-85", "IVM-86", 
                                     "IVM-87", "IVM-88", "IVM-89", "IVM-90", "IVM-91", "IVM-92", "IVM-93"))
summary(data)

#boxplots to visualise data
par(mfrow=c(1,1))
#boxplot(sqrt(FECs)~Treatment, data, main="FECs for each treatment group")
#boxplot(sqrt(FECs)~Day, data, main="Square root of FECs at 0, 14 and 21 days after treatment")
boxplot(sqrt(FECs)~Treatment:Day, data, col=(c("lightblue","lightgreen", "orange")), las=1,
        main="Square root of FECs by treatment type over time", ylab=expression(sqrt(FEC)))
#boxplot(sqrt(FECs)~Day:Treatment, data, 
#        main="Boxplot of Faecal egg counts by treatment type over time")


#Fit fixed effects model to check model assumptions
z.lm <- lm(FECs~Treatment + cow + Day + Treatment:Day, data)
#summary(z.lm)
#par(mfrow=c(2,2))
#plot(z.lm)  #Plots look fine

#Other residual plots
par(mfrow=c(2,3))
stan_res <- (residuals(z.lm)-mean(residuals(z.lm)))/sd(residuals(z.lm))
#Histogram of resids
hist(stan_res, main = "Histogram of Standardised Residuals", xlab="Standardised Residuals", 
     ylab = "Frequency", cex.lab=1.5, cex.main=1.5, cex.axis=1.2, las=1)
#Density plot of resids
plot(density(stan_res), main = "Density Plot of Standardised Residuals", 
     ylab="Density", xlab="Standardised Residuals", cex.lab=1.5, cex.main=1.5, cex.axis=1.2, las=1)
#QQ plot
qqnorm(stan_res, cex.lab=1.5, cex.main=1.5, cex.axis=1.2, las=1)
abline(a=0, b=1)  #and its line
#Versus fit of resids
plot(stan_res ~ fitted(z.lm), main="Versus Fits Plot", xlab="Fitted Values", 
     ylab="Residuals", cex.lab=1.5, cex.main=1.5, cex.axis=1.2, las=1)
abline(a=0, b=0)
#Temporal Plot - needed as observations taken over time
plot(stan_res, type="b", main = "Time series Plot of Residuals", 
     ylab="Residuals", xlab="Order", cex.lab=1.5, cex.main=1.5, cex.axis=1.2, las=1)

#Missing data point so unbalanced --> use lmer/lme
z.lmer <- lmer(FECs ~ Treatment*Day + (1|cow), data)
summary(z.lmer)
#anova(z.lmer)

#Can look at lme output also
z.lme <- lme(FECs ~ Treatment + Day + Day:Treatment, random = ~ 1|cow, data, method = "REML")
summary(z.lme)
anova(z.lme)

#interaction plot
par(mfrow=c(1,1))
with(data, interaction.plot(Day, Treatment, FECs, las=1,
                            main="Interaction plot of treatment and time after treatment", 
                            col = c("blue2", "green", "orange")))

emmeans(z.lme, pairwise~Treatment|Day, adjust="Tukey")
emmeans(z.lme, pairwise~Day|Treatment, adjust="Tukey")

########
#Get R^2
resid <- residuals(z.lme)  #Obtain residuals
mean_FECs <- mean(data$FECs)

#The percentage of variance explained by both fixed and random effects
(R2_both <- 1 - sum(resid^2)/sum((data$FECs - mean_FECs)^2))
