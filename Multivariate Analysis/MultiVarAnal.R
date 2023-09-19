#libraries required
library(lattice) #for barchart
library(corrplot) #for correlation plot
library(Epi) #for ROC curve
library(factoextra) #for biplot

# Data
AllData <- read.csv("MVA-Project-Data.csv")
data <- subset(AllData, Student != 202078239 | Student %in% c(NA))

str(data)
data <- data[, 1:17] #keep data we want
str(data) 
class(data)
data$diabetes <- as.factor(data$diabetes)
summary(data[,1:9])

#Check if there are any NAs in data
sapply(data, function(x) sum(is.na(x)))

###################### Investigate data ###############################

#barplot of response variable
tab <- table(data$diabetes)
# % pos/neg diabetes
prop.table(tab)*100

barchart(table(data$diabetes), horizontal = F, xlab="Diabetes", ylab="No. of Pima women tested", 
         col=c("light green", "red"), main="Diabetes test result for Pima women")

#histograms and barplot of data
par(mfrow=c(3,3))
barplot(table(data$pregnant), xlab="No. of pregnancies", ylab="Frequency", 
        main="Barchart of number of pregnancies", 
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, ylim=c(0,100))
hist(data$glucose, xlab="Glucose concentration", 
     main="Histogram of glucose concentration", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$pressure, xlab="Blood pressure (mm Hg)", 
     main="Histogram of diastolic blood pressure", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$triceps, xlab="Triceps skin fold thickness (mm)", 
     main="Histogram of Triceps skin fold thickness",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$insulin, xlab="2-hour serum insulin (mu U/ml)", 
     main="Histogram of Serum Insulin", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$mass, xlab="Body mass index (weight in kg/(height in m)^2)", 
     main="Histogram of Body Mass Index",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$pedigree, xlab="Diabetes pedigree function", 
     main="Histogram of Diabetes Pedigree Function",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$age, xlab="Age (years)", 
     main="Histogram of Age",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

#boxplots of data
par(mfrow=c(2,4))
boxplot(data$pregnant~data$diabetes, ylab="No. of pregnancies", 
        main="Boxplot of no. of pregnancies", 
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$glucose~data$diabetes, ylab="Glucose concentration", 
        main="Boxplot of glucose concentration", 
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$pressure~data$diabetes, ylab="Blood pressure (mm Hg)", 
        main="Boxplot of diastolic blood pressure", 
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$triceps~data$diabetes, ylab="Triceps skin fold thickness (mm)", 
        main="Boxplot of Triceps skin fold thickness",
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$insulin~data$diabetes, ylab="2-hour serum insulin (mu U/ml)", 
        main="Boxplot of Serum Insulin",cex.lab=1.5, 
        cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$mass~data$diabetes, ylab="Body mass index (weight in kg/(height in m)^2)", 
        main="Boxplot of Body Mass Index",
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$pedigree~data$diabetes, ylab="Diabetes pedigree function", 
        main="Boxplot of Diabetes Pedigree Function",
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")
boxplot(data$age~data$diabetes, ylab="Age (years)", 
        main="Boxplot of Age",
        cex.lab=1.5, cex.main=1.5, cex.axis=1.5, xlab="Diabetes test")

## covariances and correlations ##
pairs(data[,1:9])
covariance <- cov(data[,1:8])
correlation <- cor(data[,1:8])
print(covariance, digit=1)
print(correlation, digits=1)
par(mfrow=c(1,1))
corrplot(correlation, method="number", type="upper")

#histograms of scaled data
par(mfrow=c(2,4))
hist(data$pregnant.sc, xlab="No. of pregnancies", 
     main="Histogram of pregnancy numbers", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$glucose.sc, xlab="Glucose concentration", 
     main="Histogram of glucose concentration", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$pressure.sc, xlab="Blood pressure (mm Hg)", 
     main="Histogram of diastolic blood pressure", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$triceps.sc, xlab="Triceps skin fold thickness (mm)", 
     main="Histogram of Triceps skin fold thickness",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$insulin.sc, xlab="2-hour serum insulin (mu U/ml)", 
     main="Histogram of Serum Insulin",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$mass.sc, xlab="Body mass index (weight in kg/(height in m)^2)", 
     main="Histogram of Body Mass Index",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$pedigree.sc, xlab="Diabetes pedigree function", 
     main="Histogram of Diabetes Pedigree Function",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$age.sc, xlab="Age (years)", 
     main="Histogram of Age",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

# transformations of scaled data #
summary(data$pregnant.sc)
data$logpregnant.sc <- log(data$pregnant.sc+2)
hist(data$logpregnant.sc, xlab="Log of No. of pregnancies", 
     main="Histogram of log pregnancy numbers", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

summary(data$insulin.sc)
data$loginsulin.sc <- log(data$insulin.sc+2)
hist(data$loginsulin.sc, xlab="Log of 2-hour serum insulin (mu U/ml)", 
     main="Histogram of log Serum Insulin",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

summary(data$mass.sc)
data$logmass.sc <- log(data$mass.sc+3)
hist(data$logmass.sc, xlab="Log of Body mass index (weight in kg/(height in m)^2)", 
     main="Histogram of log Body Mass Index",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

summary(data$pedigree.sc)
data$logpedigree.sc <- log(data$pedigree.sc+2)
hist(data$logpedigree.sc, xlab="Log of Diabetes pedigree function", 
     main="Histogram of log of Diabetes Pedigree Function",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

summary(data$age.sc)
data$logage.sc <- log(data$age.sc+1)
hist(data$logage.sc, xlab="Log of Age (years)", 
     main="Histogram of log Age",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

# Resulting scaled & transformed data. Don't use though as not much better#
par(mfrow=c(2,4))
hist(data$logpregnant.sc, xlab="Log of No. of pregnancies", 
     main="Histogram of log pregnancy numbers", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$glucose.sc, xlab="Glucose concentration", 
     main="Histogram of glucose concentration", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$pressure.sc, xlab="Blood pressure (mm Hg)", 
     main="Histogram of diastolic blood pressure", cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$triceps.sc, xlab="Triceps skin fold thickness (mm)", 
     main="Histogram of Triceps skin fold thickness",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$loginsulin.sc, xlab="Log of 2-hour serum insulin (mu U/ml)", 
     main="Histogram of log Serum Insulin",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$logmass.sc, xlab="Log of Body mass index (weight in kg/(height in m)^2)", 
     main="Histogram of log Body Mass Index",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$logpedigree.sc, xlab="Log of Diabetes pedigree function", 
     main="Histogram of log Diabetes Pedigree Function",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
hist(data$logage.sc, xlab="Log of Age (years)", 
     main="Histogram of log Age",cex.lab=1.5, cex.main=1.5, cex.axis=1.5)


# Split data into training and test datasets
set.seed(100)
test.rows <- rbinom(nrow(data), 1, prob=0.25)
train.dat <- subset(data, test.rows==0) # 75% for training set
test.dat <- subset(data, test.rows==1) # 25% for test set
dim(train.dat) #298 obs
dim(test.dat) #93 obs

# Training Model with all scaled variables (untransformed model)
full.mod1.train <- glm(diabetes=="pos" ~ pregnant.sc + glucose.sc + pressure.sc + triceps.sc + 
                         insulin.sc + mass.sc + pedigree.sc + age.sc, 
                       data=train.dat, family=binomial)
summary(full.mod1.train)

#transformed model
full.mod2.train <- glm(diabetes=="pos" ~ logpregnant.sc + glucose.sc + pressure.sc + triceps.sc + 
                         loginsulin.sc + logmass.sc + logpedigree.sc + logage.sc, 
                       data=train.dat, family=binomial)
summary(full.mod2.train)

####### Continued with un-transformed feature variables ##############
# Use full.mod.1.train as full model #
full.mod1.train <- glm(diabetes=="pos" ~ pregnant.sc + glucose.sc + pressure.sc + triceps.sc + 
                         insulin.sc + mass.sc + pedigree.sc + age.sc, data=train.dat, family=binomial)
summary(full.mod1.train)
drop1(full.mod1.train, test="Chi")
#######
m <- glm(diabetes=="pos" ~ pregnant.sc + glucose.sc + pressure.sc + 
           insulin.sc + mass.sc + pedigree.sc + age.sc, data=train.dat, family=binomial)
drop1(m, test="Chi")
######
m2 <- glm(diabetes=="pos" ~ pregnant.sc + glucose.sc + insulin.sc + mass.sc + pedigree.sc + 
            age.sc, data=train.dat, family=binomial)
drop1(m2, test="Chi")
######
m3 <- glm(diabetes=="pos" ~ pregnant.sc + glucose.sc + mass.sc + pedigree.sc + age.sc, data=train.dat, 
          family=binomial)
drop1(m3, test="Chi")
######
final.mod.train <- glm(diabetes=="pos" ~ pregnant.sc + glucose.sc + mass.sc + pedigree.sc, 
                       data=train.dat, family=binomial)
drop1(final.mod.train, test="Chi")
summary(final.mod.train)

####################################################################
# Check chi-sqr test for difference in deviances
anova(final.mod.train, full.mod1.train, test="Chi")

#Confidence intervals for model coefficients
cbind(final.mod.train$coefficients, confint.default(final.mod.train)) #log odds CI
exp(cbind(final.mod.train$coefficients, confint.default(final.mod.train))) #odds CI
####################################################################
#Assess linearity assumption
probs <- predict(final.mod.train, type="response")
logits <- log(probs/(1-probs))
par(mfrow=c(2,4))
plot(train.dat$pregnant.sc, logits, xlab="Pregnancies", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$glucose.sc, logits, xlab="Glucose", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$pressure.sc, logits, xlab="Blood pressure", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$triceps.sc, logits, xlab="Triceps skin fold thickness", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$insulin.sc, logits, xlab="Serum insulin", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$mass.sc, logits, xlab="Body mass index", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$pedigree.sc, logits, xlab="Diabetes pedigree function", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
plot(train.dat$age.sc, logits, xlab="Age", ylab="Log odds of diabetes", 
     cex.lab=1.5, cex.main=1.5, cex.axis=1.5)

####################################################################
#Training predictions of the linear predictor
#get predictions for training data for ROC curve
pred.train <- predict(final.mod.train, newdata=train.dat) 

#ROC curve
par(mfrow=c(1,1))
ROC(pred.train, train.dat$diabetes, plot="ROC", main="ROC curve choosing cut-off value for classifier", 
    cex.lab=1.5, cex.main=1.5, cex.axis=1.3)
# Gives best cut-off to be -0.881

#Classification matrix/confusion matrix for training data
table(pred.train>-0.881, train.dat$diabetes)

#Predictions for test data
pred.test <- predict(final.mod.train, newdata=test.dat) #get predictions for test data

#Classification matrix for test data
table(pred.test>-0.881, test.dat$diabetes)

########## Principle component analysis ##############
#Use original set of data
data_o <- data[,1:8]
dim(data_o)
#Principle component analysis
pca_output <- prcomp(data_o, scale=T) #scale = T uses correlation for scale
summary(pca_output)
plot(pca_output, main="How many components?")
mtext(side=1, "Principal components")

pca_output #Look at loadings
fviz_pca_biplot(pca_output, col.var="red") #biplot




############# regression model using the pcs ##############
scores <- predict(pca_output)
scores <- as.data.frame(scores)
pca_mod <- glm(data$diabetes=="pos" ~ scores$PC1 + scores$PC2 + scores$PC3 + scores$PC4, family=binomial)
summary(pca_mod)
