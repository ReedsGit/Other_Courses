###### Libraries and read in data  ###########
library(AMR)
library(dplyr)
library(lubridate)

vac <- read.csv("vaccine_projdata_2.csv", stringsAsFactors = T)

######## Make factors of variables  ###########
vac$simd5 <- as.factor(vac$simd5)
vac$clinical.riskgroup <- as.factor(vac$clinical.riskgroup)

########## Check for NAs ##############
sum(is.na(vac$flu.pos))
sum(is.na(vac$flu.a))
sum(is.na(vac$flu.ah1n1))
sum(is.na(vac$flu.b))
sum(is.na(vac$gender))
sum(is.na(vac$age))             # suspected 197 NAs with age 0
sum(is.na(vac$simd5))
sum(is.na(vac$urbanrural))      # 59 NAs 
sum(is.na(vac$no.risk.gps))
sum(is.na(vac$clinical.riskgroup))
sum(is.na(vac$seasonal.fv.py))
sum(is.na(vac$nasal.fv.py))
sum(is.na(vac$fv.py))
sum(is.na(vac$seasonal.fv.py2))
sum(is.na(vac$nasal.fv.py2))
sum(is.na(vac$fv.py2))
sum(is.na(vac$seasonal.fv.py3))
sum(is.na(vac$nasal.fv.py3))
sum(is.na(vac$fv.py3))
sum(is.na(vac$vac1date))        # 1924 NAs for those not vaccinated
sum(is.na(vac$seasonal.vacc))
sum(is.na(vac$nasal.vacc))
sum(is.na(vac$vac1))
sum(is.na(vac$days))
sum(is.na(vac$test.date))

########################################################
#############  Explore the data  #######################
#flu.a
levels(vac$flu.a)
vac$flu.a <- factor(vac$flu.a, levels = c("positive", "negative"))
summary(vac$flu.a)
prop.table(table(vac$flu.a))
plot(vac$flu.a, ylim=c(0, 3000))
barplot(prop.table(table(vac$flu.a)), ylim = c(0,1))

#Gender
summary(vac$gender)
prop.table(table(vac$gender))      #Female 58.2%, Male 41.8% in study
plot(vac$flu.a~vac$gender)
table(vac$flu.a, vac$gender)
chisq.test(vac$flu.a, vac$gender)
table1 <- table(vac$flu.a, vac$gender) ; prop.table(table1, 2)  #proportion of each gender pos/neg for flu
table1 <- addmargins(table1, FUN = list(Total=sum)) ; table1
chisq.test(vac$flu.a, vac$gender)$expected

### Age  ###
vac$age <- vac$age         
summary(vac$age)
par(mfrow=c(1,1))
hist(vac$age, breaks = seq(0, 110, 5))
sum(vac$age==0)     #There are 197 people with age 0 ---> NAs ?
sum(vac$age<18)     #870 children under 18, 2290 adults

#Remove the 0 values as under 2's are not typically vaccinated, so high no. of 0's must be NAs 
vac$age_new <- vac$age
vac$age_new <- na_if(vac$age_new, 0)
sum(vac$age_new==0)
summary(vac$age_new)

#Graph of flu types by age *Working with age_new variable with NAs replacing 0*
plot(vac$flu.a ~ vac$age_new)     #age 10-20 and 40-50 surprisingly high +ve

# Split into age groupings
vac$age_grp <- age_groups(vac$age_new, split_at = c(18, 65))
levels(vac$age_grp)
length(vac$age_grp)
plot(vac$flu.a ~ vac$age_grp)   #plot flu.a outcome for each age group
prop.table((table(vac$flu.a, vac$age_grp)), 2)  #proportions pos/neg for flu according to age group

####################################################################################
#SIMD
summary(vac$simd5)
par(mfrow=c(1,2))
plot(vac$simd5)
plot(vac$flu.a ~ vac$simd5)     #Higher rates of flu in lower SIMD 
prop.table(table(vac$flu.a, vac$simd5),2)

#Urban/Rural
summary(vac$urbanrural)
plot(vac$urbanrural)
prop.table(table(vac$flu.a, vac$urbanrural),2)
plot(vac$flu.a ~ vac$urbanrural)  #Slightly higher rates of flu as more urban

#No. in risk groups
summary(vac$no.risk.gps)
par(mfrow=c(1,1))
plot(vac$flu.a ~ vac$no.risk.gps) 
#(Crudely) higher rates of flu with more risk groups but drops again at 6 risk grps (highly managed grp?)
#Suspect it depends on which risk group you are in that affects flu more or less

#Y/N risk groups
summary(vac$clinical.riskgroup)
plot(vac$clinical.riskgroup)
plot(vac$flu.a ~ vac$clinical.riskgroup)      #More flu in higher risk groups
prop.table(table(vac$flu.a, vac$clinical.riskgroup),2)

# Days since beginning flu season to flu test
summary(vac$days)
hist(vac$days)
plot(vac$flu.a ~ vac$days, breaks=seq(from=0, to=182, by = 7)) #weekly bars
plot(vac$flu.a ~ vac$days, breaks=seq(from=0, to=182, by = 14)) #fortnightly bars
plot(vac$flu.a ~ vac$days, breaks=seq(from=0, to=182, by = 30)) #monthly (ish) bars
# Very gradual increase in flu positivity rate over time from start of flu season to flu test

##########################   Dates  ############################################
#vaccination date
vac$vac1date <- dmy(vac$vac1date)    #function to change order date given in
summary(vac$vac1date)   #The NAs are those not vaccinated (numbers agree)
class(vac$vac1date)  

#flu test date
vac$test.date <- dmy(vac$test.date)   
summary(vac$test.date)

# Make month of flu test variable
vac$month <- months(vac$test.date)
table(vac$month)
vac$month <- factor(vac$month, levels = c("October", "November", "December", "January", "February", "March"))
table(vac$month)
plot(vac$flu.a~vac$month)

#Variable to measure no. of days from vaccination date to flu test date
vac$vacc_to_test_days <- difftime(vac$test.date, vac$vac1date, units = "days")
vac$vacc_to_test_days <- as.numeric(vac$vacc_to_test_days)
vac$vacc_to_test_days
summary(vac$vacc_to_test_days)
hist(vac$vacc_to_test_days)     # Lots of "vaccination to test" days <14 days 
# (considered to be when fully immunised after vacc)
# Lots of negatives too - tested before vaccinated
sum(is.na(vac$vacc_to_test_days))

# Define new vaccine variable for vaccinated=vaccinated for at least 14 days before test
# All others are 'not vaccinated'
# If vaccine14days = 1, they have been vaccinated for at least 14 days
# If vaccine14days = 0, they haven't
vac$vaccine14days <- ifelse(!is.na(vac$vacc_to_test_days) & vac$vacc_to_test_days>13, 1, 0)
table(vac$vacc_to_test_days, vac$vaccine14days, exclude=NULL)  #Can see which ones are included here
vac$vaccine14days <- as.factor(vac$vaccine14days)
plot(vac$vaccine14days)
table(vac$vaccine14days)
plot(vac$flu.a ~ vac$vaccine14days) 
table(vac$flu.a, vac$vaccine14days)

###### Instead of excluding vaccinated for >13 days, create a waning variable ###########
###### to show unvaccinated and vacc <14 days; vaccinated 14-90 days and vacc >90 days ###
dim(vac)
vac$waning <- cut(vac$vacc_to_test_days, breaks = c(-165, 13, 90, 193), labels = FALSE)
table(vac$vacc_to_test_days, vac$waning, exclude=NULL)
vac$waning <- ifelse(is.na(vac$waning) ,1, vac$waning)
vac$waning <- factor(vac$waning, labels = c("un-vaccinated or vaccinated <14 days ago", "vaccinated 14-90 days ago", "vaccinated >90 days ago"))
table(vac$waning)

########################  Current vaccines #####################################
# Notes: In separate seasonal/nasal ones, the 0 will include those who 
# have received the other vaccine NOT just the unvaccinated
#So need to make new dataset excl those who had nasal vaccine

########## Seasonal vaccine ###############
# All vaccine dates allowed (but some were given AFTER flu test)

### This modeling includes all data but subsetting to exclude nasal.vac="yes" data 
# as some of those who haven't had the seasonal vaccine HAVE had the nasal vaccine and we want to exclude those
data_seasonal <- subset(vac, nasal.vacc!="1") ; dim(data_seasonal)
dim(data_seasonal)

summary(data_seasonal$seasonal.vacc)
table(data_seasonal$seasonal.vacc, data_seasonal$vaccine14days)   #747 have had seasonal vaccine >=14 days before flu test
plot(data_seasonal$flu.a ~ data_seasonal$seasonal.vacc)    
table(data_seasonal$flu.a, data_seasonal$seasonal.vacc)    #curr seasonal vac -> less flu
prop.table(table(data_seasonal$flu.a, data_seasonal$seasonal.vacc), 2) 
chisq.test(data_seasonal$flu.a, data_seasonal$seasonal.vacc)

# Make new variable for seasonal vaccine received >=14 days before test
data_seasonal$seas.14 <- ifelse(data_seasonal$seasonal.vacc==1 & data_seasonal$vaccine14days==1, 1,0)
data_seasonal$seas.14 <- as.factor(data_seasonal$seas.14)
summary(data_seasonal$seasonal.vacc)
summary(data_seasonal$seas.14)
table(data_seasonal$seasonal.vacc, data_seasonal$vaccine14days)

# table flu.a against seasonal vaccine for those who had vaccine >14 days ago
par(mfrow=c(1,1))
table(data_seasonal$flu.a, data_seasonal$seas.14)
plot(data_seasonal$flu.a ~ data_seasonal$seas.14)
prop.table(table(data_seasonal$flu.a, data_seasonal$seas.14), 2)
chisq.test(data_seasonal$flu.a, data_seasonal$seas.14)

########## Nasal vaccine ###############
#Need to make new dataset excl those who had nasal vaccine
### This modeling includes all data but subsetting to exclude seasonal.vacc="yes" data 
# as some of those who haven't had the nasal vaccine HAVE had the seasonal vaccine and we want to exclude those
data_nasal <- subset(vac, seasonal.vacc!="1") ; dim(data_nasal)

summary(data_nasal$nasal.vacc)
table(data_nasal$nasal.vacc, data_nasal$vaccine14days)
plot(data_nasal$flu.a ~ data_nasal$nasal.vacc)    
table(data_nasal$flu.a, data_nasal$nasal.vacc)    #curr nasal vac -> a  lot less flu
prop.table(table(data_nasal$flu.a, data_nasal$nasal.vacc), 2) 

# Make new variable for nasal vaccine received >=14 days before test
data_nasal$nasal.14 <- ifelse(data_nasal$nasal.vacc==1 & data_nasal$vaccine14days==1, 1,0)

summary(data_nasal$nasal.14)   
data_nasal$nasal.14 <- as.factor(data_nasal$nasal.14)
table(data_nasal$nasal.14)     

# table flu.a against nasal vaccine for those who had vaccine >14 days ago
#par(mfrow=c(1,1))
table(data_nasal$flu.a, data_nasal$nasal.14)
plot(data_nasal$flu.a ~ data_nasal$nasal.14)
prop.table(table(data_nasal$flu.a, data_nasal$nasal.14), 2)
chisq.test(data_nasal$flu.a, data_nasal$nasal.14)

############ Overall vaccine ############
summary(vac$vac1)
table(vac$vac1, vac$vaccine14days)
plot(vac$flu.a ~ vac$vac1)    
table(vac$flu.a, vac$vac1)    #any curr vacc -> less flu
prop.table(table(vac$flu.a, vac$vac1), 2) 

#Are there any data saying yes to both nasal and seasonal
sum(vac$seasonal.vacc==1 & vac$nasal==1)

# Make new variable for overall vaccine received >=14 days before test
vac$allvac.14 <- ifelse(vac$vac1==1 & vac$vaccine14days==1, 1,0)


summary(vac$allvac.14)   
vac$allvac.14 <- as.factor(vac$allvac.14)
table(vac$allvac.14)     

# table flu.a against overall vaccine for those who had vaccine >14 days ago
#par(mfrow=c(1,1))
table(vac$flu.a, vac$allvac.14)
plot(vac$flu.a ~ vac$allvac.14)
prop.table(table(vac$flu.a, vac$allvac.14), 2)
chisq.test(vac$flu.a, vac$allvac.14)

#######################   Previous Year vaccines    ##############################
# Notes: Not enough time to look at nasal/seasonal ones separately
summary(vac$fv.py)
plot(vac$fv.py)
table(vac$flu.a, vac$fv.py)     #any prev yr vacc -> less flu
plot(table(vac$fv.py, vac$flu.a), xlab = "vacc previous year", ylab = "flu test", main="Flu positivity if had vaccine \n previous year")
prop.table(table(vac$flu.a, vac$fv.py), 2) 

####################### Vaccines received 2 years ago   ##############################

summary(vac$fv.py2)
plot(vac$flu.a ~ vac$fv.py2)
table(vac$flu.a, vac$fv.py2)     # 2 yr ago vacc -> less flu
prop.table(table(vac$flu.a, vac$fv.py2), 2) 
plot(table(vac$fv.py2, vac$flu.a), xlab = "vacc 2 years ago", ylab = "flu test", main="Flu positivity if had vaccine \n 2 years ago")
##################  Vaccines received 3 years ago  #######################

summary(vac$fv.py3)
plot(vac$flu.a ~ vac$fv.py3)
table(vac$flu.a, vac$fv.py3)     # 3 yr ago vacc -> not much change (a little less)
prop.table(table(vac$flu.a, vac$fv.py3), 2) 
plot(table(vac$fv.py3, vac$flu.a), xlab = "vacc 3 years ago", ylab = "flu test", main="Flu positivity if had vaccine \n 3 years ago")

###################################################################################
###################################################################################
####################        Start Analysis       ##################################

########## Crude VEs for flu.a  ##############

#Re-level vac1 and flu.a for yes/no for contingency table
vac$vac1 <- factor(vac$vac1, levels = c("1", "0")); levels(vac$vac1) <- c("yes", "no"); table(vac$vac1)
levels(vac$flu.a) <- c("yes", "no"); table(vac$flu.a)
table(vac$vac1, vac$flu.a)
(1-((70*1738)/(1166*186)))*100  #VE = 43.90343

data_seasonal$seasonal.vacc <- factor(data_seasonal$seasonal.vacc, levels = c("1", "0")); levels(data_seasonal$seasonal.vacc) <- c("yes", "no"); table(data_seasonal$seasonal.vacc)
table(data_seasonal$seasonal.vacc, data_seasonal$flu.a)
(1-((63*1738)/(1005*186)))*100  #VE = 41.42513

data_nasal$nasal.vacc <- factor(data_nasal$nasal.vacc, levels = c("1", "0")); levels(data_nasal$nasal.vacc) <- c("yes", "no"); table(data_nasal$nasal.vacc)
table(data_nasal$nasal.vacc, data_nasal$flu.a)
(1-((7*1738)/(161*186)))*100  #VE = 59.37354

# Crude VEs for flu.a with vaccines given at least 14 days before test
table(vac$allvac.14)
vac$allvac.14 <- factor(vac$allvac.14, levels = c("1", "0")); levels(vac$allvac.14) <- c("yes", "no"); table(vac$allvac.14)
table(vac$allvac.14, vac$flu.a)
(1-((52*2085)/(819*204)))*100  #VE = 35.10738

table(data_seasonal$seas.14)
data_seasonal$seas.14 <- factor(data_seasonal$seas.14, levels = c("1", "0")); levels(data_seasonal$seas.14) <- c("yes", "no"); table(data_seasonal$seas.14)
table(data_seasonal$seas.14, data_seasonal$flu.a)
(1-((45*2041)/(702*204)))*100  #VE = 35.86601

table(data_nasal$nasal.14)
data_nasal$nasal.14 <- factor(data_nasal$nasal.14, levels = c("1", "0")); levels(data_nasal$nasal.14) <- c("yes", "no"); table(data_nasal$nasal.14)
table(data_nasal$nasal.14, data_nasal$flu.a)
(1-((7*1782)/(117*186)))*100  #VE = 42.6799

##########################################################################################################
##############################        Vac1 modeling       ##############################################
### This modeling includes all data, (incl all age data)  #####
#Only modelling useful variables

#Re-level allvac.14 so model compares yes to no (reference being no)
vac$allvac.14 <- factor(vac$allvac.14, levels = c("no", "yes"))


z <- glm(flu.a=="yes" ~ gender, data=vac,  family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ vaccine14days, data=vac,  family = binomial)
summary(z) 

z <- glm(flu.a=="yes" ~ month, data=vac,  family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ simd5, data=vac,family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ urbanrural, data=vac, family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ no.risk.gps,data=vac,family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ clinical.riskgroup,data=vac,  family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ allvac.14, data=vac, family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ days, data=vac, family = binomial)
summary(z) 

z <- glm(flu.a=="yes" ~ waning,data=vac,family = binomial)
summary(z)

z <- glm(flu.a=="yes" ~ age,data=vac,family = binomial)
summary(z)

z <- glm(flu.a=="yes" ~ age_new,data=vac,family = binomial)
summary(z)

z <- glm(flu.a=="yes" ~ age_grp,data=vac,family = binomial)
summary(z)

###########  Best model for overall vaccine (all age data)   ################
z1 <- glm(flu.a=="yes" ~ allvac.14 + simd5 + clinical.riskgroup + days, data=vac,family = binomial)
summary(z1)      
cbind(z1$coefficients, confint.default(z1))
exp(cbind(z1$coefficients, confint.default(z1)))
(1-exp(cbind(z1$coefficients, confint.default(z1))))*100


#using waning variable instead of allvac.14
zw <- glm(flu.a=="yes" ~ waning+simd5+clinical.riskgroup+days, data=vac,family = binomial)
summary(zw)      
cbind(zw$coefficients, confint.default(zw))
exp(cbind(zw$coefficients, confint.default(zw)))
(1-exp(cbind(zw$coefficients, confint.default(zw))))*100


###############################################################################
############### Do same modeling but with data excluding age=0 ################
vac0age <- subset(vac, age!=0) ; dim(vac0age)
#Re-level allvac.14 so model compares yes to no (reference being no)
vac0age$allvac.14 <- factor(vac0age$allvac.14, levels = c("no", "yes"))
### Now model with vac0age data set  #####

z <- glm(flu.a=="yes" ~ gender, data=vac0age,  family = binomial)
summary(z)   

z <- glm(flu.a=="yes" ~ simd5, data=vac0age,family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ urbanrural, data=vac0age, family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ no.risk.gps,data=vac0age,family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ clinical.riskgroup,data=vac0age,  family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ allvac.14, data=vac0age, family = binomial)
summary(z)  

z <- glm(flu.a=="yes" ~ days, data=vac0age, family = binomial)
summary(z) 

z <- glm(flu.a=="yes" ~ waning,data=vac0age,family = binomial)
summary(z)

##################### best models with age=0 taken out ################
#Best model for overall vaccine
z1a <- glm(flu.a=="yes" ~ allvac.14+simd5+no.risk.gps+gender+days, data=vac0age,family = binomial)
summary(z1a)      
cbind(z1a$coefficients, confint.default(z1a))
exp(cbind(z1a$coefficients, confint.default(z1a)))
(1-exp(cbind(z1a$coefficients, confint.default(z1a))))*100


#using waning variable instead of allvac.14
z1a <- glm(flu.a=="yes" ~ waning+simd5+no.risk.gps+gender+days, data=vac0age,family = binomial)
summary(z1a)      
cbind(z1a$coefficients, confint.default(z1a))
exp(cbind(z1a$coefficients, confint.default(z1a)))
(1-exp(cbind(z1a$coefficients, confint.default(z1a))))*100


####################################################################################################
####################################################################################################
############################## modeling with seasonal.vacc ########################################

#Re-level seasonal.vacc so model compares yes to no (reference being no)
data_seasonal$seasonal.vacc <- factor(data_seasonal$seasonal.vacc, levels = c("no", "yes"))
table(data_seasonal$seasonal.vacc) #check no/yes

z <- glm(flu.a=="positive" ~ gender, data=data_seasonal,  family = binomial)
summary(z)   

z <- glm(flu.a=="positive" ~ simd5, data=data_seasonal,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ urbanrural, data=data_seasonal, family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ no.risk.gps,data=data_seasonal,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ clinical.riskgroup,data=data_seasonal,  family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ seasonal.vacc,data=data_seasonal, family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ days, data=data_seasonal, family = binomial)
summary(z) 

z <- glm(flu.a=="positive" ~ waning,data=data_seasonal,family = binomial)
summary(z)

#Best model for seasonal vaccine
#start with just seasonal.vacc and build up model with variables with 
#lowest p-value and see how the VE changes when you add other covariates in.
#Use just one of the clinical risk variables - whichever has lowest value

z2 <- glm(flu.a=="positive" ~ seasonal.vacc+simd5,data=data_seasonal, family = binomial)
summary(z2)
cbind(z2$coefficients, confint.default(z2))
exp(cbind(z2$coefficients, confint.default(z2)))           #This gives OR
(1-exp(cbind(z2$coefficients, confint.default(z2))))*100   #This gives VE

###############################################################################
############### Do same modeling but with data excluding age=0 ################
data_seasonal_age_n0 <- subset(data_seasonal, age!=0) ; dim(data_seasonal_age_n0)
table(data_seasonal_age_n0$seasonal.vacc) #check no/yes

### Now model with vac0age data set  #####

z <- glm(flu.a=="positive" ~ gender, data=data_seasonal_age_n0,  family = binomial)
summary(z)   

z <- glm(flu.a=="positive" ~ simd5, data=data_seasonal_age_n0,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ urbanrural, data=data_seasonal_age_n0, family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ no.risk.gps,data=data_seasonal_age_n0,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ clinical.riskgroup,data=data_seasonal_age_n0,  family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ seasonal.vacc,data=data_seasonal_age_n0, family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ days, data=data_seasonal_age_n0, family = binomial)
summary(z) 

z <- glm(flu.a=="positive" ~ waning,data=data_seasonal_age_n0,family = binomial)
summary(z)

##################### best models with age=0 taken out ################
#Again start with just seasonal.vacc and build up model with lowest p-values
#Use just one of the clinical risk variables - whichever has lowest value

#Best model for seasonal vaccine
z2a <- glm(flu.a=="positive" ~ seasonal.vacc+simd5+no.risk.gps+gender, data=data_seasonal_age_n0, family = binomial)
summary(z2a)
cbind(z2a$coefficients, confint.default(z2a))
exp(cbind(z2a$coefficients, confint.default(z2a)))
(1-exp(cbind(z2a$coefficients, confint.default(z2a))))*100


####################################################################################################
####################################################################################################
##############################        modeling with nasal.vacc     ###############################
#Re-level nasal.vacc so model compares yes to no (reference being no)
data_nasal$nasal.vacc <- factor(data_nasal$nasal.vacc, levels = c("no", "yes"))

z <- glm(flu.a=="positive" ~ gender, data=data_nasal,  family = binomial)
summary(z)   

z <- glm(flu.a=="positive" ~ simd5, data=data_nasal,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ urbanrural, data=data_nasal, family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ no.risk.gps,data=data_nasal,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ clinical.riskgroup,data=data_nasal,  family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ nasal.vacc,data=data_nasal,  family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ days, data=data_nasal, family = binomial)
summary(z) 

z <- glm(flu.a=="positive" ~ waning,data=data_nasal,family = binomial)
summary(z)

#Best model for nasal vaccine
#start with just nasal.vacc and build up model with variables with 
#lowest p-value and see how the VE changes when you add other covariates in.
#Use just one of the clinical risk variables - whichever has lowest value

z3 <- glm(flu.a=="positive" ~ nasal.vacc+simd5+clinical.riskgroup, data=data_nasal, family = binomial)
summary(z3)

cbind(z3$coefficients, confint.default(z3))
exp(cbind(z3$coefficients, confint.default(z3)))
(1-exp(cbind(z3$coefficients, confint.default(z3))))*100
###############################################################################
############### Do same modeling but with data excluding age=0 ################
data_nasal_age_n0 <- subset(data_nasal, age!=0) ; dim(data_nasal_age_n0)
data_nasal_age_n0$nasal.vacc <- factor(data_nasal_age_n0$nasal.vacc, levels = c("no", "yes"))

### Now model with vac0age data set  #####

z <- glm(flu.a=="positive" ~ gender, data=data_nasal_age_n0,  family = binomial)
summary(z)   

z <- glm(flu.a=="positive" ~ simd5, data=data_nasal_age_n0,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ urbanrural, data=data_nasal_age_n0, family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ no.risk.gps,data=data_nasal_age_n0,family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ clinical.riskgroup,data=data_nasal_age_n0,  family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ nasal.vacc,data=data_nasal_age_n0,  family = binomial)
summary(z)  

z <- glm(flu.a=="positive" ~ days, data=data_nasal_age_n0, family = binomial)
summary(z) 

z <- glm(flu.a=="positive" ~ waning,data=data_nasal_age_n0,family = binomial)
summary(z)


##################### best models with age=0 taken out ################
#Best model for nasal vaccine
#Again start with just nasal.vacc and build up model with lowest p-values
#Use just one of the clinical risk variables - whichever has lowest value

z3a <- glm(flu.a=="positive" ~ nasal.vacc+simd5+clinical.riskgroup, data=data_nasal_age_n0, family = binomial)
summary(z3a)
cbind(z3a$coefficients, confint.default(z3a))
exp(cbind(z3a$coefficients, confint.default(z3a)))
(1-exp(cbind(z3a$coefficients, confint.default(z3a))))*100


####################################################################################################
####################################################################################################
####################################################################################################
#Sample size calculation using any vaccine data
p1=186/(186+1738); p1  # % with flu in control group
p2=.7*p1; p2 # % we want in vaccine group to have VE of 30%
power.prop.test(n=NULL, p1=p1, p2=p2, sig.level=0.05, power=0.8)
# 1406.382 --> 1407 per group  
1407*2  #2814 needed in total to detect a VE of 30%.

#Sample size calculation using 'any' vaccine data but just those vaccinated >13 days before test
p1v=204/(204+2085); p1v  # % with flu in control group
p2v=.7*p1v; p2v # % we want in vaccine group to have VE of 30%
power.prop.test(n=NULL, p1=p1v, p2=p2v, sig.level=0.05, power=0.8)
#1536.329 --> 1537 per group
1537*2  #3074 needed in total to detect a VE of 30%.

#NNTs calculated using 2x2 tables
#NNT for any vaccine
#ARR is absolute risk reduction (difference between % with flu in each group)
#adjusted to incl only those vaccinated >=14days
ARR_all_ad = (204/(204+2085))-(52/(52+819)); ARR_all_ad
NNT_all_ad = 1/ARR_all_ad ; NNT_all_ad  #34

#NNT for seasonal vaccine
#adjusted to incl only those vaccinated >=14days
ARR_seas_ad = (204/(204+2085))-(45/(45+702)); ARR_seas_ad
NNT_seas_ad = 1/ARR_seas_ad ; NNT_seas_ad  #35
  
#NNT for nasal vaccine
#adjusted to incl only those vaccinated >=14days
ARR_nasal_ad = (204/(204+2085))-(7/(7+117)); ARR_nasal_ad
NNT_nasal_ad = 1/ARR_nasal_ad ; NNT_nasal_ad  #31

########## Extras
table(vac0age$flu.a,vac0age$waning)
plot(table(vac0age$waning, vac0age$flu.a))
table(vac0age$waning)

################
#Power calculation using any vaccine data
p1=186/(186+1738); p1  # % with flu in control group
p2=.7*p1; p2 # % we want in vaccine group to have VE of 30%
power.prop.test(n=256, p1=p1, p2=p2, sig.level=0.05)


