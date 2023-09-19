### Question 1 ###

# Enter data: Data_R0 is no. of secondary cases from our 20 individuals.
Data_R0 <- c(4,5,2,1,0,5,8,2,2,3,5,9,0,1,3,2,6,2,3,5)
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/hist.pdf", width=3, height=2.5, pointsize=9)
hist(Data_R0, xlab="No. of secondary infections", xlim=c(0,10), ylim=c(0, 5), breaks=11, main = "") #visualise the data
#dev.off()

# Non-parametric bootstrapping function (npbs)
npbs<-function(x,b){       
  y<-c()                   
  for(i in 1:b){
    y[i]<-median(sample(x,length(x), replace=TRUE))
  }
  y}

# Run the model for 500 bootstraps
R0_500medians<-npbs(Data_R0,500)   #500 bootstrap samples of median R0

#Plot distribution of R0 medians (with smoothing function)
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/bootstrap.pdf", width=3, height=2.5, pointsize=9)
plot(density(R0_500medians, bw=1), xlab="Median number of secondary infections", main="")
#dev.off()

#Mean of the medians
mean(R0_500medians)
#standard deviation
sd(R0_500medians)
#min & max values
min(R0_500medians); max(R0_500medians)
#Quantiles of the median R0 values
quantile(R0_500medians, probs=seq(0,1,0.025))


### Question 2 ###

##The variables:
# R0_500medians is my distribution of 500 median values of R0 from Q1
t <- 1:30   #Time in days (after outbreak)
gama <- 12  #average duration of infectiousness (days)
r <- (R0_500medians-1)/gama  #500 values of r, the growth rate of the infection
I_0 <- 20   #Initial no. of cases

#Use the parameters to calculate an array of no. of infections at different times after outbreak
I<-array(dim=c(length(t), length(r)))
for (j in 1:length(r)){  # for each of the 500 values of r
for (i in 1:length(t)){  # for each different time
I[i,j] <- I_0*exp(r[j]*t[i])
}}

# No. of Infections, I has 30 rows (time after outbreak, 1-30 days) and 500 columns (r values)
# So we have a distribution of infection nos. for each time after outbreak (rows of data)
head(I[30,], 10)      #quick check of data, day 30 
max(I)      #max no. of infections (for the y axis limit)


#y values for graph
t5  <- I[5,]   #infections at 5 days
t10 <- I[10,]  #Infections at 10 days
t15 <- I[15,]  #Infections at 15 days
t20 <- I[20,]  #Infections at 20 days
t25 <- I[25,]  #Infections at 25 days
t30 <- I[30,]  #Infections at 30 days

#x values for graph
ts5  <- rep(5, 500)
ts10 <- rep(10, 500)
ts15 <- rep(15, 500)
ts20 <- rep(20, 500)
ts25 <- rep(25, 500)
ts30 <- rep(30, 500)

# Graph of the infections
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/infections.pdf", width=3, height=2.5, pointsize=9)
plot(ts10, t10, xlim=c(0, 30), ylim = c(0, max(I)),
     xlab="No. of days after outbreak", ylab="No. of infections", col="red")
points(ts20, t20, xlim=c(0, 30), ylim = c(0, max(I)), col="red")
points(ts30, t30, xlim=c(0, 30), ylim = c(0, max(I)), col="red")
points(ts5, t5, xlim=c(0, 30), ylim = c(0, max(I)))
points(ts15, t15, xlim=c(0, 30), ylim = c(0, max(I)))
points(ts25, t25, xlim=c(0, 30), ylim = c(0, max(I)))
#dev.off()

#Summary stats for infections at 10 days
summary(t10)
quantile(t10, probs=seq(0,1,0.025)) 

#Summary stats for infections at 20 days
summary(t20)
quantile(t20, probs=seq(0,1,0.025)) 

#Summary stats for infections at 30 days
summary(t30)
quantile(t30, probs=seq(0,1,0.025)) 

# Represent no of infections for t=1:30 for first 10 r values
head(I[1,], 10)       #shows the no. of infections first 10 r values
t <- 1:30
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/predictions.1.pdf", width=7, height=2.5, pointsize=9)
par(mfrow=c(1,2))
plot(t, I[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 441000), lwd = 0.6)
for (i in 1:10){
lines(t, I[,i], lty=i+1)
  legend("topleft", inset=0.05, c("r values (1,3,6,10)", "r values (2,8)", "r value (4)", "r values (5,7)", "r value (9)"), lty = c(2,3,4,5,6), cex = 0.9)
}
plot(t, I[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 11000), lwd = 0.6)
for (i in 1:10){
  lines(t, I[,i], lty=i+1)
}
#dev.off()

#Above graph gives one extreme value of infection cases.... Why?
hist(R0_500medians) #Distribution of R0
max(R0_500medians) # Max R0 in model is 5 secondary infections
sum(ifelse(R0_500medians==5, 1, 0))/500   #6.4% cases give R0=5

### Question 3 ###

#Expert opinion of proportions needing hospital bed
minHosp <- 0.25
maxHosp <- 0.35
modal <- 0.3

#Distribution of possible proportions needing beds
prop.range <- seq(0.2,0.4,0.001)
a <- 250
calc.b <- function(a, mode){
  b <- 2-a+(a-1)/mode
  print(b)
}
calc.b(a=a, mode=modal)
b <- 582
prop.Hosp <- dbeta(prop.range, a, b) #Distribution of values for proportion that need hospital

#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/HospProp.pdf", width=3, height=2.5, pointsize=9)
plot(prop.range, prop.Hosp, xlab="Proportion needing hospitalised", type="l", ylab="Density")
#dev.off()

#Using the median no. of infections/cases at 30 days, [median(t30) = 2968 = n], 
#generate random variables from binomial distribution with p=prop.Hosp in (binom(n,p))
#to simulate the no. of beds required in hospital.

#Function to generate values from binomial dist for no. of beds required
n <- 2968       #median no. of infections at 30 days
#n1 is no. of values sampled from beta dist (uncertainty)
#n2 is no. of values sampled from binomial dist (variability)

simul <- function(n1, n2){
  y <- array(dim=c(n1, n2));
for(i in 1:n1){
  p <- rbeta(1,a,b);     #Generate 1 value from our beta distribution (a=250, b=582)
  for (j in 1:n2){
    y[i,j] <- rbinom(1, n, p) 
  }
}
y
}

#Run function for no. of beds required
beds <- simul(10,1000) 

min(beds)   #check for min for x-axis
max(beds)   #check for max for x-axis

#Plot distributions
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/HospCases.pdf", width=6, height=2.5, pointsize=9)
par(mfrow=c(1,2))
plot(ecdf(beds[1,]), do.p=FALSE, verticals=TRUE, main="",
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(min(beds), max(beds)))
for (i in 2:10) 
  lines(ecdf(beds[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)
plot(ecdf(beds[1,]), do.p=FALSE, verticals=TRUE, main="",
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(min(beds), 2500))
for (i in 2:10) 
  lines(ecdf(beds[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)
#dev.off()

#Probability of threshold being exceeded
cumulprob1 <- ecdf(beds[1,])
1-cumulprob1(2400) 
cumulprob2 <- ecdf(beds[2,])
1-cumulprob2(2400)
cumulprob3 <- ecdf(beds[3,])
1-cumulprob3(2400) 
cumulprob4 <- ecdf(beds[4,])
1-cumulprob4(2400) 
cumulprob5 <- ecdf(beds[5,])
1-cumulprob5(2400) 
cumulprob6 <- ecdf(beds[6,])
1-cumulprob6(2400)
cumulprob7 <- ecdf(beds[7,])
1-cumulprob7(2400) 
cumulprob8 <- ecdf(beds[8,])
1-cumulprob8(2400) 
cumulprob9 <- ecdf(beds[9,])
1-cumulprob9(2400) 
cumulprob10 <- ecdf(beds[10,])
1-cumulprob10(2400) 

### Question 4 ###
#### Use MEAN bootstrapping ####

# Non-parametric bootstrapping function (npbs2)
npbs2<-function(x,b){       
  y<-c()                   
  for(i in 1:b){
    y[i]<-mean(sample(x,length(x), replace=TRUE))
  }
  y}

# Run the model for 500 bootstraps
R0_500means<-npbs2(Data_R0,500)   #500 bootstrap samples of mean R0

#Plot distribution of R0 means (with smoothing function)
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/bootstrap2.pdf", width=3, height=2.5, pointsize=9)
plot(density(R0_500means, bw=1), xlab="Mean number of secondary infections", main="")
#dev.off()

#Mean of the means
mean(R0_500means)
#standard deviation
sd(R0_500means)
#min & max values
min(R0_500means); max(R0_500means)
#Quantiles of the mean R0 values
quantile(R0_500means, probs=seq(0,1,0.025))


### (Q. 2) ###

##The variables:
# R0_500means is my distribution of 500 mean values of R0 from Q1
t <- 1:30   #Time in days (after outbreak)
gama <- 12  #average duration of infectiousness (days)
gama7 <- 7
gama10 <- 10
r2 <- (R0_500means-1)/gama  #500 values of r, the growth rate of the infection
# r for different gammas
r_gama7 <- (R0_500means-1)/gama7
r_gama10 <- (R0_500means-1)/gama10

I_0 <- 20   #Initial no. of cases

#Use the parameters to calculate an array of no. of infections at different times after outbreak
I2<-array(dim=c(length(t), length(r)))
for (j in 1:length(r2)){  # for each of the 500 values of r
  for (i in 1:length(t)){  # for each different time
    I2[i,j] <- I_0*exp(r2[j]*t[i])
  }}
# I for gamma 7
Ig7<-array(dim=c(length(t), length(r_gama7)))
for (j in 1:length(r_gama7)){  # for each of the 500 values of r
  for (i in 1:length(t)){  # for each different time
    Ig7[i,j] <- I_0*exp(r_gama7[j]*t[i])
  }}

# I for gamma 10
Ig10<-array(dim=c(length(t), length(r_gama10)))
for (j in 1:length(r_gama10)){  # for each of the 500 values of r
  for (i in 1:length(t)){  # for each different time
    Ig10[i,j] <- I_0*exp(r_gama10[j]*t[i])
  }}

# No. of Infections, I has 30 rows (time after outbreak, 1-30 days) and 500 columns (r values)
# So we have a distribution of infection nos. for each time after outbreak (rows of data)
head(I2[30,], 10)      #quick check of data, day 30 
max(I2)      #max no. of infections (for the y axis limit)


#y values for graph
t5m  <- I2[5,]   #infections at 5 days
t10m <- I2[10,]  #Infections at 10 days
t15m <- I2[15,]  #Infections at 15 days
t20m <- I2[20,]  #Infections at 20 days
t25m <- I2[25,]  #Infections at 25 days
t30m <- I2[30,]  #Infections at 30 days

#y for gamma 7
t5g7  <- Ig7[5,]   #infections at 5 days
t10g7 <- Ig7[10,]  #Infections at 10 days
t15g7 <- Ig7[15,]  #Infections at 15 days
t20g7 <- Ig7[20,]  #Infections at 20 days
t25g7 <- Ig7[25,]  #Infections at 25 days
t30g7 <- Ig7[30,]  #Infections at 30 days

#y for gamma 10
t5g10  <- Ig10[5,]   #infections at 5 days
t10g10 <- Ig10[10,]  #Infections at 10 days
t15g10 <- Ig10[15,]  #Infections at 15 days
t20g10 <- Ig10[20,]  #Infections at 20 days
t25g10 <- Ig10[25,]  #Infections at 25 days
t30g10 <- Ig10[30,]  #Infections at 30 days

#x values for graph
ts5  <- rep(5, 500)
ts10 <- rep(10, 500)
ts15 <- rep(15, 500)
ts20 <- rep(20, 500)
ts25 <- rep(25, 500)
ts30 <- rep(30, 500)

# Graph of the infections
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/infections2.pdf", width=3, height=3, pointsize=9)
plot(ts10, t10m, xlim=c(0, 30), ylim = c(0, max(I2)),
     xlab="No. of days after outbreak", ylab="No. of infections", col="red")
points(ts20, t20m, xlim=c(0, 30), ylim = c(0, max(I2)), col="red")
points(ts30, t30m, xlim=c(0, 30), ylim = c(0, max(I2)), col="red")
points(ts5, t5m, xlim=c(0, 30), ylim = c(0, max(I2)))
points(ts15, t15m, xlim=c(0, 30), ylim = c(0, max(I2)))
points(ts25, t25m, xlim=c(0, 30), ylim = c(0, max(I2)))
#dev.off()

#gamma plots
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/gammas.pdf", width=6.5, height=2, pointsize=9)
par(mfrow=c(1,3))
plot(ts10, t10g7, xlim=c(0, 30), ylim = c(0, max(Ig10)),
     xlab="No. of days after outbreak", ylab="No. of infections", col="red", main="gamma=7 days")
points(ts20, t20g7, xlim=c(0, 30), ylim = c(0, max(Ig10)), col="red")
points(ts30, t30g7, xlim=c(0, 30), ylim = c(0, max(Ig10)), col="red")
points(ts5, t5g7, xlim=c(0, 30), ylim = c(0, max(Ig10)))
points(ts15, t15g7, xlim=c(0, 30), ylim = c(0, max(Ig10)))
points(ts25, t25g7, xlim=c(0, 30), ylim = c(0, max(Ig10)))

plot(ts10, t10g10, xlim=c(0, 30), ylim = c(0, max(Ig10)),
     xlab="No. of days after outbreak", ylab="No. of infections", col="red", main="gamma=10 days")
points(ts20, t20g10, xlim=c(0, 30), ylim = c(0, max(Ig10)), col="red")
points(ts30, t30g10, xlim=c(0, 30), ylim = c(0, max(Ig10)), col="red")
points(ts5, t5g10, xlim=c(0, 30), ylim = c(0, max(Ig10)))
points(ts15, t15g10, xlim=c(0, 30), ylim = c(0, max(Ig10)))
points(ts25, t25g10, xlim=c(0, 30), ylim = c(0, max(Ig10)))

plot(ts10, t10m, xlim=c(0, 30), ylim = c(0, max(Ig10)),
     xlab="No. of days after outbreak", ylab="No. of infections", col="red", main="gamma=12 days")
points(ts20, t20m, xlim=c(0, 30), ylim = c(0, max(Ig10)), col="red")
points(ts30, t30m, xlim=c(0, 30), ylim = c(0, max(Ig10)), col="red")
points(ts5, t5m, xlim=c(0, 30), ylim = c(0, max(Ig10)))
points(ts15, t15m, xlim=c(0, 30), ylim = c(0, max(Ig10)))
points(ts25, t25m, xlim=c(0, 30), ylim = c(0, max(Ig10)))
#dev.off()

#Summary stats for infections at 10 days
summary(t10m)
quantile(t10m, probs=seq(0,1,0.025)) 

#Summary stats for infections at 20 days
summary(t20m)
quantile(t20m, probs=seq(0,1,0.025)) 

#Summary stats for infections at 30 days
summary(t30m)
quantile(t30m, probs=seq(0,1,0.025)) 

# for gamma 7
summary(t10g7)
quantile(t10g7, probs=seq(0,1,0.025)) 
summary(t15g7)
summary(t20g7)
quantile(t20g7, probs=seq(0,1,0.025)) 
summary(t30g7)
quantile(t30g7, probs=seq(0,1,0.025)) 

# for gamma 10
summary(t10g10)
quantile(t10g10, probs=seq(0,1,0.025)) 
summary(t20g10)
quantile(t20g10, probs=seq(0,1,0.025)) 
summary(t30g10)
quantile(t30g10, probs=seq(0,1,0.025)) 

# Represent no of infections for t=1:30 for first 10 r values
head(I2[30,], 10)       #shows the no. of infections at t=30 first 10 r values
t <- 1:30
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/predictions2.pdf", width=7, height=3, pointsize=9)
par(mfrow=c(1,2))
plot(t, I2[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 52650))
for (i in 1:10){
  lines(t, I2[,i], lty=i+1)
#  legend("topleft", c("", "", "", "", ""), lty = i+1)
}
plot(t, I2[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 12000))
for (i in 1:10){
  lines(t, I2[,i], lty=i+1)
}
#dev.off()

### for gamma 7
head(Ig7[30,], 10)       #shows the no. of infections at t=30 first 10 r values
t <- 1:30

par(mfrow=c(1,2))
plot(t, Ig7[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 14588327.40))
for (i in 1:10){
  lines(t, Ig7[,i], lty=i+1)
}
plot(t, Ig7[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 12000))
for (i in 1:10){
  lines(t, Ig7[,i], lty=i+1)
}

### for gamma 10
head(Ig10[30,], 10)       #shows the no. of infections at t=30 first 10 r values
t <- 1:30

par(mfrow=c(1,2))
plot(t, Ig10[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 17081.1753))
for (i in 1:10){
  lines(t, Ig10[,i], lty=i+1)
}
plot(t, Ig10[,1], type="n", 
     xlab="Time after outbreak (days)", ylab="No. of infected cases", ylim=c(0, 12000))
for (i in 1:10){
  lines(t, Ig10[,i], lty=i+1)
}

### Q. 3 ###

#Expert opinion of proportions needing hospital bed
minHosp2 <- 0.15
maxHosp2 <- 0.25
modal2 <- 0.2

#Distribution of possible proportions needing beds
prop.range <- seq(0.1,0.3,0.001)
a2 <- 250
calc.b <- function(a2, mode){
  b <- 2-a2+(a2-1)/mode
  print(b)
}
calc.b(a=a2, mode=modal2)
b2 <- 997
prop.Hosp2 <- dbeta(prop.range, a2, b2) #Distribution of values for proportion that need hospital

#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/HospProp2.pdf", width=3, height=3, pointsize=9)
plot(prop.range, prop.Hosp, xlab="Proportion needing hospitalised", type="l", ylab="Density")
#dev.off()

#Using the median no. of infections/cases at 30 days, [median(t30) = 8068 = n], 
#generate random variables from binomial distribution with p=prop.Hosp in (binom(n,p))
#to simulate the no. of beds required in hospital.

#Function to generate values from binomial dist for no. of beds required
n <- 8068       #median no. of infections at 30 days -----> change this as appropriate
#n1 is no. of values sampled from beta dist (uncertainty)
#n2 is no. of values sampled from binomial dist (variability)

simul <- function(n1, n2){
  y <- array(dim=c(n1, n2));
  for(i in 1:n1){
    p <- rbeta(1,a,b);     #Generate 1 value from our beta distribution (a=250, b=582)
    for (j in 1:n2){
      y[i,j] <- rbinom(1, n, p) 
    }
  }
  y
}

# For 2nd beta distribution for proportion being hospitalised
simul2 <- function(n1, n2){
  y <- array(dim=c(n1, n2));
  for(i in 1:n1){
    p <- rbeta(1,a2,b2);     #Generate 1 value from our beta distribution (a2=250, b2=997)
    for (j in 1:n2){
      y[i,j] <- rbinom(1, n, p) 
    }
  }
  y
}

#Run function for no. of beds required
beds2 <- simul(10,1000) 
bedsg7 <- simul(10, 1000)
bedsg10 <- simul(10, 1000)

beds12b2 <- simul2(10,1000) #beds required: gamma=12, 2nd beta distribution
beds10b2 <- simul2(10,1000) #beds required: gamma=10, 2nd beta distribution
beds7b2 <- simul2(10,1000) #beds required: gamma=7, 2nd beta distribution

min(beds2)   #check for min for x-axis
median(beds2)#median no. of beds required
max(beds2)   #check for max for x-axis
min(bedsg7)
median(bedsg7)
max(bedsg7)
min(bedsg10)
median(bedsg10)
max(bedsg10)

#2nd beta distribution data
min(beds12b2)
max(beds12b2)
median(beds12b2)
min(beds10b2)
max(beds10b2)
median(beds10b2)
min(beds7b2)
max(beds7b2)
median(beds7b2)

#Plot distributions
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/HospCases2.pdf", width=4, height=2.5, pointsize=9)
par(mfrow=c(1,1))
plot(ecdf(beds2[1,]), do.p=FALSE, verticals=TRUE, main="",
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(min(beds2), max(beds2)))
for (i in 2:10) 
  lines(ecdf(beds2[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)
#plot(ecdf(beds2[1,]), do.p=FALSE, verticals=TRUE, main="",
#     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(min(beds2), 2500))
#for (i in 2:10) 
#  lines(ecdf(beds2[i,]), verticals=TRUE, do.p=FALSE)
#abline(v = 2400, col = "red", lty=2)
#dev.off()

#gammas
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/HospCasesGammas.pdf", width=6, height=2, pointsize=9)
par(mfrow=c(1,3))
plot(ecdf(bedsg7[1,]), do.p=FALSE, verticals=TRUE,
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(0, max(bedsg7)), main="gamma=7 days")
for (i in 2:10) 
  lines(ecdf(bedsg7[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)

plot(ecdf(bedsg10[1,]), do.p=FALSE, verticals=TRUE,
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(2400, max(bedsg10)), main="gamma=10 days")
for (i in 2:10) 
  lines(ecdf(bedsg10[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)

plot(ecdf(beds2[1,]), do.p=FALSE, verticals=TRUE,
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(min(beds2), max(beds2)), main="gamma=12 days")
for (i in 2:10) 
  lines(ecdf(beds2[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)
#dev.off()

#gammas and 2nd beta distriburtion
#pdf(file = "/Users/sarah/Desktop/MSc\ Applied\ Statistics\ in\ Health\ Sciences/MM925/Assessment/HospCasesGammasBeta2.pdf", width=6, height=2, pointsize=9)
par(mfrow=c(1,3))
plot(ecdf(beds7b2[1,]), do.p=FALSE, verticals=TRUE,
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(0, max(beds7b2)), main="gamma=7 days")
for (i in 2:10) 
  lines(ecdf(beds7b2[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)

plot(ecdf(beds10b2[1,]), do.p=FALSE, verticals=TRUE,
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(2400, max(beds10b2)), main="gamma=10 days")
for (i in 2:10) 
  lines(ecdf(beds10b2[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)

plot(ecdf(beds12b2[1,]), do.p=FALSE, verticals=TRUE,
     xlab="No. of hospital beds required", ylab="Empirical Cumulative Density", lty=1, xlim=c(min(beds12b2), 2400), main="gamma=12 days")
for (i in 2:10) 
  lines(ecdf(beds12b2[i,]), verticals=TRUE, do.p=FALSE)
abline(v = 2400, col = "red", lty=2)
#dev.off()
###

#Probability of threshold being exceeded
cumulprob1 <- ecdf(beds2[1,])
1-cumulprob1(2400) 
cumulprob2 <- ecdf(beds2[2,])
1-cumulprob2(2400)
cumulprob3 <- ecdf(beds2[3,])
1-cumulprob3(2400) 
cumulprob4 <- ecdf(beds2[4,])
1-cumulprob4(2400) 
cumulprob5 <- ecdf(beds2[5,])
1-cumulprob5(2400) 
cumulprob6 <- ecdf(beds2[6,])
1-cumulprob6(2400)
cumulprob7 <- ecdf(beds2[7,])
1-cumulprob7(2400) 
cumulprob8 <- ecdf(beds2[8,])
1-cumulprob8(2400) 
cumulprob9 <- ecdf(beds2[9,])
1-cumulprob9(2400) 
cumulprob10 <- ecdf(beds2[10,])
1-cumulprob10(2400) 

## Probabilities for beds10b2
cumulprob1 <- ecdf(beds10b2[1,])
1-cumulprob1(6000) 
cumulprob2 <- ecdf(beds10b2[2,])
1-cumulprob2(6000)
cumulprob3 <- ecdf(beds10b2[3,])
1-cumulprob3(6000) 
cumulprob4 <- ecdf(beds10b2[4,])
1-cumulprob4(6000) 
cumulprob5 <- ecdf(beds10b2[5,])
1-cumulprob5(6000) 
cumulprob6 <- ecdf(beds10b2[6,])
1-cumulprob6(6000)
cumulprob7 <- ecdf(beds10b2[7,])
1-cumulprob7(6000) 
cumulprob8 <- ecdf(beds10b2[8,])
1-cumulprob8(6000) 
cumulprob9 <- ecdf(beds10b2[9,])
1-cumulprob9(6000) 
cumulprob10 <- ecdf(beds10b2[10,])
1-cumulprob10(6000) 

## Probabilities for beds7b2
cumulprob1 <- ecdf(beds7b2[1,])
1-cumulprob1(120000) 
cumulprob2 <- ecdf(beds7b2[2,])
1-cumulprob2(120000)
cumulprob3 <- ecdf(beds7b2[3,])
1-cumulprob3(120000) 
cumulprob4 <- ecdf(beds7b2[4,])
1-cumulprob4(120000) 
cumulprob5 <- ecdf(beds7b2[5,])
1-cumulprob5(120000) 
cumulprob6 <- ecdf(beds7b2[6,])
1-cumulprob6(120000)
cumulprob7 <- ecdf(beds7b2[7,])
1-cumulprob7(120000) 
cumulprob8 <- ecdf(beds7b2[8,])
1-cumulprob8(120000) 
cumulprob9 <- ecdf(beds7b2[9,])
1-cumulprob9(120000) 
cumulprob10 <- ecdf(beds7b2[10,])
1-cumulprob10(120000) 

# Get summary stats for probability of exceeding threshold
z = c(0.842, 1, 0.932, 0.994, 0.454, 0.995, 0, 0.682, 0.676, 0.77)
summary(z)
quantile(z, probs=seq(0,1,0.025)) #Quantiles for the 10 graphed distributions of hospitalisation cases

z2 = c(0,0,0,0,0,0,0.272, 1,1,1)
mean(z2)

z3 = c(.991, .995, 1,0,1,1,0.968, .998, 1,1)
mean(z3)

z4 = c(0,0,1,0,0,1,0,0,1,0.66)
mean(z4)
