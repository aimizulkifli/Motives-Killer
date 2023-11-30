setwd("/Users/amy/Documents/MATH5741M/Coursework/R")
load("killersandmotives.Rdata")
createsample(201669666)
save(mysample, file = "mysample.RData")  #it saved in same folder as killersmotives
mysample[1:10,]

# DATA CLEANING
table(mysample$Motive) # enjoyment power 717, avoid arrest 22, vigilante justice 64 (803). Actual data 809. 
table(mysample$Sentence) # total 776 , actual 809
table(mysample$InsanityPlea) # total 790, actual 809
y <- ((mysample$AgeLastKill)-(mysample$AgeFirstKill))  # CareerDuration formula
mysample$CareerDuration <- y  # create new variable CareerDuration
mysample[1:5,]
mysample <- mysample[!is.na(mysample$Motive),]  # remove NA in motive column - 803 data left
mysample <- mysample[!is.na(mysample$Sentence),] # remove NA in sentence column - 770 data left
mysample <- mysample[!is.na(mysample$InsanityPlea),] # remove NA in insanity plea - 759 data left
is.na(mysample$Motive)  # check if there is any NA in Motive
mysample <- mysample[mysample$AgeFirstKill != 99999,] # remove age first kill 99999 - 750 data left
mysample <- mysample[mysample$AgeLastKill != 99999,] # no data removed
max(mysample$CareerDuration) #max duration is 35 years
min(mysample$CareerDuration) #min duration is -1 which is wrong. Need to remove
min(mysample$AgeFirstKill) # 10
max(mysample$AgeLastKill) # 66 - possible max difference is 56 years (career duration), so 35 years duration is acceptable
mysample <- mysample[mysample$CareerDuration >= 0,] #removed negative career duration - 749 data left
na.omit(mysample) # can use to remove NA data from whole dataset in one go, but no more data removed now - 749 data left
mysample <- mysample[(mysample$YearBorn + mysample$AgeFirstKill) >= 1900,] # remove first killed before the year 1900 - 743 data left
x <- ((mysample$YearBorn) + (mysample$AgeFirstKill))
mysample$YearFirstKill <- x
mysample

# DATA EXPLORATION
str(mysample) # to check data type of each variable
table(mysample$Sex) # male 725 female 18
table(mysample$Race) # white 458 black 241 hisp 31 asian 6 american 6 other 0
table(mysample$CareerDuration)
# find the typical career duration by using mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(mysample$CareerDuration) # 0 year - 222 pax
# find year mostly first kill happen
getmode(mysample$YearFirstKill) # 1986 - 39 pax
table(mysample$YearFirstKill)
table(mysample$Sex, mysample$CareerDuration)
mysample$Adult <- (mysample$AgeFirstKill >= 18)
mysample$Adult <- factor(mysample$Adult, 
                        levels = c(TRUE, FALSE), 
                        labels = c("Adult", "Child"))
boxplot(AgeLastKill ~ Adult, data = mysample, xlab = "")
q <- c(mean(adult),mean(child))
points(q, col = "red", pch = 19)
adult <- mysample[mysample$Adult == 'Adult',"AgeLastKill"]
child <- mysample[mysample$Adult == 'Child',"AgeLastKill"]
mean(adult) #35.49654
sd(adult) #9.39844
mean(child) #23.45
sd(child) #9.069759
table(mysample$Motive)
enjoyment <- mysample[mysample$Motive == 'Enjoyment or power',"AgeFirstKill"] # categorical variable - motive & first kill
escape <- mysample[mysample$Motive == 'Escape or avoid arrest',"AgeFirstKill"]
revenge <- mysample[mysample$Motive == 'Revenge or vigilante justice',"AgeFirstKill"]
sd(enjoyment) # 8.42691
sd(escape) # 6.194012 - the least spread
sd(revenge) # 9.870815
mean(enjoyment) # 30.57186
mean(escape) # 32.95 - the highest average
mean(revenge) # 30.41818
x <- c(mean(escape),mean(revenge),mean(enjoyment))
var(enjoyment) #71.012
var(escape) #38.366
var(revenge) #97.433
boxplot(mysample$AgeFirstKill ~ mysample$Motive, data = mysample)
boxplot(escape, revenge, enjoyment,
        names = c("Escape/Avoid Arrest", "Revenge/Vigilante Justice", "Enjoyment/Power"),
        xlab = "Killing Motives",
        ylab = "Age During First Kill"
)
points(x, col = "red", pch = 19)
hist(enjoyment) # look like normal dist
hist(escape) # look like normal dist
hist(revenge) # have 2 peaks
hist(revenge, breaks = seq(from = 0, to = 60, by = 10)) # after adjusting the interval, it look like normal dist

notplea <- mysample[mysample$InsanityPlea == "Didn't plead","AgeLastKill"]
successful <- mysample[mysample$InsanityPlea == "Successful","AgeLastKill"]
unsuccessful <- mysample[mysample$InsanityPlea == "Unsuccessful","AgeLastKill"]
mean(mysample$CareerDuration) # 4.548 years
var(mysample$CareerDuration) # 38.995
sd(mysample$CareerDuration) # 6.245 years
IQR(mysample$CareerDuration)# 7
summary(mysample$CareerDuration)
# Min.   1st Qu.  Median  Mean   3rd Qu.  Max. 
# 0.000   0.000   2.000   4.548   7.000  35.000 
mean(notplea) #35.308
var(notplea) #90.061
sd(notplea) #9.49
IQR(notplea) #13
summary(notplea)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.00   28.00   34.00   35.31   41.00   66.00 
quantile(notplea, type = 1)
mean(successful) #39.2
var(successful) #100.7
sd(successful) #10.03
IQR(successful) # 13
quantile(successful, type = 1)
# 0%  25%  50%  75% 100% 
#  28   30   43   43   52 
mean(unsuccessful) #33.07
var(unsuccessful) #111.6171
sd(unsuccessful)#10.57
IQR(unsuccessful) #11.25
quantile(unsuccessful, type = 1)
# 0%  25%  50%  75% 100% 
# 18   26   31   38   65 
install.packages("moments")
library(moments)
skewness(unsuccessful) #1.009
skewness(successful) #0.00643
skewness(notplea) #0.5876
skewness(escape) #0.384
skewness(revenge) #0.7304
skewness(enjoyment) #0.812
skewness(mysample$CareerDuration) #1.924
table(mysample$InsanityPlea)

hist(enjoyment, freq = FALSE) # look like normal dist
hist(escape, freq = FALSE) # almost look like normal dist
hist(revenge) # have 2 peaks
hist(revenge, breaks = seq(from = 0, to = 60, by = 10), freq = FALSE) # after adjusting the interval, it look like normal dist

par(mfrow = c(1, 3))
# but the scales are different, so need to standardize the limit
hist(enjoyment, freq = FALSE,
     main = "",
     xlab = "Enjoyment/Power",
     xlim = c(0, 70), 
     ylim = c(0, 0.07))
hist(escape, freq = FALSE,  
     main = "",
     xlab = "Escape/Avoid Arrest",
     xlim = c(0, 70), 
     ylim = c(0, 0.07))
hist(revenge, freq = FALSE,  
     main = "",
     xlab = "Revenge/Vigilante Justice",
     xlim = c(0, 70), 
     ylim = c(0, 0.07))

# MODELLING & ESTIMATION
# general function for exponential dist (lambda = 1/sample mean)
f_exp <- function(x, lambda){
  return( lambda*exp(-lambda*x) )
}
# general function for normal dist
f_normal <- function(x, mu, sigma){
  return(  exp( -0.5*(x- mu)^2 / sigma^2 ) / sqrt(2*pi*sigma^2)  )   
}
#general function for poisonn dist
f_po <- function(x, lambda){
  return( (lambda^x*exp(-lambda)) / factorial(x))
}

par(mfrow = c(1, 2))
par(mfrow = c(1, 1))

# NORMAL DISTRIBUTION MODELLING
#enjoyment:symmetric normal
hist(enjoyment, freq = FALSE,
     main = "",
     xlab = "Enjoyment/Power",
     xlim = c(0, 70), 
     ylim = c(0, 0.05))
x <- seq(from = min(enjoyment), to = max(enjoyment), by = 0.1)
lines(x, dnorm(x, mean = 30.57, sd = 8.43), lwd = 2, col = "blue")
plot(x, f_normal(x, mu = 30.57, sigma = 8.43), type = 'l', lwd = 2, col = "blue", ylab = "f(x)")
#plot(x, dnorm(x, mean = 30.57, sd = 8.43), type = "l", col = "red")

#revenge:symmetric normal
hist(revenge, freq = FALSE,  
     main = "",
     xlab = "Revenge/Vigilante Justice",
     xlim = c(0, 60), 
     ylim = c(0, 0.05),
     breaks = seq(from = 0, to = 60, by = 10))
x <- seq(from = min(revenge), to = max(revenge), by = 0.1)
lines(x, dnorm(x, mean = 30.42, sd = 9.87), lwd = 2, col = "blue")
plot(x, f_normal(x, mu = 30.42, sigma = 9.87), type = 'l', lwd = 2, col = "blue", ylab = "f(x)")

#escape:symmetric normal, bit skew to left
hist(escape, freq = FALSE,  
     main = "",
     xlab = "Escape/Avoid Arrest",
     xlim = c(0, 50), 
     ylim = c(0, 0.07))
x <- seq(from = min(escape), to = max(escape), by = 0.1)
lines(x, dnorm(x, mean = 32.95, sd = 6.19), lwd = 2, col = "blue")
plot(x, f_normal(x, mu = 32.95, sigma = 6.19), type = 'l', lwd = 2, col = "blue", ylab = "f(x)")

# EXPONENTIAL DISTRIBUTION MODELLING
#enjoyment: does not work
hist(enjoyment, freq = FALSE,
     main = "",
     xlab = "Enjoyment/Power",
     xlim = c(0, 70), 
     ylim = c(0, 0.05))
x <- seq(from = min(enjoyment), to = max(enjoyment), by = 0.1)
plot(x, f_exp(x, lambda = 1/30.57), type="l", lwd=2)
lines(x, f_exp(x, lambda = 1/30.57), lwd=2, col="blue")
lines(x, dexp(x, rate = 1/30.57), lwd = 2, col = "darkgreen")

#revenge:does not work
hist(revenge, freq = FALSE,  
     main = "",
     xlab = "Revenge/Vigilante Justice",
     xlim = c(0, 60), 
     ylim = c(0, 0.05))
x <- seq(from = min(revenge), to = max(revenge), by = 0.1)
lines(x, f_exp(x, lambda = 1/30.42), lwd=2, col="blue")

#escape:does not work
hist(escape, freq = FALSE,  
     main = "",
     xlab = "Escape/Avoid Arrest",
     xlim = c(0, 50), 
     ylim = c(0, 0.07))
x <- seq(from = min(escape), to = max(escape), by = 0.1)
lines(x, f_exp(x, lambda = 1/32.95), lwd=2, col="blue")
lines(x, dexp(x, rate = 1/32.95), lwd = 2, col = "darkgreen")

# POISONN DISTRIBUTION
#enjoyment:does not work
hist(enjoyment, freq = FALSE,
     main = "",
     xlab = "Enjoyment/Power",
     xlim = c(0, 70), 
     ylim = c(0, 0.05))
x <- seq(from = min(enjoyment), to = max(enjoyment), by = 0.1)
plot(x, f_po(x, lambda = 1/30.57), type="l", lwd=2, col="blue")
lines(x, f_po(x, lambda = 1/30.57), lwd=2, col="blue")
lines(x, dexp(x, rate = 1/30.57), lwd = 2, col = "darkgreen")

#revenge:does not work
hist(revenge, freq = FALSE,  
     main = "",
     xlab = "Revenge/Vigilante Justice",
     xlim = c(0, 60), 
     ylim = c(0, 0.05))
x <- seq(from = min(revenge), to = max(revenge), by = 0.1)
lines(x, f_po(x, lambda = 1/30.42), lwd=2, col="blue")

#escape:does not work
hist(escape, freq = FALSE,  
     main = "",
     xlab = "Escape/Avoid Arrest",
     xlim = c(0, 50), 
     ylim = c(0, 0.07))
x <- seq(from = min(escape), to = max(escape), by = 0.1)
lines(x, f_po(x, lambda = 3), lwd=2, col="blue")

#check the distribution:
# H0: our sample comes from a normal distribution
# H1: our sample does not come from a normal distribution

# 1. CDF vs Normal Distribution : F(x) = G(x)?
# Enjoyment:
Fn <- ecdf(enjoyment)
Fn(40) #0.8727545
mu <- mean(enjoyment)
sigma <- sd(enjoyment)
G <- function(x){
  return(pnorm(x, mean = mu, sd = sigma))
}
G(40) #0.8683902
plot(Fn, verticals = TRUE, pch = NA, main = "CDF of Enjoyment/Power",ylab="F(x)" )
x <- seq(from = min(enjoyment), to = max(enjoyment), by = 0.1)
lines(x, G(x), col = "red", lwd=2)
#The vertical distances between the two curves generally appear to be quite small, 
#suggesting close agreement between the sample CDF and the normal CDF.

# Revenge:
Fn <- ecdf(revenge)
Fn(40) #0.7818182
mu <- mean(revenge)
sigma <- sd(revenge)
G <- function(x){
  return(pnorm(x, mean = mu, sd = sigma))
}
G(40) #0.8341566 - not really same
plot(Fn, verticals = TRUE, pch = NA, main = "CDF of Revenge/Vigilante Justice",ylab="F(x)" )
x <- seq(from = min(revenge), to = max(revenge), by = 0.1)
lines(x, G(x), col = "red", lwd=2)
#The vertical distances between the two curves generally appear to be not really small, 
#but we will keep suggesting close agreement between the sample CDF and the normal CDF for this time.

# Escape:
Fn <- ecdf(escape)
Fn(40) #0.85
mu <- mean(escape)
sigma <- sd(escape)
G <- function(x){
  return(pnorm(x, mean = mu, sd = sigma))
}
G(40) # 0.8724807 - almost same
plot(Fn, verticals = TRUE, pch = NA, main = "CDF of Escape/Avoid Arrest",ylab="F(x)" )
x <- seq(from = min(escape), to = max(escape), by = 0.1)
lines(x, G(x), col = "red", lwd=2)

# 1.1 Kolmogorov-Smirnov test - H0: F(x)=G(x)
# Enjoyment:
mu <- mean(enjoyment)
sigma <- sd(enjoyment)
ks.test(x =  enjoyment, 
        y = "pnorm", 
        mean = mu, sd = sigma)
#data:  enjoyment
#D = 0.075548, p-value = 0.0009759
#alternative hypothesis: two-sided
#The results suggest that enjoyment (which produced a small p-value) is not normally distributed.


# Revenge:
mu <- mean(revenge)
sigma <- sd(revenge)
ks.test(x =  revenge, 
        y = "pnorm", 
        mean = mu, sd = sigma)
#data:  revenge
#D = 0.12732, p-value = 0.3346
#alternative hypothesis: two-sided
#Because p>5%, the distance between the two CDFs is not considered to be improbably large. 
#The results suggest that revenge (which produced a large p-value) is normally distributed.

# Escape:
mu <- mean(escape)
sigma <- sd(escape)
ks.test(x =  escape, 
        y = "pnorm", 
        mean = mu, sd = sigma)
#data:  escape
#D = 0.13817, p-value = 0.8397
#alternative hypothesis: two-sided
#Because p>5%, the distance between the two CDFs is not considered to be improbably large. 
#The results suggest that escape (which produced a large p-value) is normally distributed.

# 2. QQ-Plot : 
# Enjoyment
mu <- mean(enjoyment)
sigma <- sd(enjoyment)
qqnorm(enjoyment, main="Enjoyment/Power")
abline(a = mu, b = sigma, col = "red")
#the points lie roughly in a straight line then it suggests our sample comes from some normal distribution
shapiro.test(enjoyment)
#data:  enjoyment
#W = 0.96302, p-value = 6.221e-12
#The results suggest that enjoyment (which produced a small p-value) is not normally distributed.

# Revenge
mu <- mean(revenge)
sigma <- sd(revenge)
qqnorm(revenge, main="Revenge/Vigilante Justice")
abline(a = mu, b = sigma, col = "red")
#the points lie roughly in a straight line then it suggests our sample comes from some normal distribution
shapiro.test(revenge)
#data:  revenge
#W = 0.94617, p-value = 0.01564
#The results suggest that enjoyment (which produced a small p-value) is not normally distributed.

# Escape
mu <- mean(escape)
sigma <- sd(escape)
qqnorm(revenge, main="Escape/Avoid Arrest")
abline(a = mu, b = sigma, col = "red")
#the points lie roughly in a straight line then it suggests our sample comes from some normal distribution
shapiro.test(escape)
#data:  escape
#W = 0.94851, p-value = 0.3451
#Because p>5%, the results suggest that escape (which produced a large p-value) is normally distributed.

# Chi-Squared Goodness of Fit Test
library(nortest)
# Enjoyment:
pearson.test(enjoyment)
#data:  enjoyment
#P = 168.92, p-value < 2.2e-16
#Reject H0. Not a normal dist

# Revenge:
pearson.test(revenge)
#data:  revenge
#P = 9.9091, p-value = 0.1938
#Fail to reject. is a normal dist

# Escape:
pearson.test(escape)
#data:  escape
#P = 8, p-value = 0.09158
#Fail to reject. is a normal dist

# WARNING!!
#ENJOYMENT : For large sample sizes, some hypothesis tests are likely to reject the null hypothesis 
#even when it is true (e.g. rejecting normality even if the sample does come from a normal distribution).
#ESCAPE : For small sample sizes, they might fail to reject the null even when it is false.
#REVENGE : No issue. It is normally distributed based on CDF, KS test, QQ and Chi-squared test.

# estimate the parameters mu

mu      <- 0
sigma   <- 10 

muhat1  <- rep(NA, 1000)
muhat2  <- rep(NA, 1000)

for(i in 1:1000){
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type = 1)[3]   
}
par(mfrow = c(1, 2))

hist(muhat1, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat1), col = "blue", lty = 2, lwd = 3)

hist(muhat2, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat2), col = "blue", lty = 2, lwd = 3)

# muhat1 = xbar is the closest estimators

# estimate the parameters sigma

mu      <- 0
sigma   <- 10 

sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)

for(i in 1:1000){
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (9/10)*sd(x)^2   
}
par(mfrow = c(1, 2))

hist(sigma2hat1, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat1), col = "blue", lty = 2, lwd = 3)

hist(sigma2hat2, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat2), col = "blue", lty = 2, lwd = 3)

# sigma2hat1 = sd^2 is the closest estimators

# INFERENCE ABOUT THE MOTIVES
# H0: mu= 27 years old (95% Confidence interval)
# enjoyment - based on z-test as the sample size is large
n <- length(enjoyment)
sigma <- 8.6
xbar <- mean(enjoyment)                              
# Calculate the confidence interval:
CI =  xbar + c(-1, 1)*1.96*sqrt(sigma^2/n)   
# View the confidence interval:
CI  #29.91968 31.22403 - we reject H0 as 27 is not in our CI

r <- (1:n)
z <- (xbar - 27)/(sigma/sqrt(n))
c <- qnorm(0.975) # 1.96
z > c | z < -c # TRUE - this means the mu is not in the middle
# Hence, we reject H null 
# check p value
pnorm(-z)*2 # 7.006108e-27 smaller than 5%, thats why we reject H null

# revenge - based on z-test and t-test 
# z-test
n <- length(revenge)
sigma <- 8.6
xbar <- mean(revenge)                              
# Calculate the confidence interval:
CI =  xbar + c(-1, 1)*1.96*sqrt(sigma^2/n)   
# View the confidence interval:
CI  #28.14532 32.69104 - we reject H0 as 27 is not in our CI
z <- (xbar - 27)/(sigma/sqrt(n))
c <- qnorm(0.975) # 1.96
z > c | z < -c # TRUE - this means the mu is not in the middle
# Hence, we reject H null 
# check p value
pnorm(-z)*2 # 0.003201843 smaller than 5%, thats why we reject H null
# t-test
qt(p=0.975, df=n-1) #2.004879
s <- 8.6
CI =  xbar + c(-1, 1)*2.004879*sqrt(s^2/n)
CI  # 28.09328 32.74309 - we reject H0 

# escape - based on t-test as sample size is small
n <- length(escape)
xbar <- mean(escape)                              
# Calculate the confidence interval:
qt(p=0.975, df=n-1) #2.093024
s <- sd(escape)
#t-test
CI =  xbar + c(-1, 1)*2.093024*sqrt(s^2/n)
CI  #30.05111 35.84889 - we reject H0 
#z-test
CI =  xbar + c(-1, 1)*1.96*sqrt(sigma^2/n)  
CI  #29.18088 36.71912 - we reject H0

t.test(escape, alternative = "two.sided", mu = 27, conf.level = 0.95)
#t = 4.296, df = 19, p-value = 0.00039
#alternative hypothesis: true mean is not equal to 27
#95 percent confidence interval:
#  30.05111 35.84889
#sample estimates:
#  mean of x 
#32.95 

t = (xbar - 27)/sqrt(s^2/n)
pt(q = abs(t), df = n-1)
#0.999805

library(nortest)
pearson.test(escape)
shapiro.test(escape)
par(mfrow = c(1, 1))
qqnorm(escape)
hist(escape)
#We don’t need a large sample size assumption when we base our confidence intervals on the t-test. 
#The main assumption here is normality. We can, for example, check this using a histogram, 
#a normal Q-Q plot, a Shapiro-Wilk test and a chi-squared test.




library(forestplot)
library(dplyr)
library(metafor)
library(CompQuadForm)
library(meta)

