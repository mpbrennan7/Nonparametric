#3.10

sum<-0
sum<- sum +factorial(20)/factorial(0)/factorial(20-0) * (.5^20)
sum<- sum +factorial(20)/factorial(1)/factorial(20-1) * (.5^20)
sum<- sum +factorial(20)/factorial(2)/factorial(20-2) * (.5^20)
sum<- sum +factorial(20)/factorial(3)/factorial(20-3) * (.5^20)
sum<- sum +factorial(20)/factorial(4)/factorial(20-4) * (.5^20)
sum<- sum +factorial(20)/factorial(5)/factorial(20-5) * (.5^20)
sum<- sum +factorial(20)/factorial(6)/factorial(20-6) * (.5^20)
sum<- sum +factorial(20)/factorial(7)/factorial(20-7) * (.5^20)
sum<- sum +factorial(20)/factorial(8)/factorial(20-8) * (.5^20)
sum<- sum +factorial(20)/factorial(9)/factorial(20-9) * (.5^20)
2*sum


pbinom(7.5,20,.5)



#3.16
plants<-c(21,18,42,29,81,12,94,117,88,210,44,39,11,83,42,94,2,11,33,91,141,48,12,50,61,35,111,73,5,44,6,11,35,91,147,83,91,48,22,17)
hist(plants)
boxplot(plants)

#(i) sign test
plants<-sort(plants)
plants
#Personal note: 2*P(x<=0) is same as saying the 95% confidence level is between (2,210)

#Trial and error to find the interval with an acceptable confidence level
pbinom(0,40,.5)
pbinom(10,40,.5)
pbinom(11,40,.5)
pbinom(12,40,.5)
pbinom(13,40,.5)#Aha!
pbinom(14,40,.5)

1-2*pbinom(14,40,.5)
1-2*pbinom(13,40,.5) #We are 96.1% confident that the population median is between the 14th order statistic and the 26th order statistic
#14th order statistic: 33
#26th order statistic: 73

#(ii)
wilcox.test(plants,mu=50,alternative = "two.sided",conf.int=TRUE)


#3.17
parking<-c(10,42,29,11,63,145,11,8,23,17,5,20,15,36,32,15)
sort(parking)
hist(parking)
#sign test
length(parking)
#trial and error to find appropriate tails
pbinom(0,16,.5)
pbinom(1,16,.5)
pbinom(2,16,.5)
pbinom(3,16,.5) #<---
pbinom(4,16,.5)
1-2*pbinom(4,16,.5) #Not a high enough conf level
1-2*pbinom(3,16,.5)

#We are 97.8% confident that the population median is between the 3rd and 14th order statistics
#3rd order statistic: 10
#14th order statistic: 42
#CI: (11,36)

#3.19
component<-c(14.2,11.3,12.7,19.2,13.5,14.4,11.8,15.1,12.3,11.7,13.2,13.4,14.0,14.1,13.7,11.9,11.8,10.7,11.3,12.2)
boxplot(component)
#This boxplot indicates a symmetric distribution
#Therefore, we can use a Wilcoxon-signed Rank test
wilcox.test(component,mu=14.2,alternative="two.sided",conf.int = TRUE)


#3.20
wt<-c(-1.2,1.4,.2,-.7,-6.4,-2.7,-8.6,-1.7,-2.2,.1,-.4,-4.2,-1.6,1.2,-1.3,-2.4,3.1,-.2,-4.5,-6.3,-1.7,0.0,.2,-3.7,1.1,-2.3,-.1,-7.3,.2,-1.4,-.9,-2.0,0.0,1.1,-.3,-1.1)
length(wt)
boxplot(wt)
sort(wt)
pbinom(11,length(wt), .5)#p-value
1-2*pbinom(11,length(wt),.5)#CL of CI


#6 bootstrapping
data(BodyTemp50,package='Lock5Data')
str(BodyTemp50)
pulses<-BodyTemp50$Pulse
pulses

hist(pulses)
xbar <-mean(pulses)
xbar
s<-sd(pulses)
s

B<-10000
xbars <- rep(0,B)

for(i in 1:B){
  xbs<-sample(pulses,length(pulses),replace=TRUE)
  xbars[i] = mean(xbs)
}

hist(xbars)
mean(xbars) #E*(Xbar*) roughly equal to mean(x) which is xbar
#off a little because we only have 10,000 xbars, not infinite
sd(xbars)

se.xbars <- sd(xbars) #se stands for standard error
tcv <- qt(0.975,length(pulses)-1) #take t for 1-alpha/2, n-1
#tcv stands for critical values from the t distribution


# A 95% percentile bootstrap CI for mu using the bootstrap sampling distribution of xbars

quantile(xbars,probs=c(0.025,0.975)) 


# A 95% bootstrap CI for mu using se.xbars to estimate the standard error of xbar 

mean(pulses) + c(-1,1)*tcv*se.xbars