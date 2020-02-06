#4.1
mcGamma<-c(13,13,22,26,33,33,59,72,72,72,77,78,78,80,81,82,85,85,85,86,88)
shapiro.test(mcGamma)

#4.2
parking<-c(10,42,29,11,63,145,11,8,23,17,5,20,15,36,32,15)
mu<-20
rate<-1/mu
rate
ks.test(parking,"pexp",rate,alternative = "two.sided")
#should be p-value = .165

#4.4
insurance<-c(1175,1183,1327,1581,1592,1624,1777,1924,2483,2642,2713,3419,5350,7615)
#X = amount of 2005 British insurance claims
# Ho: X ~ Unif(1100,7700)
# H1: X /~ Unif(1100,7700)
ks.test(insurance, "punif", 1100, 7700, alternative = "two.sided")
# For any alpha >= .0002277, we reject the null in favor of the alternative. Therefore, it is not reasonable to assume that the 
# data came from a population uniformly distributed from 1100 to 7700.


#4.6
binom.test(26,77,p=.1, alternative = "two.sided")
#A 95% CI is (.234, .454)
#Since the proposed value of .1 falls below the low end of this confidence interval,
#the commentator's estimate was too pessimistic.

#4.7
binom.test(6,18,p=.5,alternative = "two.sided")

binom.test(75,225, p=.5, alternative = "two.sided")
#power increases with sample size


#4.12
#Ho: The data has no monotonic trend
#H1: The data may have a monotonic trend
library(randtests)
railway<-c(76,92,105,86,91,81,103,92,71,132,71,57,48,63,43,60)
runs.test(railway)

#p=.03843
#for any p >= .03843, we reject the null in favor of H1 
#For a rule-of-thumb .05 = alpha, we reject Ho in favor of H1. Therefore, it is reasonable to assume that some trend may exist.