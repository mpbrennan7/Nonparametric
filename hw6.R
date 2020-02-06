#5.1
#Ho: median(bloodDif) = 0
#H1: median(bloodDif) =/= 0

bloodDif <- c(7,5,12,-3,-5,2,14,18,19,21,-1)
bloodDif <-sort(bloodDif)

#We have no knowledge of how the differences in systolic blood pressures are distributed
#Therefore, a sign test for this hypothesis is appropriate
hist(bloodDif)
boxplot(bloodDif)
library(BSDA)
SIGN.test(bloodDif, md = 0, alternative = "two.sided", conf.level = .95)

#p-value = .227
#Confidence interval: (-1.57, 18.23)
#Based on available data, we fail to reject the null.

#5.2
lab1<-c(11.7,12.1,13.3,15.1,15.9,15.3,11.9,16.2,15.1,13.6)
lab2<-c(10.9,11.9,13.4,15.4,14.8,14.8,12.3,15.0,14.2,13.1)

creamDifferences<-rep(0,length(lab1))
for (i in 1:length(lab1)){
  creamDifferences[i] = lab1[i] - lab2[i] 
}
hist(creamDifferences)
#The textbook says to use a Wilcoxon Signed-Rank test, so we'll use it
wilcox.test(creamDifferences, mu=0, alternative = "two.sided",conf.int=TRUE, conf.level = .95)
#The p-value of .05263 indicates some support for failing to reject the null. Both laboratories may have no consistent
#difference in the bacterium level for subsamples from the same dairy. However, we recommend obtaining a larger 
#sample before reaching a definitive conclusion.

#95% CI: (-.04998, .8999) <-- this is for the median

wilcox.test(creamDifferences, mu=0, alternative = "two.sided",conf.int=TRUE, conf.level = .99)

#99% CI: (-.35, 1.15) <--this is for the median, can we infer that the distribution is symmetric because we're
#using the Wilcoxon Signed-Rank test?

#When normality is assumed, we can construct a CI with a t test
t.test(creamDifferences, mu=0, alternative = "two.sided", conf.level = .95)
#95% CI is (.0294, .8505)
t.test(creamDifferences, mu=0, alternative = "two.sided", conf.level = .99)
#99% CI is (-.1498, 1.0298)


#5.5
#Ho: median(CAL) = median(lectures)
#H1: median(CAL) > median(lectures)

CAL<-c(50,56,51,46,88,79,81,95,73)
lectures<-c(25,58,65,38,91,32,31,13,49)

testDifferences<-rep(0,length(CAL))
for (i in 1:length(CAL)){
  testDifferences[i] = CAL[i] - lectures[i] 
}

#In other words, Ho: median(testDifferences) = 0
#H1: median(testDifferences) > 0

hist(testDifferences)
#This histogram does not supply evidence for the symmetry of the parent population
#Therefore, a sign test is appropriate to test this claim.
library(BSDA)
SIGN.test(testDifferences, md=0, alternative = "greater")
#result: p-value = .2539
#For standard .95 alpha, we reject Ho in favor of H1. It is reasonable to say that CAL material leads to better
#examination results

#5.6


#Ho: p1+ = p+1
#H1: p1+ =/= p+1

video<-matrix(c(41,27,16,58),nrow=2,ncol=2,byrow = TRUE)
mcnemar.test(video)
#p-value = .1273
#Moderate support for failure to reject of the null
z<-sqrt(2.3256)
z
#This z-score indicates that the true difference in proportions may indeed be 0
#No significant evidence against Ho
#Therefore, it is reasonable to say that the video may not have been efficacious






#5.8
#A harder (negative): 16
#B harder (positive): 24

#Ho: median(S) = 0
#H1: median(S) =/= 0 
#n = 40
#S = 8

#S~Binom(40,.5) under Ho
#Test statistic of 8

#P(x<=16)


#p-value 2*P(S<=8)
#P(S<=8) = 
2*pbinom(16,40,.5)
#2*pbinom(8,40,.5)
#p-value = .000182
#Strong rejection of null


#5.10

#Ho: mean(golfDifferences) <= 3
#H1: Mean(golfDifferences) > 3

Round2<-c(73,73,74,66,71,73,68,72,73,72)
Round3<-c(72,79,79,77,83,78,70,78,78,77)

golfDifferences<-rep(0,length(Round2))
for (i in 1:length(Round2)){
  golfDifferences[i] = Round3[i] - Round2[i] 
}
golfDifferences
hist(golfDifferences)
#From the sample, all of the differences are less than 3


library(BSDA)
wilcox.test(golfDifferences, mu = 3, alternative = "greater", conf.level = .95)


