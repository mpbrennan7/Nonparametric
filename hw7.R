#6.4

insulation<-c(15.7,14.8,14.2,16.1,15.3,13.9,17.2,14.9,
              13.7,14.1,14.7,15.4,15.6,14.4,12.9,15.1,14.0)
group<-c("old","old","old","old","old","old","old","old",
           "new","new","new","new","new","new","new","new","new")
boxplot(insulation[1:8])
boxplot(insulation[9:17])
var(insulation[1:8])
var(insulation[9:17])

#don't look like equal variances...




#Wilcox Mann Whitney
#Assumes populations have equal variances, is that ok?
#Book doesn't mention the equal variance assumption
wilcox.test(insulation ~ group, var.equal=FALSE, alternative="two.sided",mu=0, conf.int=TRUE)


#t-based
t.test(insulation ~ group, alternative = "two.sided", mu = 0, conf.int=TRUE)

#t-based procedure is more precise



#6.7

diabetic<-c(42,44,38,52,48,46,34,44,38)
normal<-c(34,43,35,33,34,26,30,31,27,28,27,30,37,38,32,32,36,32,32,38,42,36,44,33,38)

boxplot(diabetic)
boxplot(normal)
var(diabetic)/var(normal)
weight<-c(diabetic,normal)
weight
length(weight)
groups1<-rep("diabetic", length(diabetic))
groups1
groups2<-rep("normal", length(normal))
groups2

groups<-c(groups1,groups2)
groups
length(groups)

wilcox.test(weight~groups, mu=0, alternative="two.sided", conf.int = TRUE)
t.test(weight~groups, mu=0, alternative="two.sided", conf.int = TRUE)


#6.15 

withoutDifficulties<-c(204,218,197,183,227,233,191)
withDifficulties<-c(243,228,261,202,343,242,220,239)

ks.test(withoutDifficulties, withDifficulties, atlernative="two.sided")
ks.test(withoutDifficulties, withDifficulties, alternative = "greater")

#6.16
conover<-c(21,20,17,25,29,21,32,18,32,31)
bradley<-c(45,14,13,31,35,20,58,41,64,25)
words<-c(conover,bradley)

g1<-rep("conover",length(conover))
g2<-rep("bradley",length(bradley))
group<-c(g1,g2)
group<-as.factor(group)

#i)
#Check for difference in centrality
wilcox.test(conover, bradley, var.eq=FALSE)

#ii)
library(coin)
conover_test(words ~ group)

#iii)
ks.test(conover, bradley)

#iv)
shapiro.test(conover)
shapiro.test(bradley)

#5
#bootstrap

data(CaffeineTaps, package='Lock5Data')
str(CaffeineTaps)

taps <- CaffeineTaps$Taps

boxplot(taps ~ CaffeineTaps$Group)

M <- 10000
bs_diffs <- vector("numeric",length=M)
for(i in 1:M){
  #sampling with replacement from each independent sample
  bs_taps1 <- sample(taps[1:10],replace=TRUE) #caffeine
  bs_taps2 <- sample(taps[11:20],replace=TRUE) #no caffeine
  bs_diffs[i] <- mean(bs_taps1)-mean(bs_taps2) #Take a bootstrap sample mean and store it in vector
}

hist(bs_diffs)

CI <- quantile( bs_diffs, probs=c(0.025, 0.975) )  #Creates a 95% CI
CI



#WMW
wilcox.test(taps ~ CaffeineTaps$Group, mu = 0, alternative="two.sided",conf.int = TRUE)

#t
t.test(taps ~ CaffeineTaps$Group, mu = 0, alternative="two.sided",conf.int = TRUE)

#Which do I prefer? 
#Bootstrap is most precise and has all its assumptions met




#6

#a
data(Smiles, package='Lock5Data')
str(Smiles)
leniency <- Smiles$Leniency
Smiles$Group
hist(leniency[1:34])
hist(leniency[35:length(Smiles$Group)])

#b
mean(leniency[1:34])
Sx<-sd(leniency[1:34])
Sx

mean(leniency[35:length(Smiles$Group)])
Sy<-sd(leniency[35:length(Smiles$Group)])
Sy


(Sx^2)/(Sy^2)
#Not significantly different

#c
library(car)

#Ho: var of smiles = variance of neutral
#H1: var of smiles =/= var neutral
leveneTest(leniency ~ Smiles$Group)
#p-value .6188
#Fail to reject Ho


#d
t.test(leniency ~ Smiles$Group, var.equal=TRUE, mu=0)
#t is the test statistic



#e

#Ho:
#H1: mux - muy =/= 0 

# permutation test for a difference in two group means 

meanSmile <- mean(leniency[1:34])

meanNeutral <- mean(leniency[35:length(leniency)])

obs_ds <- meanSmile - meanNeutral #observed 
obs_ds

M <- 10000
diffs <- vector("numeric",length=M) #what is this line? A vector, with numeric entries, of 
for(i in 1:M){
  perm_leniency <- sample(leniency)     #take a sample from the taps (is it just a shuffling here?)
  diffs[i] <- mean(perm_leniency[1:34])-mean(perm_leniency[35:length(leniency)]) #store the difference in the sample from Group 1 and Group 2
}

hist(diffs)

sum(abs(diffs) >= obs_ds)   #sum where the absolute value of the difference is greater than the observed test statistic
#Means, sum up the the values that are more extreme than the observed test statistic
#The absolute value accounts for the two tails
pval<-sum(abs(diffs) >= obs_ds)/M # two-sided test  
#Takes the proportion of the sample more extreme that the observed test statistic
pval

#For p-value = .0452, reject Ho . However, recommend gathering a larger sample before 