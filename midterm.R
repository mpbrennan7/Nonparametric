
#2

pnorm(2.8,3.0,.1)
pnorm(2.9,3.0,.1) + 1 - pnorm(3.1,3.0,.1)

#3
visits <- c(9.4,13.4,15.6,16.2,16.4,16.8,18.1, 18.7,18.9,19.1,19.3,20.1,20.4,21.6,21.9,23.4,23.5,24.8,24.9,26.8)
visits <-sort(visits)
length(visits)
median(visits)
x<-sum(visits < 22)
x

pbinom(15,20,.5)


#4

library(MASS)
help(Boston)
attach(Boston)
summary(dis)

boxplot(dis) #doesn't look symmetric, why are we using wilcox and t
hist(dis)

wilcox.test(dis, mu =3.5, alternative = "two.sided", conf.int = TRUE)
t.test(dis,mu=3.5,alternative = "two.sided")



#5
binom.test(175,272,alternative ="two.sided")



#6
polls<-matrix(c(63,21,4,13), nrow =2, ncol=2, byrow = TRUE)
mcnemar.test(polls)


