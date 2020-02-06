#3.8
x<-c(126,142,156,228,245,246,370,419,433,454,478,503)
wilcox.test(x,mu=400,alternative="two.sided",conf.int=TRUE)
#assume normality
t.test(x,mu=400,alternative = "two.sided", conf.int=TRUE)

#3.11
y<-c(3.1,1.8,2.7,2.4,2.9,.2,3.7,5.1,8.3,2.1,2.4)
wilcox.test(y,mu=2,alternative = "greater", conf.int=TRUE)


#3.13
fish<-c(64,65,65,66,67,68,68,68,68,69,69,69,70,70,70,70,71,71,71,71,71,72,72,72,73,73,73,75,77,77,77,77,77,77,78,83)
wilcox.test(fish,mu=73.5,alternative = "two.sided", conf.int=TRUE)
t.test(fish,mu=73.5,alternative = "two.sided", conf.int=TRUE)