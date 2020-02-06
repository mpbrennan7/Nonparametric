#10.4
year<-c(1827,1884,1895,1908,1914,1918,1924,1928,1936,1941,1964,1965,1977)
age<-c(13,83,34,1,11,16,68,13,77,74,87,65,83)

plot(year,age)
#hard to see a trend, looks roughly positive

#Ho: rho = 0
#H1: rho > 0
cor.test(year,age,method="spearman", alternative= "greater")
#p-value .03854 indicates that the true population correlation coefficient is greater than 0

cor.test(year,age,method="kendall", alternative = "greater")
#p-value of .0331 indicates that rho is greater than 0

#kendall is more confident than spearman

#10.9

#Ho: rho = 0
#H1: rho =/= 0

mvo<-c(78,92,116,90,106,78,89)
lvp<-c(33,33,45,30,38,24,44)

plot(mvo,lvp)
cor.test(mvo,lvp,method="pearson")
#p-value = .07875
#Not enough evidence to support rejecting the null hypothesis. However gather more data etc
#is bivariate normality met?
hist(mvo)
hist(lvp)
#doesn't really look normal for either

cor.test(mvo,lvp,method="spearman")
cor.test(mvo,lvp,method="kendall")

#10.10
cattle<-c(41,0,42,15,47,0,0,0,56,67,707,368,231,104,132,200,172,146,0)
sheep<-c(4716,4605,4951,2745,6592,8934,9165,5917,2618,1105,150,2005,3222,7150,8658,6304,1800,5270, 1537)

hist(cattle)
hist(sheep)

#Ho: rho = 0
#H1: rho =/= 0

cor.test(cattle,sheep,method="pearson")
#which test is appropriate?
#data does not look normal, so pick one of the nonparametric tests


#cor.test(cattle,sheep,method="spearman") didn't use spearman b/c it's closely related to Pearson's r. It's
#fine if you do use it though
cor.test(cattle,sheep,method="kendall")


#10.14
veneer<-matrix(c(7,2,1,4,96,18,1,10,11),nrow=3,ncol=3,byrow = TRUE)
veneer

#Ho: K = 0
#H1: K =/= 0


library(psych)
cohen.kappa(veneer, alpha = .05)
#use unweighted kappa



#5
library(npsm)
data(bb2010)
#bb2010
#help(bb2010)

hr<-bb2010$hr
rbi<-bb2010$rbi

plot(hr,rbi)
#looks like a positive correlation
#Jose Bautista is a monster, 54 hr and 124 RBI in 2010

hist(hr) #does not look normal
hist(rbi) #looks normalish

#Ho: rho = 0
#H1: rho =/= 0

cor.test(hr,rbi,method = "pearson") 
#p-value very small -> some trend exists
#CI indicates very positive correlation
#But hists do not support bivariate normality, so treat result with caution


cor.test(hr,rbi,method = "spearman") 
#small p-value -> some trend exists

cor.test(hr,rbi,method = "kendall") 






