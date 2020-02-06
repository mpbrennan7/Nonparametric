#11.2
hours<-c(0,1,2,3,4,5,6)
flow<-c(2.5,3.1,3.4,4.0,4.6,5.1,11.1)

library(Rfit)
library(geometry)
flow_par_model<-lm(flow~hours)
res_flow <- resid(flow_par_model)
res_flow
sum(res_flow)
dot(res_flow,hours)


#11.7

#really asking, is beta = 0?
horses<-c(91.8,88.0,80.6,43.6,16.7,14.4,10.5)
tractors<-c(9.2,30.9,51.8,72.7,89.9,88.7,90.2)

plot(horses,tractors)

#two different clusters make it inappropriate for us to do regression here????




#11.8
x<-c(25,50,100,150,155,187,200,237,287,290,300)
y<-c(6.13,5.51,6.18,6.70,7.22,7.28,7.22,7.48,7.38,7.38,7.64)
library(Rfit)
plot(x,y)


#Ho: beta = 0
#H1:beta =/= 0

nonpar_model <- rfit(y~x)
nonpar_model
#estimated beta of .006527
summary(nonpar_model)
#p-value .005708

.0063158 + qt(.975,9)*.0017522
.0063158 - qt(.975,9)*.0017522

#standard error = standard deviation of ammonium concentrate

abline(rfit(y~x))
legend("topleft",c('Rank'),lty=c(1))

# residual analysis

res_nonpar <- resid(nonpar_model)
yhat_nonpar <- fitted.values(nonpar_model)

shapiro.test(res_nonpar)
#do not reject normality
#sample may come from a normally distributed population

plot(yhat_nonpar,res_nonpar)

#11.11
eng<-c(38,57,85,108,129,145,156,185,193,265)
scot<-c(56,76,103,131,150,160,168,201,195,292)

plot(eng,scot)
flower_model<-rfit(scot~eng)
flower_model

summary(flower_model) #reject Ho, beta is not 0

scot122<-.9864 * 122 + 18.837
scot122 #101.5

#residual analysis

resids_flowers<-resid(flower_model)
shapiro.test(resids_flowers)
#may come from normal population

#5
#regression to the mean: high game1 -> low game2, low game1->high game2
#so negative slope
#another way to ask this question:
#Ho: beta = 0
#H1: beta < 0
library(npsm)
data("simon")
plot(game2~game1,data=simon)


plot(game2~game1,data=simon)
nonpar_model <- rfit(game2~game1,data = simon)
abline(rfit(game2~game1,data = simon))
abline(0,1,lty=2)
legend("topleft",c('Rank','Y=X'),lty=c(1,2))

#c)
summary(nonpar_model)
nonpar_model
#rank-based estimate of slope of line: .500000
#CI: betahat +- t*SE
CI<-.5 + c(-1,1)*qt(.95,length(simon$game1)-2)*.15751
#probably 95% but should check
CI
#d) Standard deviation is estimated by standard error = .15751
#e) p-value .003541 suggests that game1 does have predictive power for game2
#Furthermore, that CI is positive, suggesting that there is no regression to the mean
