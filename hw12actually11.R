library(DescTools)

#1
#a


#remember that p1 is the probability of falling in p11 out of p1+, and p2 is P(falling in p21 out of p2+)
#probability of "success" for treatment 1 vs probability of "success" for treatment 2


afterlifetable <- matrix(c(509, 116, 398,104), ncol=2, byrow=TRUE,
                  dimnames = list("Gender" = c("Female", "Male"),"Believe in Afterlife" = c("Yes", "No or Undecided")))



fmsums<-rowSums(afterlifetable, dims = 1)
n1plus_afterlife = fmsums['Female']
n2plus_afterlife = fmsums['Male']

n11_afterlife = afterlifetable['Female', 'Yes']
n21_afterlife = afterlifetable['Male','Yes']



p1_hat_afterlife = n11_afterlife/n1plus_afterlife
p2_hat_afterlife = n21_afterlife/n2plus_afterlife

p1_hat_afterlife - p2_hat_afterlife

alpha_afterlife = .1
z_afterlife = qnorm(1-(alpha_afterlife/2))

SE_p_afterlife = z_afterlife * sqrt((p1_hat_afterlife*(1-p1_hat_afterlife)/n1plus_afterlife)+(p2_hat_afterlife*(1-p2_hat_afterlife)/n2plus_afterlife))
CI_p_afterlife = p1_hat_afterlife - p2_hat_afterlife + c(-1,1) * SE_p_afterlife
CI_p_afterlife
#-0.01766588  0.06080851


#b
#Ho: theta = 1
#H1: theta =/= 1
n12_afterlife = afterlifetable['Female','No or Undecided']
n22_afterlife = afterlifetable['Male', 'No or Undecided']

theta_hat_afterlife = n11_afterlife*n22_afterlife/n12_afterlife/n21_afterlife
theta_hat_afterlife

SE_theta_afterlife = z_afterlife * sqrt((1/n11_afterlife)+(1/n22_afterlife)+(1/n12_afterlife)+(1/n21_afterlife))

CI_theta_afterlife = log(theta_hat_afterlife) + c(-1,1)*SE_theta_afterlife
CI_theta_afterlife
#-0.1110979  0.3846914



#c
#Ho: p1 - p2 = 0  (Gender and belief in an anfterlife are independent)
#H1: p1-p2 =/= 0  (Gender and belief in an anfterlife are not independent)

#The proposed difference of 0 does not fall within the confidence interval of (0.009168424, 0.033974206). Therefore,
#we fail to reject the null hypothesis. Sample evidence suggests that gender and a belief in an
#afterlife are may be independent.


#2
#12.8
#Ho: p1 - p2 = 0
#H1: p1-p2 =/= 0

flighttable <- matrix(c(14, 34, 31,41), ncol=2, byrow=TRUE,
                         dimnames = list("Aptitude" = c("Pass", "Fail"),"Personality" = c("Introvert", "Extrovert")))

pfsums<-rowSums(flighttable, dims = 1)
n1plus_flight = pfsums['Pass']
n2plus_flight = pfsums['Fail']

n11_flight = flighttable['Pass', 'Introvert']
n21_flight = flighttable['Fail','Introvert']



p1_hat_flight = n11_flight/n1plus_flight
p2_hat_flight = n21_flight/n2plus_flight

p1_hat_flight - p2_hat_flight

alpha_flight = .05 #assuming standard alpha of .05
z_flight = qnorm(1-(alpha_flight/2))

SE_p_flight = z_flight * sqrt((p1_hat_flight*(1-p1_hat_flight)/n1plus_flight)+(p2_hat_flight*(1-p2_hat_flight)/n2plus_flight))
CI_p_flight = p1_hat_flight - p2_hat_flight + c(-1,1) * SE_p_flight
CI_p_flight
#-0.4498683 -0.1056872
#Since the proposed value for p1-p2 of 0 is not included in this 95% confidence interval, we fail to reject the null hypothesis.
#Therefore, aptitude and personality type are not associated. 


#3
#12.12
soccer <- matrix(c(55, 38, 26,25,37,19), ncol=3, byrow=TRUE,
                         dimnames = list("Win" = c("English win", "Scottish win"),"Nationality" = c("English", "Scottish", "Welsh")))

#Ho: Nationality and prediction of winner are independent
#Ho: Nationality and prediction of winner are not independent
chisq.test(soccer)
#p-value = .06979
#This p-value exceeds the standard alpha of .05. Therefore, we fail to reject the null hypothesis. Sample evidence 
#indicates that nationality and prediction of the winner of a game are independent. However, we recommend obtaining 
#a larger sample before taking action based on these results.

GTest(soccer)
#p-value = .06814

#4
#12.25
numbers <- matrix(c(31, 72, 60,57,27,63,53,58,29), ncol=3, byrow=TRUE,
                 dimnames = list("Second Digit" = c("1", "2","3"),"First Digit" = c("1","2","3")))

#Ho: First number and second number are independent
#H1: First number and second number are not independent


chisq.test(numbers)$expect
chisq.test(numbers)
#Based on this small p-value, we emphatically reject the null hypothesis.
#Therefore, the first and second number are not independent



#14.4

X<-c(1,2,3,5,8)
Y<-c(3,7,9,7,12)


B <- 10000
xoverybars <- rep(0,B) #like making an array of size B


# Generate B xbars-values using a loop
xtest
ytest
for( i in 1:B ) {
  
  xs<-rep(1,length(X))#array for this bootstrap sample
  ys<-rep(1,length(Y))
  
  for(j in 1:length(X)){
    
    index= sample(1:5, 1,replace=TRUE)
    x<-X[index]
    y<-Y[index]
    
    xs[j]<-x
    ys[j]<-y
  }
  
  xoverybars[i]<-mean(xs)/mean(ys)
}


hist(xoverybars)
mean(xoverybars)
se.xbars <- sd(xoverybars)
se.xbars
quantile(xoverybars,probs=c(0.025,0.975))  #does generate a 95% CI



#11.11
eng<-c(38,57,85,108,129,145,156,185,193,265)
scot<-c(56,76,103,131,150,160,168,201,195,292)

plot(eng,scot)
cor.test(eng,scot,method="pearson")$estimate


B <- 10000
pr <- rep(0,B) #like making an array of size B


# Generate B xbars-values using a loop
prtest<-0
for( i in 1:B ) {
  
  xs<-rep(1,length(eng))#array for this bootstrap sample
  ys<-rep(1,length(scot))
  
  for(j in 1:length(eng)){
    
    index= sample(1:length(eng), 1,replace=TRUE)
    x<-eng[index]
    y<-scot[index]
    xs[j]<-x
    ys[j]<-y
  }
  #pr[i]<-cor.test(xs,ys,method="pearson")$estimate
  pr[i]<-cor(xs,ys)
  
}


hist(pr)
mean(pr)
quantile(pr,probs=c(0.025,0.975))  #does generate a 95% CI

