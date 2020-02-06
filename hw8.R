#7.1

sentences<-c(13,22,26,26,27,31,32,35,37,43,47,26,33,33,33,37,44,54)
group<-c(rep("vulliamy",5), rep("queen",6), rep("mccloy", 7))
boxplot(sentences~group)


par_model <- lm(sentences~group)#lm = a linear model: ldl depends on treatment
#Yij = mui + eij
anova(par_model)
#Ho: equal means
#H1: Not all means equal

res<-resid(par_model)
hist(res)
shapiro.test(res)




#7.4
flowers <- data.frame(
  nodes = c(60,65,63,64,62,61,62,65,61,67,65,62,61,68,61,63,62,62,60,65,60,61,64,65),    
  acid = c(rep(c('Control','Gibberellic','Kinetin','indole acetic', 'adenine', 'maleic'),4)),
  plant  =  c(rep('I',6),rep('II',6),rep('III',6),rep('IV',6))
)


#flowers

attach(flowers)

boxplot(nodes ~ acid) #no info about block shown here
#interaction.plot(nodes, acid, plant) #lots of crossing of lines would suggest that block and tire brand interact with one another

par_model <- lm(nodes ~ acid + plant) #parametric modeling of wear versus treatment + blocking factor
anova(par_model)

res <- resid(par_model)
hist(res)
shapiro.test(res)
#Moderate support for normality assumption

friedman.test(nodes ~ acid | plant)

#7.10
drugs<-data.frame(
  contractions=c(170,7,0,19,1.4,6,187,205,18,10,.3,1,216,.2,22,49,33,30,7,37,3,474,9,5,.4,.6,0,1.4,63,36,27,145,26,29,0,0),
  drugtype=c(rep(c('a','b','c'),12)),
  patient=c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3),rep(11,3),rep(12,3))
)

#drugs
attach(drugs)

friedman.test(contractions,drugtype,patient)

par_model <- lm(contractions ~ drugtype + patient) #parametric modeling of wear versus treatment + blocking factor
anova(par_model)

res <- resid(par_model)
hist(res)
shapiro.test(res)

#7.13
width<-c(53,50,52,50,49,47,54,51,52,57,49,49,47,54,43,51,49,51,50,46,49,58,51,45,53,49,51,50,51)
species<-c(rep(1,10),rep(2,11),rep(3,8))

kruskal.test(width~species)