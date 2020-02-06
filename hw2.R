#2.5
"""books<-c(126,142,156,228,245,246,370,419,433,454,478,503)


theta<-median(books)

df<-length(books)-1 #11

t<-qt(.975,df)

s =sd(books)
sqr_n = sqrt(12)
370 + (t*s/sqr_n)


theta + (t*s/sqrt(12))
theta - t*sd(books)/sqrt(length(books))
theta + t*sd(books)/sqrt(df)"""



help(wilcox.test)
wilcox.test(c(2,6,4,5),c(1,3,7))

