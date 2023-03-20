
attach(PATIENTS3anni)

grado <- factor(grado_ccs)

i1 <- which(grado_ccs%in%c(1:3))
i2 <- which(grado_ccs%in%c(4))
i3 <- which(grado_ccs%in%c(5))
i4 <- which(grado_ccs%in%c(6))


n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n4 <- length(i4)
n  <- n1+n2+n3+n4

g  <- 4
p  <- 2

### Verify the assumptions:
# 1)  normality (multivariate) in each group (3 tests)
Ps <- NULL
for(i in 1:g)
  Ps <- c(Ps, mcshapiro.test(iris[get(paste('i',i, sep='')),1:4])$p) 
Ps

df = PATIENTS3anni[,c(4,6,7)]
j = which(df$labelOUT=="DECEDUTO")
df$labelOUT = 1
df$labelOUT[j]=0
mcshapiro.test(df[i1,-c(3)])
mcshapiro.test(df[i2,-c(3)])
mcshapiro.test(df[i3,-c(3)])
mcshapiro.test(df[i4,-c(3)])

cov(df[i1,-c(3)])
cov(df[i2,-c(3)])
cov(df[i3,-c(3)])
cov(df[i4,-c(3)])


### One-way MANOVA 
df1 = df[,-3]
fit <- manova(as.matrix(df1) ~ grado)
summary.manova(fit,test="Wilks")

### Via ANOVA: for each of the p=4 variables we perform an ANOVA test
###            to verify if the membership to a group has influence
###            on the mean of the variable (we explore separately the
###            4 axes directions in R^4)
summary.aov(fit)

#To understand which group differs
### Via Bonferroni
alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(df1,mean)         # estimates mu
m1 <- sapply(df1[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(df1[i2,],mean)    # estimates mu.2=mu+tau.2
m3 <- sapply(df1[i3,],mean)    # estimates mu.3=mu+tau.3
m4 = sapply(df1[i4,],mean)
inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )

inf14 <- m1-m4 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n4) )
sup14 <- m1-m4 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n4) )
inf43 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n4+1/n3) )
sup43 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n4+1/n3) )
inf24 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n4) )
sup24 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n4) )



CI <- list(uno_due=cbind(inf12, sup12), uno_tre=cbind(inf13, sup13), due_tre=cbind(inf23, sup23),uno_quattro=cbind(inf14, sup14),due_quattro=cbind(inf24, sup24),quattro_tre=cbind(inf43, sup43))
CI
