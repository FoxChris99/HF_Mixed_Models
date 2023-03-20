library(MASS)
load("C:/Users/foxfo/OneDrive/Desktop/Applied Statistics/Project/HeartFailure/pazienti/PATIENTS_MCA.Rda")
load("C:/Users/foxfo/OneDrive/Desktop/Applied Statistics/Project/HeartFailure/pazienti/com_pat100.Rda")

df = data.frame(PATIENTS_LPCA[,-c(11,12,13,14,15)], logpca_model$PCs)
label <- factor(df$labelOUT, labels=c('DECEDUTO','TRONCATO'))

t <- which(label=='TRONCATO')
d <- which(label=='DECEDUTO')


df = df[,-c(1,3,4,5,6,7,8)]
nt = length(t)
nd = length(d)

df = df[-c(1,2,3,6,7,8)]#X1 e X2
#df=df[1:300,]
#label = label[1:300]
#polmoni = polmoni[1:300]
lda.patients <- lda(as.matrix(df), polmoni)
lda.patients


Lda.patients <- predict(lda.patients, as.matrix(df))
Lda.patients$posterior

#-------------------------------------------------------------------------
#guardiamo i problemi ai polmoni riferendoci a PC1 e PC2
polmoni = factor(com_pat$pulmonarydz, labels=c('0','1'))
x11()
#plot(as.matrix(df), main='label', xlab='età', ylab='num_osp', pch=20)
#points(as.matrix(df)[d,], col='red', pch=20)
#points(as.matrix(df)[t,], col='green', pch=20)
col.polm = ifelse(polmoni ==1 , 'red', 'blue')
plot(df, col = col.polm)

points(lda.patients$means, pch=4,col=c('blue','red') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(eta_min=x, n_osp_diversi=y)

z  <- predict(lda.patients, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
#contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


#AERCV
errors <- (Lda.patients$class != polmoni)

AERCV   <- sum(errors)/length(polmoni) * 100
AERCV#5%

#-------------------------------------------------------------------------
#guardiamo i problemi hypertension
hypertension = factor(com_pat$hypertension, labels=c('0','1'))

lda.patients <- lda(as.matrix(df), hypertension)

Lda.patients <- predict(lda.patients, as.matrix(df))


x11()
#plot(as.matrix(df), main='label', xlab='età', ylab='num_osp', pch=20)
#points(as.matrix(df)[d,], col='red', pch=20)
#points(as.matrix(df)[t,], col='green', pch=20)
color = ifelse(hypertension ==1 , 'red', 'blue')
plot(df, col = color)

points(lda.patients$means, pch=4,col=c('blue','red') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(X1 = x,  X2= y)

z  <- predict(lda.patients, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
#contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


#AERCV
errors <- (Lda.patients$class != hypertension)

AERCV   <- sum(errors)/length(hypertension) * 100
AERCV#14%

#---------------------------------------------------
#guardiamo i problemi arrhytmia
arrhytmia = factor(com_pat$arrhythmia, labels=c('0','1'))

lda.patients <- lda(as.matrix(df), arrhythmia)

Lda.patients <- predict(lda.patients, as.matrix(df))


x11()
#plot(as.matrix(df), main='label', xlab='età', ylab='num_osp', pch=20)
#points(as.matrix(df)[d,], col='red', pch=20)
#points(as.matrix(df)[t,], col='green', pch=20)
color = ifelse(arrhythmia ==1 , 'red', 'blue')
plot(df, col = color)

points(lda.patients$means, pch=4,col=c('blue','red') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(X1 = x,  X2= y)

z  <- predict(lda.patients, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
#contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


#AERCV
errors <- (Lda.patients$class != arrhythmia)

AERCV   <- sum(errors)/length(arrhytmia) * 100
AERCV#7%


#------------------------------------------------------------------------------
#Qda sui primi 100 pazienti

qda.patients <- qda(df[1:100,], polmoni[1:100])
qda.patients

Qda.patients <- predict(qda.patients, df[1:100,])

col.polm = ifelse(polmoni ==1 , 'red', 'blue')
plot(df[1:100,], col = col.polm)

points(qda.patients$means, pch=4,col=c('blue','red') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(eta_min=x, n_osp_diversi=y)

z  <- predict(qda.patients, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
#contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


#AERCV
errors <- (Qda.patients$class != polmoni[1:100])

AER   <- sum(errors)/length(polmoni) * 100
AER#5%

#-----------------------------------------------------------------------------
### knn-classifier (polmoni, 100 pazienti)
library(class)

k <- 7

x11()
plot(df[1:100,], main='df', xlab='feature1', ylab='feature2', pch=20)
points(df[which(polmoni[1:100]=='TRONCATO'),], col=2, pch=20)
points(df[which(polmoni[1:100]=='DECEDUTO'),], col=3, pch=20)
legend("topright", legend=levels(polmoni[1:100]), fill=c(3,2))

x  <- seq(min(df[1:100,1]), max(df[1:100,1]), length=200)
y  <- seq(min(df[1:100,2]), max(df[1:100,2]), length=200)
xy <- expand.grid(feature1=x, feature2=y)

df.knn <- knn(train = df[1:100,], test = xy, cl = polmoni[1:100], k = k)

z  <- as.numeric(df.knn)

contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)



