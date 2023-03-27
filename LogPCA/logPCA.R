library(logisticPCA)
library(ggplot2)
library(rARPACK)



D=com_pat[,-c( 1,5,7,19)]
D=D[,-c(1,2,17,6,10,11,5,13,14)]#logpca su 9 comorb

#D = D[1:1000,]

colSums(D)/100000 * 100 #freq comorb
heatmap(cor(D))#correlazione comorb
#per cercare il miglior m da usare in logisticPCA
#m in logistic PCA is chosen by cross validation
#logpca_cv = cv.lpca(D, ks = 5, ms = 4:20, folds = 5)
#plot(logpca_cv)

logpca_model = logisticPCA(D, k = 5, m = 11 ,main_effects = TRUE)#main_effects da capire
logpca_model

com = colnames(D)

#SCORES
scores = logpca_model$PCs
loadings = logpca_model$U

#variances of original variables
#ci aspettiamo che solo le comorb con alta variabilità verranno spiegate da pca
barplot(sapply(D,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')

#vedo i loadings significativi abs > 0.4 si può pensare di cambiarlo (abbassarlo)
x11()
par(mar = c(1,5,0,2), mfrow = c(5,1))
for(i in 1:5) barplot(ifelse(abs(loadings[,i]) < 0.25, 0, loadings[,i]) , ylim = c(-1, 1),names.arg = colnames(D))

#guardo spazialmente i loadings delle comorb
x11()
plot(loadings,xlim = c(-1,1),ylim = c(-1, 1));abline(h=0);abline(v=0)
text(loadings[,1],loadings[,2],colnames(D), cex=0.6, adj = c(1,-1))

#quelle dentro al quadrato centrale vengono spiegate poco (abs<0.3)
plot(loadings,xlim = c(-1,1),ylim = c(-1, 1))
text(loadings[,1],loadings[,2],colnames(D), cex=0.6, adj = c(1,-1))
abline(h=0.3);abline(v=0.3);abline(h=-0.3);abline(v=-0.3)

#pc1 positiva su pulmonarydz e hypertension e (pulmcirc , pvd)
#pc2 negativa su arrhytmia e hypertension
#pc3 positiva su pvd (e renal hypertension, negativa su tumor e arrhytmia)
#pc4 negativa su chf, renal e anemia
#pc5 positiva su tumor e pvd, negativa su renal



#------------------------------------------------------------------------------
#grafici per vedere se i pazienti formano delle classi rispetto alle pc1 e pc2
#plot label out on pca
label = PATIENTS$labelOUT
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = label)) + 
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("blue", "red"))
#pulmonarydz
polmoni = factor(com_pat$pulmonarydz, labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = polmoni)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("blue", "red"))
#arrhythmia
arrhythmia = factor(com_pat$arrhythmia, labels=c('0','1'))
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = arrhythmia)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("blue", "red"))

#------------------------------------------------------------------------------

#----------------------------------------------------------
#plot pc1 vs pulmonarydz e hyper
x11()
par(mfrow = c(2,1))
plot(com_pat$pulmonarydz,logpca_model$PCs[,1])
plot(com_pat$hypertension,logpca_model$PCs[,1])


#plot pc2 vs aritmia e hyper
x11()
par(mfrow = c(2,1))
plot(com_pat$arrhythmia,logpca_model$PCs[,2])
plot(com_pat$hypertension,logpca_model$PCs[,2])

PATIENTS_LPCA = data.frame(PATIENTS[,-c(11,12,13,14,15)], logpca_model$PCs)



#plot bello
x11()
ggplot(data.frame(logpca_model$PCs[,1:2]),aes(logpca_model$PCs[,2],logpca_model$PCs[,1])) +  geom_point(alpha = 0.1) +
  geom_rug(alpha = 0.01)
