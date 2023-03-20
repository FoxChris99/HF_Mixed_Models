library(logisticPCA)
library(ggplot2)
library(rARPACK)
load("C:/Users/foxfo/OneDrive/Desktop/Applied Statistics/Project/HeartFailure/pazienti/com_pat100.Rda")
#da caricare dataset com_pat100
D=com_pat[,-c( 1,5,7,19)]
D=D[,-c(1,2,17,6,10,11,5,13,14)]#logpca su 9 comorb
logpca_model = logisticPCA(D, k = 5, m = 11 ,main_effects = TRUE)
logpca_model
scores = logpca_model$PCs
loadings = logpca_model$U

#original variables vs scores



#vedo i loadings significativi abs > 0.25
x11()
par(mar = c(1,5,0,2), mfrow = c(5,1))
for(i in 1:5) barplot(ifelse(abs(loadings[,i]) < 0.25, 0, loadings[,i]) , ylim = c(-1, 1),names.arg = colnames(D))

#####################
#pc1
#####################
#+ pulmonarydz, hypertension, pulmcirc, pvd, anemia
#-
#spiega i problemi ai polmoni: pulmonarydz, pulmcirc, pvd (pulmonary vascular disease)
#che sono correlati all'hypertensione polmonare 
#e all'anemia (il numero di globuli rossi non è sufficiente a trasportare abbastanza ossigeno)
#pulmonarydz è ostruzione dei polmoni, collegata a pulmcirc e problemi del trasporto di ossigeno

#####################
#pc2
#####################
#- arrhythmia, hypertension
#+ pulmonarydz
#spiega la contrapposizione tra problemi polmonari (pulmonarydz) e del sangue
#che però non sono strettamente legati ai polmoni: hypertensione arteriosa e aritmia, che
#sono correlate tra loro

####################
#pc3
###################
#+ pvd,renal,hypertension
#- arrhytmia, tumor
#spiega positivamente problemi del sangue legati ai polmoni (pvd e hyper polm) e ai reni
#spiega negativamente tumore (in pochi lo avevano) e aritmia

###################
#pc4
##################
#- anemia, renal chf
#anemia renale e scompenso cardiaco vanno di pari passo spesso

#################
#pc5
#################
#+ tumor


#Concentrandosi solo su quelle molto spiegate abs(loadings)>0.4
par(mar = c(1,5,0,2), mfrow = c(5,1))
for(i in 1:5) barplot(ifelse(abs(loadings[,i]) < 0.4, 0, loadings[,i]) , ylim = c(-1, 1),names.arg = colnames(D))
#pc1 + ostruzione polmonare (dovuto a ipertensione polmonare)
#pc2 - aritmia causata dall'aumento di pressione arteriosa
#pc3 + circolazione polmonare (sangue e polmoni pvd)
#pc4 - anemia renale correlata a chf
#pc5 + tumore polmoni e non renale

#pc1 alta e pc2 alta vuol dire problemi circoscritti ai polmoni (ostruiti)
#pc1 alta e pc2 negativa vuol dire problemi a tutto l'apparato circolatorio
#e di conseguenza alla circolazione polmonare -> HF
#pc1 alta e pc3 alta vuol dire problemi gravi ai polmoni
#pc4 negativa e pc5 negativa vuol dire correlazione tra chf e insufficienza renale
#ma mancanza di problemi polmonari o tumorali


#zoom on PC1vsPC2
comorb = factor(com_pat$pulmonarydz==1  & com_pat$hypertension==1 & com_pat$arrhythmia==1 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&hyper=1&aritm=1"
  )


comorb = factor(com_pat$pulmonarydz==1  & com_pat$hypertension==0 & com_pat$arrhythmia==1 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&hyper=0&aritm=1"
  )

comorb = factor(com_pat$pulmonarydz==1  & com_pat$hypertension==1 & com_pat$arrhythmia==0 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&hyper=1&aritm=0"
  )

comorb = factor(com_pat$pulmonarydz==1  & com_pat$hypertension==0 & com_pat$arrhythmia==0 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&hyper=0&aritm=0"
  )


comorb = factor(com_pat$pulmonarydz==1 & com_pat$arrhythmia==0 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&aritm=0"
  )

comorb = factor(com_pat$pulmonarydz==1 & com_pat$arrhythmia==1 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&aritm=1"
  )

comorb = factor(com_pat$pulmonarydz==1 & com_pat$hypertension==0 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&hyper=0"
  )

comorb = factor(com_pat$hypertension==1 , labels=c('0','1')) 
x11()
plot(logpca_model, type = "scores") + geom_point(aes(colour = comorb)) +
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("lightblue", "salmon"))+
  labs(
    colour = "pulm=1&hyper=1"
  )




#pulmonarydz vs pc1
barplot(table(com_pat$pulmonarydz[logpca_model$PCs[,1]> 0]))
