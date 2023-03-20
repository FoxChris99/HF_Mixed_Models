library(FactoMineR)
library("factoextra")
load("C:/Users/foxfo/OneDrive/Desktop/Applied Statistics/Project/HeartFailure/pazienti/PATIENTS_MCA.Rda")
load("C:/Users/foxfo/OneDrive/Desktop/Applied Statistics/Project/HeartFailure/pazienti/com_pat100.Rda")

df = PATIENTS #dataset con anche le componenti MCA
X=com_pat[,-c( 1,5,7,19)]
X=X[,-c(1,2,17,6,10,11,5,13,14)]
i=0
while(i < ncol(X)){ #mca chiede tutti factor, tolgo (-1) i ccs
  i=i+1  
  X[,i] = as.factor(X[,i])
}

head(X)
res.mca = MCA(X, ncp = 5, graph = TRUE)

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

#X1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
#pulmcirc_1 - chf_0 - pulmonarydz_1

#pulmcirc
x11()
attach(com_pat)
plot(com_pat$pulmcirc,df$X1)

#confronto presenza problemi polmoni con chf e non presenza
par(mfrow=c(2,1))
#sono 3493
plot(com_pat$pulmcirc[com_pat$pulmcirc==1&chf==1&pulmonarydz==1],df$X1[com_pat$pulmcirc==1&chf==1&pulmonarydz==1])
#sono 73256
plot(com_pat$pulmcirc[com_pat$pulmcirc==0&chf==1&pulmonarydz==0],df$X1[com_pat$pulmcirc==0&chf==1&pulmonarydz==0])
#quelli con pulmcirc che non hanno pulmonaryds sono solo 700
length(com_pat$pulmcirc[com_pat$pulmcirc==1&pulmonarydz==0])
#confrinto solo chf e pulmonarydz
plot(com_pat$pulmcir[chf==1&pulmonarydz==0],df$X1[chf==1&pulmonarydz==0])
length(com_pat$pulmcirc[com_pat$pulmcirc==0&chf==1&pulmonarydz==0])
abline(h=1.2)
errore_pulm = (length(com_pat$COD_REG[com_pat$pulmcirc==0 & df$X1 >1])+length(com_pat$COD_REG[com_pat$pulmcirc==1 & df$X1 <1]))/length(com_pat$COD_REG) * 100
errore_pulm

x11()
q = data.frame(factor(com_pat$pulmcirc), df$X1)
ggplot(q, aes(x=factor(com_pat$pulmcirc), y=df$X1)) +  geom_boxplot(fill='gray')
#FP + FN / tot

#chf
plot(com_pat$chf,df$X1)
abline(h=1.2)
errore_chf = (length(com_pat$COD_REG[com_pat$chf==0 & df$X1 <1.2])+length(com_pat$COD_REG[com_pat$chf==1 & df$X1 >1.2]))/length(com_pat$COD_REG) * 100
errore_chf

#pulmonarydz
x11()

plot(com_pat$pulmonarydz,df$X1)
abline(h=1.2)
errore_pulmonarydz = (length(com_pat$COD_REG[com_pat$pulmonarydz==0 & df$X1 >1])+length(com_pat$COD_REG[com_pat$pulmonarydz==1 & df$X1 <1]))/length(com_pat$COD_REG) * 100
errore_pulmonarydz

x11()
par(mfrow=c(2,1))
q = data.frame(factor(com_pat$pulmonarydz), df$X1)
ggplot(q, aes(x=reorder(com_pat$pulmonarydz, com_pat$pulmonarydz, function(x)-length(x)))) +
  geom_bar(fill='black')
ggplot(q, aes(x=factor(com_pat$pulmonarydz), y=df$X1)) +  geom_boxplot(fill='gray')


