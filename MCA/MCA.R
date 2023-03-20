#MCA PATIENTS 100
library(FactoMineR)
library("writexl")
library("factoextra")

df = PATIENTS

X=com_pat[,-c( 1,5,7,19)]
X=X[,-c(1,2,17,6,10,11,5,13,14)]
#X = X[chf=1,]#solo quelli con chf
#X = X[,-c(1)]#tolgo chf
i=0
while(i < ncol(X)){ #mca chiede tutti factor, tolgo (-1) i ccs
  i=i+1  
  X[,i] = as.factor(X[,i])
}

res.mca = MCA(X, ncp = 5, graph = TRUE)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_contrib(res.mca, choice = "var", axes = 3, top = 15)

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

res.desc <- dimdesc(res.mca, axes = c(1,2,3))
# Description of dimension 1
res.desc[[3]]
# Description of dimension 2
c = res.desc[[3]]

df = PATIENTS

COMPONENTE_3_ARITMIA = df$X3
ARITMIA = com_pat$arrhythmia

table(ARITMIA)

table(ARITMIA[COMPONENTE_3_ARITMIA < 0.2 & COMPONENTE_3_ARITMIA > - 0.2])

COMPONENTE_1 = df$X1
chf = com_pat$chf

table(chf)

table(chf[COMPONENTE_1 < -0.23])

plot(ARITMIA, COMPONENTE_3_ARITMIA)
abline(h = 0.25)

plot(chf, COMPONENTE_1)

pulmcirc = com_pat$pulmcirc
plot(pulmcirc, COMPONENTE_1)

pulmonarydz = com_pat$pulmonarydz
plot(pulmonarydz, COMPONENTE_1)

COMPONENTE_2 = df$X2
plot(com_pat$renal, COMPONENTE_2)


COMPONENTE_4 = df$X4
plot(com_pat$tumor, COMPONENTE_2)
