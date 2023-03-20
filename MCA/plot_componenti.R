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