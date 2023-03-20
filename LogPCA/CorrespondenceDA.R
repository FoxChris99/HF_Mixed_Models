
df=com_pat[,-c( 1,5,7,19)]
df=df[,-c(1,2,17,6,10,11,5,13,14)]
label <- factor(com_pat$labelOUT, labels=c('DECEDUTO','TRONCATO'))

library(TExPosition)

label_matrix = data.frame(Troncato = label=="TRONCATO", Deceduto = label=="DECEDUTO")
label_matrix[label!="TRONCATO",1] = 0
label_matrix[label!="DECEDUTO",2] = 0

dica.res <- tepDICA(df,DESIGN=label_matrix,graphs = FALSE, k = 1)

dica.res$TExPosition.Data$assign$confusion
dica.res$TExPosition.Data$assign$confusion[1,2]/sum(dica.res$TExPosition.Data$assign$confusion[1,]) *100
dica.res$TExPosition.Data$assign$confusion[2,2]/sum(dica.res$TExPosition.Data$assign$confusion[2,]) *100
