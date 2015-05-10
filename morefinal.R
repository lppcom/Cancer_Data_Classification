library("randomForest")
library("MLInterfaces")
library("genefilter")


library(GEOquery)
library(Biobase)
GDS <- getGEO(filename='GDS4136.soft')


 eset <- GDS2eSet(GDS, do.log2=FALSE)








set.seed(123)



ptm <- proc.time()




rf1 = randomForest(x=t(exprs(eset)),y=pData(eset)$"disease.state",ntree=10)


timetaken=proc.time() - ptm
varImpPlot(rf1,sort=TRUE,n.var=min(20,nrow(rf1$importance)))

syms = as.character(hgu95av2SYMBOL["40202_at"])


plot(rf1$importance)
