library("randomForest")
library("MLInterfaces")
library("genefilter")
library(class)



types = c("BCR/ABL", "NEG", "ALL1/AF4")
threeG = ALL$mol.biol %in% types
ALL3gf = ALL[,threeG]
ALL3gf$mol.biol = factor(ALL3gf$mol.biol)

s1 = table(ALL3gf$mol.biol)
trainN = ceiling(s1/1.4)
sN = split(1:length(ALL3gf$mol.biol), ALL3gf$mol.biol)
trainInd = NULL
testInd = NULL
set.seed(777)
for(i in 1:3) {
trI = sample(sN[[i]], trainN[[i]])
teI = setdiff(sN[[i]], trI)
trainInd = c(trainInd, trI)
testInd = c(testInd, teI)
}

trainSet = ALL3gf[, trainInd]
testSet = ALL3gf[, testInd]




knn1MV = knn(t(exprs(trainSet)), t(exprs(testSet)),
trainSet$mol.biol,k=6,l=0)
tab1 = table(knn1MV, testSet$mol.biol)
tab1
s3 = table(testSet$mol.biol)


