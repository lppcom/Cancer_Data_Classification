library("randomForest")
library("MLInterfaces")
library("genefilter")
set.seed(123)
library("ALL")
data(ALL)

moltyp = which(as.character(ALL$mol.biol)
%in% c("NEG", "BCR/ABL"))
ALL_bcrneg = ALL[, moltyp]
ALL_bcrneg$mol.biol = factor(ALL_bcrneg$mol.biol)





 Negs = which(ALL_bcrneg$mol.biol == "NEG")
 Bcr = which(ALL_bcrneg$mol.biol == "BCR/ABL")
 S1 = sample(Negs, 45, replace=FALSE)
 S2 = sample(Bcr, 25, replace = FALSE)
 TrainInd = c(S1, S2)
TestInd = setdiff(0:110, TrainInd)

ptm <- proc.time()


Traintt = rowttests(ALL_bcrneg[, TrainInd], "mol.biol")
ordTT = order(abs(Traintt$statistic), decreasing=TRUE)
fNtt = featureNames(ALL_bcrneg)[ordTT[1:50]]

BNf = ALL_bcrneg[fNtt,]



rf1 = MLearn( mol.biol~., data=BNf,
randomForestI, TrainInd, ntree=10000)

timetaken=proc.time() - ptm



cfR=confuMat(rf1, "test")
 MAE = (cfR[2,1] + cfR[1,2])/sum(cfR)

confuMat(rf1, "test")
timetaken
MAE

