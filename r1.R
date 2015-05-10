library("Biobase")
library("ALL")
library("genefilter")
 data("ALL")
library("MLInterfaces")



moltyp = which(as.character(ALL$mol.biol)
%in% c("NEG", "BCR/ABL"))
ALL_bcrneg = ALL[,  moltyp]
ALL_bcrneg$mol.biol = factor(ALL_bcrneg$mol.biol)





Negs = which(ALL_bcrneg$mol.biol == "NEG")
Bcr = which(ALL_bcrneg$mol.biol == "BCR/ABL")
S1 = sample(Negs, 45, replace=FALSE)
S2 = sample(Bcr, 25, replace = FALSE)
TrainInd = c(S1, S2)
TestInd = setdiff(0:110, TrainInd)


Traintt = rowttests(ALL_bcrneg[, TrainInd], "mol.biol")
ordTT = order(abs(Traintt$statistic), decreasing=TRUE)
fNtt = featureNames(ALL_bcrneg)[ordTT[1:50]]

BNf = ALL_bcrneg[fNtt,]

knnf = MLearn( mol.biol ~ ., data=BNf, .method=knnI(1,0), 
    TrainInd)
 confuMat(knnf)


cfKNN = confuMat(knnf)
(cfKNN[1,2] + cfKNN[2,1])/sum(cfKNN)


confuMat(knnf)
