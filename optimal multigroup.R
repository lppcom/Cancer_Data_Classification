F=c( "1007_s_at","1249_at","1467_at","1635_at","1636_g_at","1859_s_at", "32310_f_at" 

  ,"32542_at" ,  "32562_at"  , "32612_at" ,  "32979_at" ,  "330_s_at" ,  "33362_at", "33385_g_at" 

  , "33408_at" ,  "33462_at" ,  "33823_at"  , "33997_at"  , "34362_at", "35162_s_at"  , "35626_at" 

  ,"35831_at" ,  "35912_at"  , "35951_at",   "36275_at" ,  "36536_at" ,  "36591_at" ,  "36638_at"

 , "36643_at" ,  "37015_at" ,  "37043_at" ,  "37093_at",   "37363_at"  , "37600_at" ,  "37762_at" 
 
 , "38052_at",   "38385_at",   "38518_at",   "39372_at",   "39631_at",   "39730_at",  "39824_at" 

,"39837_s_at",   "40019_at", "40132_g_at" ,"40167_s_at" ,  "40202_at" ,  "40504_at"  , "40855_at" 

  ,"40953_at"  , "41071_at",  "41257_at"  ,   "879_at" )



library("randomForest")
library("MLInterfaces")
library("genefilter")
library(class)



types = c("BCR/ABL", "NEG", "ALL1/AF4")
threeG = ALL$mol.biol %in% types
ALL3gf = ALL[,threeG]
ALL3gf$mol.biol = factor(ALL3gf$mol.biol)
ALL3gf=ALL3gf[F,]

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
trainSet$mol.biol,k=1,l=0)
tab1 = table(knn1MV, testSet$mol.biol)
tab1
s3 = table(testSet$mol.biol)


