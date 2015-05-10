library("Biobase")
library("ALL")
library("genefilter")
 data("ALL")
library("MLInterfaces")



F=c( "1007_s_at","1249_at","1467_at","1635_at","1636_g_at","1859_s_at", "32310_f_at" 

  ,"32542_at" ,  "32562_at"  , "32612_at" ,  "32979_at" ,  "330_s_at" ,  "33362_at", "33385_g_at" 

  , "33408_at" ,  "33462_at" ,  "33823_at"  , "33997_at"  , "34362_at", "35162_s_at"  , "35626_at" 

  ,"35831_at" ,  "35912_at"  , "35951_at",   "36275_at" ,  "36536_at" ,  "36591_at" ,  "36638_at"

 , "36643_at" ,  "37015_at" ,  "37043_at" ,  "37093_at",   "37363_at"  , "37600_at" ,  "37762_at" 
 
 , "38052_at",   "38385_at",   "38518_at",   "39372_at",   "39631_at",   "39730_at",  "39824_at" 

,"39837_s_at",   "40019_at", "40132_g_at" ,"40167_s_at" ,  "40202_at" ,  "40504_at"  , "40855_at" 

  ,"40953_at"  , "41071_at",  "41257_at"  ,   "879_at" )




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



BNf = ALL_bcrneg[F,]

knnf = MLearn( mol.biol ~ ., data=BNf, .method=ldaI, 
    TrainInd)
 confuMat(knnf)


cfKNN = confuMat(knnf)
(cfKNN[1,2] + cfKNN[2,1])/sum(cfKNN)


confuMat(knnf)
