library("randomForest")
library("MLInterfaces")
library("genefilter")


set.seed(123)
library("ALL")
data(ALL)
library("annotate")
library("hgu95av2.db")
moltyp = which(as.character(ALL$mol.biol)
%in% c("NEG", "BCR/ABL"))
ALL_Filter = ALL[, moltyp]
ALL_Filter$mol.biol = factor(ALL_Filter$mol.biol)


ptm <- proc.time()




rf1 = randomForest(x=t(exprs(ALL_Filter)),y=ALL_Filter$mol.biol,ntree=10)


timetaken=proc.time() - ptm
varImpPlot(rf1,sort=TRUE,n.var=min(20,nrow(rf1$importance)))

syms = as.character(hgu95av2SYMBOL["40202_at"])


