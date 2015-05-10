library("Biobase")
library("ALL")
data(ALL)


library("genefilter")

library("MLInterfaces")

library("RColorBrewer")
bcell = grep("^B", as.character(ALL$BT))
moltyp = which(as.character(ALL$mol.biol)
%in% c("NEG", "ALL1/AF4"))
ALL_bcrneg = ALL[, intersect(bcell, moltyp)]
ALL_bcrneg$mol.biol = factor(ALL_bcrneg$mol.biol)
ALLfilt_bcrneg = nsFilter(ALL_bcrneg, var.cutoff=0.75)$eset


iqrs = esApply(es2, 1, IQR)
gvals = scale(t(exprs(es2)), rowMedians(es2),
iqrs[featureNames(es2)])
manDist = dist(gvals, method="manhattan")

 hc1 = hclust(manDist)
 hc2 = hclust(manDist, method="single")
hc3 = hclust(manDist, method="ward")
 hc4 = diana(manDist)


plot(hc4, ann=FALSE, which.plots=2)

 title(main="Divisive Clustering", cex.main=2)
 par(mfrow=c(1,1))
