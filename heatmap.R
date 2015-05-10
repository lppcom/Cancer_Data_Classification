library("Biobase")
library("ALL")
library("genefilter")
 data("ALL")
library("RColorBrewer") 
library("MLInterfaces")

 bcell = grep("^B", as.character(ALL$BT))
 moltyp = which(as.character(ALL$mol.biol)
%in% c("NEG", "ALL1/AF4"))
 ALL_bcrneg = ALL[, intersect(bcell, moltyp)]
 ALL_bcrneg$mol.biol = factor(ALL_bcrneg$mol.biol)
ALLfilt_bcrneg = nsFilter(ALL_bcrneg, var.cutoff=0.1)$eset


 GOTFfun = function(GOID) {
x = hgu95av2GO2ALLPROBES[[GOID]]
unique(x[ names(x) != "IEA"])
}
 GOIDs = c("GO:0003700", "GO:0003702", "GO:0003709",
"GO:0016563", "GO:0016564")
 TFs = unique(unlist(lapply(GOIDs, GOTFfun)))


#Filter out genes for transcription factors factors taken for heatmap(reducing genes under study)
#Transcription factor activity, activator activity, repressor activity etc
#This is to be modified for better results in machie learning



 inSel = match(TFs, featureNames(ALLfilt_bcrneg), nomatch=0)
 es2 = ALLfilt_bcrneg[inSel,]
 iqrs = esApply(es2, 1, IQR)
 gvals = scale(t(exprs(es2)), rowMedians(es2),
iqrs[featureNames(es2)])
 manDist = dist(gvals, method="manhattan")
 hmcol = colorRampPalette(brewer.pal(10, "RdBu"))(256)
 hmcol = rev(hmcol)
 heatmap(as.matrix(manDist), sym=TRUE, col=hmcol,
distfun=function(x) as.dist(x)) 


