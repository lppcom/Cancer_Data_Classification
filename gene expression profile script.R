library("Biobase")
library("ALL")
library("genefilter")
data("ALL")
library("RColorBrewer") 
library("MLInterfaces")
library("annotate")
library("hgu95av2.db")




 syms = as.character(hgu95av2SYMBOL[featureNames(ALL)])
 whFeat = names(which(syms =="CBFB"))
 ordSamp = order(ALL$mol.biol)

# filter out a specific gene and plot its expression profile wrt probes that map to it

 CD44 = ALL[whFeat[1], ordSamp]

 plot(as.vector(exprs(CD44)),main="Gene: CBFB      probe:41175_at",ylab="Gene Expression", xlab="Patient Number",)


