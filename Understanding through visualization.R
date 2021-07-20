library(DESeq2)
library(pheatmap)
library(ggplot2)
library(gganimate)



dds <- makeExampleDESeqDataSet(m=6,betaSD=1)

countmatrix_raw <- assay(dds)

pheatmap(countmatrix)

sample1 <- data.frame(assay(dds)[,1])
colnames(sample1) <- "C1"

ggplot(sample1, aes(x=C1)) + geom_histogram()
























##MinMax Normalization 
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

pheatmap(min_max_norm(countmatrix+1))

##Log10 Normalization 
pheatmap(log10(countmatrix+1))

##Squareroot Normalization 
pheatmap(sqrt(countmatrix))

##Cube root Normalization 
pheatmap((countmatrix^(0.333)))

##PolyRoot Normalization 
pheatmap((countmatrix^(0.1)))

##PolyRoot Normalization + Center Scaling
pheatmap(scale((countmatrix^(0.1))))

#rlog normalization 
dds_rlog <- rlog(dds)
countmatrix_raw <- assay(dds_rlog)
pheatmap(countmatrix_raw)


hist(countmatrix[,1])
hist(log2(countmatrix[,1]+1))
hist((countmatrix[,1])^(0.2))

###############################################################
###############################################################

library(ggplot2)
library(gganimate)

plotdata1 <- data.frame(assay(dds)[,1])
colnames(plotdata1) <- "C1"

ggplot(plotdata1, aes(x=C1)) + geom_histogram()

###############################################################

plotdata_log10 <- log10(plotdata1+1)

ggplot(plotdata_log10, aes(x=C1)) + geom_histogram()

###############################################################

##Squareroot Normalization 
pheatmap(sqrt(countmatrix))
