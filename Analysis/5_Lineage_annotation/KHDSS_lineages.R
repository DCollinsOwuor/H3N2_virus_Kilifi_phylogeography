# Title: "Plotting H3N2 virus lineages by gene segment to visualize reassortant strains".
# Author: "D. Collins Owuor"
# Date:   "January 26, 2021"
# R version 1.3.1093 
# Clear data in memory, load necessary packages, and load data.

### Clear workspace:
rm(list=ls())

### Install and load required packages:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
BiocManager::install("treeio")
BiocManager::install("ggplot2")
BiocManager::install("Biostrings")

library(ggtree)
library(ggplot2)
library(Biostrings)
library(treeio)

### Import BEAST MCC.trees file:
bt <- read.beast("H3N2_KHDSS_2015-2016_WGS_GTRG_UncorrRelax_LogNorm_Bayesian_SkyGrid_BSSVS_400M_MCC.trees")

bt

### Visualize imported tree:
ggtree(bt)

ggtree(bt, mrsd = "2016-12-05") + theme_tree2()


### Visualize ggtree annotated with MRSD:
p <- ggtree(bt, mrsd = "2016-12-05") + theme_tree2() #+ scale_x_continuous(breaks = seq(from = 2013.0, to = 2017.0, length.out = 21)) 

ggsave(p, file="phylotree.pdf")

### Import metadata file for annotation of virus lineges:
metad <- read.table("Lineage_metadata.txt", header=T, sep="\t", stringsAsFactor=F, row.names = 1)

selected <- data.frame()

selected <- metad[0]

selected <- cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(selected, pb2=metad$pb2),pb1=metad$pb1), pa=metad$pa), ha=metad$ha), np=metad$np), na=metad$na), m=metad$m), ns=metad$ns) 

### Generate heat map for lineage classification:
p2 <- (p)%>%gheatmap(selected, offset=0 ,width=0.4, font.size=3.5, colnames_angle=45, hjust=1) + scale_fill_manual(breaks=c("3C.2A", "3C.2A1b", "3C.2A2", "3C.2A3"), values=c("3C.2A"="Red", "3C.2A1b"="Green", "3C.2A2"="Cyan", "3C.2A3"="Blue")) 

p2

### Import metadata file for annotation of tip lables:
meta <- read.table("Lineage_metadata.txt", header=TRUE, sep="\t", stringsAsFactor=F, check.names = F)

p3 <- (p2) %<+% meta + geom_tippoint(aes(color=Clade), size=2) + scale_color_manual(values=c("red","green","cyan","blue"))

p3
