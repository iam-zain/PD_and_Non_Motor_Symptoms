setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association")
library(ReactomePA)
library(org.Hs.eg.db)
library(clusterProfiler)
library(enrichplot)
library(DOSE)
library(ggnewscale)
library(ggupset)

df <- read.delim("tempGeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.01,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

myEnrich <- enrichDGN(GeneID$ENTREZID) #Disease Pathway
barplot(myEnrich, showCategory = 20)
termsim<-pairwise_termsim(myEnrich) #Similarity Matrix
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

myEnrichKegg <- enrichKEGG(GeneID$ENTREZID, organism = "hsa", keyType = "ncbi-geneid",pvalueCutoff = 0.1) #KEGGPathway
barplot(myEnrichKegg, showCategory = 20)
termsim1<-pairwise_termsim(myEnrichKegg)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot

#Visualizing using Cnet Plot
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, node_label="all", color_category='firebrick', color_gene='steelblue') 

myEnrichName1 <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName1, node_label="all", color_category='firebrick', color_gene='steelblue') 

# BarPlot plotting using ggplot
ggplot(myEnrichKegg@result[1:10,], aes(y=reorder(Description,-Count),x=Count,fill= p.adjust))+geom_bar(stat = "identity")

#Reactome Pathway Analysis
x <- enrichPathway(gene=GeneID$ENTREZID, pvalueCutoff = 0.05, readable=TRUE)
barplot(x, showCategory = 5)

## Details available on the link below ##
## https://yulab-smu.top/biomedical-knowledge-mining-book/enrichplot.html 
