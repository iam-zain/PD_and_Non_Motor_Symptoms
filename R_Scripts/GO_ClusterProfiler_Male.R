
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Xtras")
## Details available on the link below ##
## https://yulab-smu.top/biomedical-knowledge-mining-book/enrichplot.html 

library(DOSE)
library(enrichR)
library(ggupset)
library(biomaRt)
library(gProfileR)
library(ReactomePA)
library(enrichplot)
library(ggnewscale)
library(org.Hs.eg.db)
library(clusterProfiler)




#####  NMI Above 0.08  ####
df <- read.delim("CpG_NMI.08_Male_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.05,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot






#####  Top 50 from important Feats  ####
df <- read.delim("All11_Top50sCpG_Male_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.05)
barplot(myEnrich, showCategory = 25)
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE, node_label="all", showCategory = 10)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot



#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.05,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all",  showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot





#####  Common in 11 Feats and NMI Chromosome  ####
df <- read.delim("CpG_Common_inNMIChr_11Feats_Male_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
myEnrichName1 <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName1, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all")
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot


#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.5,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, node_label="all",showCategory = 10, color_category='firebrick', color_gene='steelblue') #Cnet Plot
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot






#####  NMI Above 0.07  ####
df <- read.delim("CpG_NMI.07_Male_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))
GeneID <- as.data.frame(na.omit(GeneID))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.5)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName1 <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName1, node_label="all", color_category='firebrick', color_gene='steelblue') #Cnet Plot 
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot
#No results found in disease pathway

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID, organism = "human", pvalueCutoff = 0.5, pAdjustMethod = "BH", minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, node_label="all",showCategory = 10, color_category='firebrick', color_gene='steelblue') #Cnet Plot
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot







#####  NMI Above 0.06  ####
df <- read.delim("CpG_NMI.06_Male_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))
GeneID <- as.data.frame(na.omit(GeneID))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.9)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName1 <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName1, node_label="all", color_category='firebrick', color_gene='steelblue') #Cnet Plot 
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.2,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, node_label="all",showCategory = 10, color_category='firebrick', color_gene='steelblue') #Cnet Plot
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot







#####  UPSIT - Unique in Male  ####
setwd ('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG')
df <- read.delim("Gene_Unique_UPSIT_Male.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.05,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot









#####  Cognition - Unique in Male  ####
setwd ('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\GeneCpG\\Common_Male_Female\\Cognition')
df <- read.delim("Gene_Unique_Cognition_Male.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.6)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.6,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot
