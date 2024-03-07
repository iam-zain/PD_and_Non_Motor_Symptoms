
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Xtras")

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


#####  NMI Above 0.15  ####
df <- read.delim("CpG15_Female_GeneList.txt", header = F) #Gene List in text file
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
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.2,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot





#####  Common from 5 Feats  ####
df <- read.delim("All5_Top50sCpG_Female_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID", "UNIPROT"))

gene=as.vector(na.omit(GeneID$ENTREZID))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.05)
barplot(myEnrich, showCategory = 25)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE, node_label="all", showCategory = 10)
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





#####  Common in 5 Feats and NMI Chromosome  ####
df <- read.delim("CpG_Common_inNMIChr_5Feats_Female_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE, node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.2,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot




#####  NMI Above 0.13  ####
df <- read.delim("CpG13_Female_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))
GeneID <- as.data.frame(na.omit(GeneID))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE, node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.2,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot






#####  NMI Above 0.12  ####
df <- read.delim("CpG12_Female_GeneList.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))
GeneID <- as.data.frame(na.omit(GeneID))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE, node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.2,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot





#####  UPSIT - Unique in Female  ####
setwd ('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG')
df <- read.delim("Gene_Unique_UPSIT_Female.txt", header = F) #Gene List in text file
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
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.1,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot








#####  Cognition - Unique in Female  ####
setwd ('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG')
df <- read.delim("Gene_Unique_Cognition_Female.txt", header = F) #Gene List in text file
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
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.6,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot
