############################################################
# Enrichment Analysis of PULSE Genes
#
# Aim:
#   - Identify biological processes, pathways, and disease terms enriched
#     among PULSE genes (genes overlapping UPSIT and PD modules)
#   - Perform enrichment using:
#       1. Gene Ontology (GO: BP, MF, CC)
#       2. Disease Pathways (DisGeNET)
#       3. Reactome Pathways
#       4. KEGG Pathways
#   - Generate dot plots for visual interpretation
#   - Save detailed gene–term associations with p-values and FDR corrections
#
# Input:
#   - PULSE_Genes.csv (list of gene symbols)
#
# Expected Output:
#   - PULSE_GO.csv (significant GO terms with gene associations)
#   - PULSE_DisPath.csv (significant disease pathways with gene associations)
#   - PULSE_Reactome.csv (significant Reactome pathways with gene associations)
#   - PULSE_KEGG.csv (significant KEGG pathways with gene associations)
#   - Dot plots for each enrichment category
#
# This script provides functional insights into PULSE genes by
# linking them to biological processes, disease mechanisms,
# and molecular pathways.
############################################################









setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')

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


#  1. GO Enrich Terms (BP, MF,CC)  ---------------------------------------------

df <- read.delim("PULSE_Genes.csv", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05, ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
dotplot(GeneInfo, showCategory = 30, label_format = 60)
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL", keys = df, columns = c("SYMBOL","ENTREZID"))
# Filter for significant adjusted p-values (FDR ≤ 0.05)
significant_GO <- GeneInfo@result[GeneInfo@result$p.adjust <= 0.05, ]
# Extract relevant columns
#significant_GO_simple <- significant_GO[, c("ID", "Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "geneID")]
write.csv(significant_GO, "PULSE_GO.csv", row.names = FALSE)



# 2. Disease Enrichment Analysis  ----------------------------------------------
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.05)
dotplot(myEnrich, showCategory = 30, label_format = 60)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
#cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)

# Filter only significant results (adjusted p-value ≤ 0.05)
significant_disease <- myEnrichName@result[myEnrichName@result$p.adjust <= 0.05, ]

# Extract the gene-disease associations from significant terms
disease_gene_list <- data.frame(
  Disease = significant_disease$ID,
  Disease_name = significant_disease$Description,
  Genes = sapply(significant_disease$geneID, function(x) paste(unlist(strsplit(x, "/")), collapse = ", ")),
  pvalue = significant_disease$pvalue,
  p_adj = significant_disease$p.adjust,
  qvalue = significant_disease$qvalue
)


write.csv(disease_gene_list, "PULSE_DisPath.csv", row.names = FALSE)



#  3. Reactome Pathway Enrichment  ---------------------------------------------
myEnrichKegg = enrichPathway(GeneID$ENTREZID, organism = "human", pvalueCutoff = 0.05, pAdjustMethod = "BH", minGSSize = 2)
dotplot(myEnrichKegg, showCategory = 25, label_format = 60)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichPath <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
#cnetplot(myEnrichPath, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)

# Filter only significant results (adjusted p-value ≤ 0.05)
significant_pathways <- myEnrichPath@result[myEnrichPath@result$p.adjust <= 0.05, ]

# Extract the gene-pathway associations from significant terms
pathway_gene_list <- data.frame(
  Pathway = significant_pathways$ID,
  Pathway_name = significant_pathways$Description,
  Genes = sapply(significant_pathways$geneID, function(x) paste(unlist(strsplit(x, "/")), collapse = ", ")),
  pvalue = significant_pathways$pvalue,
  p_adj = significant_pathways$p.adjust,
  qvalue = significant_pathways$qvalue
)

write.csv(pathway_gene_list, "PULSE_Reactome.csv", row.names = FALSE)




#  4. KEGG Pathway Enrichment  -------------------------------------------------
myEnrichKEGG <- enrichKEGG(gene = GeneID$ENTREZID,
                           organism = "hsa", # "hsa" is the KEGG code for human
                           pvalueCutoff = 0.05,
                           pAdjustMethod = "BH",
                           minGSSize = 2)

# Visualize
dotplot(myEnrichKEGG, showCategory = 25, label_format = 60)

# Term similarity (optional, useful for clustering)
termsim_kegg <- pairwise_termsim(myEnrichKEGG)

# Make gene IDs readable (convert ENTREZ back to SYMBOL for clarity)
myEnrichKEGG_readable <- setReadable(myEnrichKEGG, 'org.Hs.eg.db', 'ENTREZID')

# Filter only significant results (adjusted p-value ≤ 0.05)
significant_kegg <- myEnrichKEGG_readable@result[myEnrichKEGG_readable@result$p.adjust <= 0.05, ]

# Extract the gene-pathway associations
kegg_gene_list <- data.frame(
  KEGG_ID = significant_kegg$ID,
  KEGG_Pathway = significant_kegg$Description,
  Genes = sapply(significant_kegg$geneID, function(x) paste(unlist(strsplit(x, "/")), collapse = ", ")),
  pvalue = significant_kegg$pvalue,
  p_adj = significant_kegg$p.adjust,
  qvalue = significant_kegg$qvalue
)

write.csv(kegg_gene_list, "PULSE_KEGG.csv", row.names = FALSE)
