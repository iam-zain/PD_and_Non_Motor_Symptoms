library("DESeq2")

#################       Male       ####################
setwd('S:\\PPMI_RNA\\Processed\\Male')
# Only healthy vs Patient
countData <- read.csv('All_Counts_PatAndHealthy_Male.csv', header = TRUE, sep = ",")
head(countData[1:6, 1:8])

metaData <- read.csv('metaDataIR3_OnlyPat_Healthy_PATNO_Filter_Male.csv', header = TRUE, sep = ",")
head(metaData)

# Construct DESEQDataSet Object
dds <- DESeqDataSetFromMatrix(countData=countData, colData=metaData,design=~DIAGNOSIS, tidy = TRUE)

dds # view details of dds

dds <- DESeq(dds) # run DESEQ function # time taken step

res <- results(dds)
head(results(dds, tidy=TRUE)) #let's look at the results table
summary(res) #summary of results

res_list = results(dds, tidy=TRUE) #let's look at the results table
write.csv(res_list, 'DEGs_Result_Male.csv', row.names = F)
# Visualize any particular geneid count plot
#plotCounts(dds, gene="ENSG00000263590.2", intgroup="DIAGNOSIS")

# Make a basic volcano plot
# Increase font size and make it bold
par(font.axis = 2, font.lab = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
with(res, plot(log2FoldChange, -log10(pvalue), pch=20, main="DEGs of PPMI Data: Patient Vs Healthy Control: Male", xlim=c(-1.5,1.5)))
# Add colored points: blue if padj<0.01, red if log2FC>1 and padj<0.05)
with(subset(res, padj<.01 ), points(log2FoldChange, -log10(pvalue), pch=20, col="#F2CC8F"))
with(subset(res, padj<.01 & abs(log2FoldChange)>0.25), points(log2FoldChange, -log10(pvalue), pch=20, col="#36C3D1")) #Upregulate
with(subset(res, padj<.01 & log2FoldChange < -0.25), points(log2FoldChange, -log10(pvalue), pch=20, col="#F1746B")) #Downregulate

# Add labels to points with specified colors
#labels1 <- subset(res, pvalue<.05 & log2FoldChange > 0.5)
#text(labels1$log2FoldChange, -log10(labels1$pvalue), labels=rownames(labels1), pos=4, col="#36C3D1", cex=0.8)

#labels2 <- subset(res, pvalue<.05 & log2FoldChange < -0.5)
#text(labels2$log2FoldChange, -log10(labels2$pvalue), labels=rownames(labels2), pos=4, col="#F1746B", cex=0.8)


# PCA
vsdata <- vst(dds, blind=FALSE)
plotPCA(vsdata, intgroup="DIAGNOSIS") #using the DESEQ2 plotPCA fxn we can

# PCA of only up or down regulated
# Subset the data for upregulated and downregulated genes
upregulated_genes <- subset(res, padj < 0.01 & abs(log2FoldChange) > 0.25)
downregulated_genes <- subset(res, padj < 0.01 & log2FoldChange < -0.25)
selected_genes <- rbind(upregulated_genes, downregulated_genes)

# Create a new DESeqDataSet with the selected genes
dds_selected <- dds[row.names(dds) %in% row.names(selected_genes),]

# Perform variance stabilizing transformation (VST) directly
vsdata_selected <- varianceStabilizingTransformation(dds_selected)

# Plot PCA for the selected genes
plotPCA(vsdata_selected, intgroup = "DIAGNOSIS")

