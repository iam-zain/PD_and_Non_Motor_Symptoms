############################################################
# WGCNA Network Construction and Trait Correlation Script
#
# Aim:
#   - Construct gene co-expression networks from normalized RNA-seq data
#   - Identify co-expression modules and calculate module eigengenes
#   - Correlate modules with clinical traits (e.g., UPSIT, Diagnosis)
#   - Identify hub genes and gene–trait associations
#
# Input:
#   - Input_for_Network.RData (processed RNA-seq counts + phenotype data)
#
# Expected Output:
#   - Input_for_bwnet.RData (WGCNA blockwiseModules object and inputs)
#   - Gene_clustering_plot.svg (TOM-based gene clustering dendrogram)
#   - Module_dendrogram_plot.svg (dendrogram with module assignments)
#   - All_colNames_Heatmap.csv (list of heatmap column names)
#   - Module_membership_measure_pvals.csv (hub gene statistics)
#   - Gene_signif_corr_pvals_<Trait>_BL.csv (per-trait gene significance results)
#   - module_trait_correlation_heatmap.svg (module–trait correlation heatmap)
#
# This script performs module detection and associates modules 
# with traits, producing hub gene statistics and visualization outputs.
############################################################









# Loading the data (although not required)
#lnames = load(file = './zain/WGCNA/Input_for_Network.RData')
setwd('S:\\Scripts\\Scripts_R\\RScripts_PPMI')
lnames = load(file = 'Input_for_Network.RData')

library(DESeq2)
library(WGCNA)
library(dplyr)

data_subset = t(datExpr)
# 4. Network Construction ------------------------------------------------------
# choose a set of soft threshold powers
power <- c(c(1:10), seq(from = 12, to = 50, by = 2))
# 1  2  3  4  5  6  7  8  9 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50

# create dds
dds <- DESeq2::DESeqDataSetFromMatrix(countData = data_subset, colData = colData, design = ~1) # not specifying model
summary(data_subset)

# remove genes with counts < 15 in more than 75% of samples (497*0.75 = 373)
# suggested by WGCNA on RNASeq FAQ
dds75 <- dds[rowSums(counts(dds) >= 200) >= 497,]
nrow(dds75) # 3651 genes

# perform variance stabilization
dds_norm <- DESeq2::vst(dds75)

# get normalized counts
# now gene id should be in the columns, so transpose
norm_counts <- assay(dds_norm) %>% t()


# pick power
adjacency = WGCNA::adjacency(datExpr, power = 14)
## To minimize effects of noise and spurious associations, we transform the adjacency into 
## Topological Overlap Matrix (TOM)
TOM = WGCNA::TOMsimilarity(adjacency)

# calculate corresponding dissimilarity
dissTOM = 1-TOM

## Hierarchical clustering
# gene tree
geneTree = fastcluster::hclust(as.dist(dissTOM), method = 'average')
# plot
sizeGrWindow(12, 9)
# Save the plot to an SVG file
svg("Gene_clustering_plot.svg", width = 12, height = 9)

# Plot the gene tree
plot(geneTree, xlab = '', sub = '', main = 'Gene clustering on TOM-based dissimilarity',
     labels = FALSE, hang = 0.04)

# Turn off the device
dev.off()


# convert matrix to numeric
norm_counts[] <- sapply(norm_counts, as.numeric)

# it will suggest which power to chose
#sft[['powerEstimate']]

soft_power <- 14 # choose the power
# just make sure cor function uses WGCNA cor function and not from other packages
temp_cor <- cor
cor <- WGCNA::cor

## memory estimate w.r.t blocksize
bwnet <- WGCNA::blockwiseModules(norm_counts, maxBlockSize = 20000, TOMType = 'signed', power = soft_power,
                          mergeCutHeight = 0.25, numericLabels = F, randomSeed = 1, verbose = 3)
save(bwnet, data_subset, colData, file = "Input_for_bwnet.RData")

# mergeCutHeight = threshold to merge similar modules
# numericLabels = F = color names, instead of numbers 

cor <- temp_cor


# 5. Module Eigengenes  --------------------------------------------------------
module_eigengenes <- bwnet$MEs

# Print out a preview
head(module_eigengenes)

# get number of genes for each module
table(bwnet$colors)

# plot the dendogram
svg("Module_dendrogram_plot.svg", width = 12, height = 9)

WGCNA::plotDendroAndColors(bwnet$dendrograms[[1]], cbind(bwnet$unmergedColors, bwnet$colors),
                    c('Unmerged', 'Merged'),
                    dendroLabels = F, addGuide = T, hang = 0.03, guideHang = 0.05)
dev.off()


# 6. relate module to traits  --------------------------------------------------

# change disease state to binary from character
traits <- colData %>% dplyr::mutate(disease_state = ifelse(grepl(1, Diagnosis), 1, 0)) %>% dplyr::select(c(2:ncol(colData)))

# define number of samples and genes
nSamples <- nrow(norm_counts)
nGenes <- ncol(norm_counts)

module_trait_corr <- cor(module_eigengenes, traits, use = 'p')
module_trai_corr_pvals <- WGCNA::corPvalueStudent(module_trait_corr, nSamples)

# heatmap
heatmap_data <- merge(module_eigengenes, traits, by = 'row.names')
heatmap_data <- heatmap_data %>% column_to_rownames(var = 'Row.names')

# x will have trait data
# y will have all eigengenes names
names(heatmap_data[11])
col_names <- names(heatmap_data)
write.csv(data.frame(ColumnNames = col_names), "./zain/WGCNA/All_colNames_Heatmap.csv", row.names = FALSE)



# intra-molecular hub genes
module_membership_measure <- cor(module_eigengenes, norm_counts, use = 'p')
module_membership_measure_pvals <- WGCNA::corPvalueStudent(module_membership_measure, nSamples)
module_membership_measure_pvals <- t(module_membership_measure_pvals)

# calculate the gene significance & associated p-values with Disease condition
gene_signif_corr <- cor(norm_counts, traits$disease_state, use = 'p')
gene_signif_corr_pvals <- WGCNA::corPvalueStudent(gene_signif_corr, nSamples)

# view top 15 genes
gene_signif_corr_pvals %>%
  as.data.frame() %>%
  arrange(V1) %>%
  head(15)


write.csv(module_membership_measure_pvals, './zain/WGCNA/Module_membership_measure_pvals.csv')
#write.csv(gene_signif_corr_pvals, 'Gene_signif_corr_pvals_Diagnosis_Female.csv')



## For each and every trait 
colnames(traits)
# Define list of columns
columns_to_process <- c("UPSIT_Total", "Diagnosis"  )

# Define a function to process each column
process_column <- function(column_name) {
  # Compute correlation
  gene_signif_corr <- cor(norm_counts, traits[[column_name]], use = 'p')
  
  # Compute p-values
  gene_signif_corr_pvals <- WGCNA::corPvalueStudent(gene_signif_corr, nSamples)
  
  # Convert to data frame and arrange
  gene_signif_corr_pvals_df <- gene_signif_corr_pvals %>%
    as.data.frame() %>%
    arrange(V1)
  
  # Save the results
  output_file <- paste0('./zain/WGCNA/Gene_signif_corr_pvals_', column_name, '_BL.csv')
  write.csv(gene_signif_corr_pvals_df, output_file, row.names = T)
}

# Apply the function to each column
lapply(columns_to_process, process_column)


svg("./zain/WGCNA/module_trait_correlation_heatmap.svg", width = 12, height = 10)
CorLevelPlot::CorLevelPlot(heatmap_data, x = names(heatmap_data)[30:75], y = names(heatmap_data)[1:29],
                           col = c('#0d7d97', '#99c6cc', 'grey', '#ff5a5e', '#c31e23'), rotLabX = 90, cexCorval = 0.7)
dev.off()
