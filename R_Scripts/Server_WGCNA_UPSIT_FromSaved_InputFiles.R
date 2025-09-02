############################################################
# WGCNA UPSIT Module–Trait Correlation Script
#
# Aim:
#   - Load pre-processed RNA-seq expression data and WGCNA network object
#   - Extract module eigengenes and map genes to modules
#   - Correlate modules with UPSIT scores and Diagnosis
#   - Identify hub genes and compute gene significance p-values
#   - Export module-specific gene lists and trait association results
#
# Input:
#   - Input_for_Network.RData (processed RNA-seq counts + phenotype data)
#   - Input_for_bwnet.RData (WGCNA blockwiseModules object)
#
# Expected Output:
#   - Module_<Color>_genes.csv (list of genes for each detected module)
#   - Module_membership_measure_pvals.csv (hub gene statistics)
#   - Gene_signif_corr_pvals_UPSIT.csv (gene significance vs UPSIT)
#   - Gene_signif_corr_pvals_Diagnosis.csv (gene significance vs Diagnosis)
#   - Gene_signif_corr_pvals_<Trait>_BL.csv (per-trait significance results)
#   - Correlation heatmaps (via CorLevelPlot)
#
# This script produces gene–module assignments and statistical associations 
# with UPSIT smell scores and diagnosis, aiding biological interpretation.
############################################################









# Loading the data
#lnames = load(file = './zain/WGCNA/Input_for_Network.RData')
setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')
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
dds75 <- dds[rowSums(counts(dds) >= 150) >= 497,]
nrow(dds75) # 4710 genes

# perform variance stabilization
dds_norm <- DESeq2::vst(dds75)

# get normalized counts
# now gene id should be in the columns, so transpose
norm_counts <- assay(dds_norm) %>% t()


cnames = load(file = 'Input_for_bwnet.RData')


# 5. Module Eigengenes  --------------------------------------------------------
module_eigengenes <- bwnet$MEs

# Print out a preview
head(module_eigengenes)

# get number of genes for each module
table(bwnet$colors)

# Ensure gene names are accessible
gene_names <- colnames(norm_counts)  # assuming genes are in columns after normalization

# Create a data frame linking gene names to their module colors
gene_module_df <- data.frame(
  Gene = gene_names,
  Module = bwnet$colors
)

head(gene_module_df)

unique_modules <- unique(bwnet$colors)

for (mod in unique_modules) {
  # Subset genes for the current module
  mod_genes <- gene_module_df %>% filter(Module == mod)
  
  # Define filename using module name
  filename <- paste0("Module_", mod, "_genes.csv")
  
  # Save to CSV in the current working directory
  write.csv(mod_genes, file = filename, row.names = FALSE)
}

# plot the dendogram
WGCNA::plotDendroAndColors(bwnet$dendrograms[[1]], cbind(bwnet$unmergedColors, bwnet$colors),
                           c('Unmerged', 'Merged'),
                           dendroLabels = F, addGuide = T, hang = 0.03, guideHang = 0.05)


# 6. relate module to traits  --------------------------------------------------

# change disease state to binary from character
traits <- colData %>%
  dplyr::mutate(Diagnosis = ifelse(grepl(1, Diagnosis), 0, 1)) %>%
  dplyr::select(Diagnosis, UPSIT_Total)

# define number of samples and genes
nSamples <- nrow(norm_counts)
# 497

nGenes <- ncol(norm_counts)
# 4710

module_trait_corr <- cor(module_eigengenes, traits, use = 'p')
module_trai_corr_pvals <- WGCNA::corPvalueStudent(module_trait_corr, nSamples)

# heatmap
heatmap_data <- merge(module_eigengenes, traits, by = 'row.names')
heatmap_data <- heatmap_data %>% column_to_rownames(var = 'Row.names')

# x will have trait data
# y will have all eigengenes names
names(heatmap_data[12])
col_names <- names(heatmap_data)
#write.csv(data.frame(ColumnNames = col_names), "./zain/WGCNA/All_colNames_Heatmap.csv", row.names = FALSE)



# intra-molecular hub genes
module_membership_measure <- cor(module_eigengenes, norm_counts, use = 'p')
module_membership_measure_pvals <- WGCNA::corPvalueStudent(module_membership_measure, nSamples)
module_membership_measure_pvals <- t(module_membership_measure_pvals)

# calculate the gene significance & associated p-values with UPSIT score
gene_signif_corr <- cor(norm_counts, traits$UPSIT_Total, use = 'p')  # gives a named vector
gene_signif_corr_pvals <- WGCNA::corPvalueStudent(gene_signif_corr, nSamples)

# view top 15 genes
gene_signif_corr_pvals %>%
   as.data.frame() %>%
   arrange(V1) %>%
   head(15)

str(gene_signif_corr_pvals)

write.csv(module_membership_measure_pvals, 'Module_membership_measure_pvals.csv')
write.csv(gene_signif_corr_pvals, 'Gene_signif_corr_pvals_UPSIT.csv')



# calculate the gene significance & associated p-values with Diagnosis
gene_signif_corr <- cor(norm_counts, traits$Diagnosis, use = 'p')  # gives a named vector
gene_signif_corr_pvals <- WGCNA::corPvalueStudent(gene_signif_corr, nSamples)

# view top 15 genes
gene_signif_corr_pvals %>%
  as.data.frame() %>%
  arrange(V1) %>%
  head(15)

str(gene_signif_corr_pvals)

write.csv(gene_signif_corr_pvals, 'Gene_signif_corr_pvals_Diagnosis.csv')



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
  output_file <- paste0('S:/PPMI_RNA/Processed/WGCNA_UPSIT/Gene_signif_corr_pvals_', column_name, '_BL.csv')
  write.csv(gene_signif_corr_pvals_df, output_file, row.names = T)
}

# Apply the function to each column
lapply(columns_to_process, process_column)


CorLevelPlot::CorLevelPlot(
  heatmap_data,
  x = names(heatmap_data)[11:12],
  y = names(heatmap_data)[1:10],
  col = c('#0d7d97', '#99c6cc', 'grey', '#F1746B', '#c31e23'),
  rotLabX = 0,
  cexCorval = 1.5,
  cexLabX = 1.2,   # Increase X-axis label font size
  cexLabY = 1.2    # Increase Y-axis label font size
)

## Without cor values
CorLevelPlot::CorLevelPlot(
  heatmap_data,
  x = names(heatmap_data)[11:12],
  y = names(heatmap_data)[1:10],
  col = c('#0d7d97', '#99c6cc', 'grey', '#F1746B', '#c31e23'),
  rotLabX = 0,
  cexCorval = 0,     # Hide correlation values
  cexLabX = 1.2,     # Increase X-axis label font size
  cexLabY = 1.2      # Increase Y-axis label font size
)

