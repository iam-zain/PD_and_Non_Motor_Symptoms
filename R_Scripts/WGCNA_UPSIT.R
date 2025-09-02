############################################################
# WGCNA Script for Parkinson’s Disease Study
#
# Aim:
#   - Perform quality control on RNA-seq data (outlier detection, filtering)
#   - Normalize counts using DESeq2
#   - Construct weighted gene co-expression networks (WGCNA)
#   - Identify gene modules and correlate them with clinical traits (e.g., UPSIT, NMS)
#
# Input:
#   - All_Counts_Having_UPSIT.csv (raw RNA-seq count data with UPSIT info)
#   - metaDataIR3_BL_Having_RNA_UPSIT.csv (phenotypic metadata)
#
# Expected Output:
#   - Input_for_Network.RData (processed data for WGCNA)
#   - Module membership and hub gene statistics
#   - Correlation results of modules with traits
#   - Gene significance p-values for each clinical trait
#   - Multiple CSV files:
#       * Module_membership_measure_pvals_Female.csv
#       * Gene_signif_corr_pvals_<Trait>.csv (one file per trait)
#
# This script produces co-expression modules linked to smell loss 
# and other non-motor symptoms, supporting integrative biological analysis.
############################################################









library(WGCNA)
library(impute)
library(DESeq2)
library(gridExtra)
library(CorLevelPlot)



# 1. Fetch Data  ---------------------------------------------------------------
setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')
data_expr <- read.csv('All_Counts_Having_UPSIT.csv', header = T)
data_expr <- data_expr %>% column_to_rownames(var = 'Geneid') # changing 1st colum to rownames
# Fixing name of data_subset, removing X
names(data_expr) <- gsub('X', '', names(data_expr))
names(data_expr) <- gsub("\\.", "_", names(data_expr))
head(data_expr[1:4, 1:4])



# 2. QC - outlier  -------------------------------------------------------------
# detect outlier
# We need Gene info ass column names and rows as each sample
# So transpose the data
gsg <- goodSamplesGenes(t(data_expr), verbose = 3) # samples as row, genes as columns, so transorming
summary(gsg)
# check missing value
gsg$allOK

#badGenes <- which(!gsg$goodGenes)
#badSamples <- which(!gsg$goodSamples)

table(gsg$goodGenes) # number of genes detected as outliers
# FALSE  TRUE 
# 1250  57530  

table(gsg$goodSamples) # samples detected as outliers
# All 504 samples good to go

# remove outliers genes
data_expr <- data_expr[gsg$goodGenes == TRUE,]


# remove outlier sample -- hierraarchial clustering - method 1
## time taken step for big data
htree <- fastcluster::hclust(dist(t(data_expr)), method = 'average')
plot(htree)
## one detected
# 5104_SL_2477, 5104_SL_1558
# exclude sample (from plot visualization)
names(data_expr)
sample_exclude <- c('5104_SL_2477', '5104_SL_1558', '5104_SL_0607', '5104_SL_2187', 
                    '5104_SL_1566', '5104_SL_2355', '5104_SL_0021')
data_expr <- data_expr[, !(colnames(data_expr) %in% sample_exclude)]

# pca - method 2
pca <- prcomp (data_expr)
pca_data <- pca$x
pca_data <- as.data.frame(pca_data)
pca_var <- pca$sdev^2
pca_var_percent <- round(pca_var/sum(pca_var)*100, digits = 2)

ggplot(pca_data, aes(PC1, PC2)) + geom_point () + geom_text (label = rownames(pca_data)) +
  labs(x = paste0('PC1: ', pca_var_percent[1], ' %'),
       y = paste0('PC2: ', pca_var_percent[2], ' %'))

# exclude genes (from PCA plot visualization)
sample_exclude <- c('ENSG00000251562.8', 'ENSG00000274012.1')
data_subset <- data_expr[!(rownames(data_expr) %in% sample_exclude), ]


# Load phenotypic data
phenodata <- read.csv('metaDataIR3_BL_Having_RNA_UPSIT.csv')
phenodata <- phenodata[, -c(2)]# removing PATNO column



# 3. Normalization -------------------------------------------------------------
# create DEseq2 dataset

colData <- phenodata %>% filter(!row.names(.) %in% sample_exclude)
names(colData)
colData <- colData %>% column_to_rownames(var = 'HudAlphaID') # changing 1st column to rownames
sample_exclude <- c('5104_SL_2477', '5104_SL_1558', '5104_SL_0607', '5104_SL_2187', 
                    '5104_SL_1566', '5104_SL_2355', '5104_SL_0021')
colData <- colData[!(rownames(colData) %in% sample_exclude), ]



# check names matching or not
all(rownames(colData) %in% colnames(data_subset))
all(rownames(colData) == colnames(data_subset))
## must return TRUE


# Recluster samples
sampleTree2 = fastcluster::hclust(dist(t(data_subset)), method = 'average')
# convert traits to color representation: white means low, red is high, grey means missing
traitColors = numbers2colors(colData, signed = F)
# Plot the sample dendogram & the colors underneath
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = names(colData),
                    main = 'Dendogram and Heatmap')

datExpr = t(data_subset)
save(datExpr, data_subset, colData, file = "Input_for_Network.RData")


# Loading the data (although not required)
lnames = load(file = 'Input_for_Network.RData')


data_subset = t(datExpr)
# 4. Network Construction ------------------------------------------------------
# choose a set of soft threshold powers
power <- c(c(1:10), seq(from = 12, to = 50, by = 2))
# 1  2  3  4  5  6  7  8  9 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50

# create dds
dds <- DESeqDataSetFromMatrix(countData = data_subset, colData = colData, design = ~1) # not specifying model
summary(data_subset)

# remove genes with counts < 15 in more than 75% of samples (497*0.75 = 373)
# suggested by WGCNA on RNASeq FAQ
dds75 <- dds[rowSums(counts(dds) >= 15) >= 373,]
nrow(dds75) # 17358 genes

# perform variance stabilization
dds_norm <- vst(dds75)

# get normalized counts
# now gene id should be in the columns, so transpose
norm_counts <- assay(dds_norm) %>% t()

# Call the network topology function analysis
sft <- pickSoftThreshold(norm_counts, powerVector = power, networkType = 'signed', verbose = 5)
sft_data <- sft$fitIndices
# we will use r-square and mean connectivity
sft[['powerEstimate']]
# it suggests to use power of 42, it is very much high
# we will look in the plot then decide

# we need that which gives max R-Square value and minimum mean connectivity
# visualization to pick power
a1 <- ggplot(sft_data, aes(Power, SFT.R.sq, label = Power)) + 
  geom_point() + 
  geom_text(nudge_y = 0.1) + # labels slightly above
  geom_hline(yintercept = 0.8, color = '#F1746B') +
  labs(x = 'Power', y = 'Scale free topology model fit, signed R^2') + 
  theme_classic() +
  theme(panel.grid.major = element_line(color = "lightgrey", size = 0.3),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.2))


a2 <- ggplot(sft_data, aes(Power, mean.k., label = Power)) + 
  geom_point() + 
  geom_text(nudge_x = 1) +
  geom_hline(yintercept = 0.8, color = '#F1746B') +
  labs(x = 'Power', y = 'Mean connectivity') + 
  theme_classic() +
  theme(panel.grid.major = element_line(color = "lightgrey", size = 0.3),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.2))

grid.arrange(a1, a2, nrow = 2)


# pick power
adjacency = adjacency(datExpr, power = 14)
## To minimize effects of noise and spurious associations, we transform the adjacency into 
## Topological Overlap Matrix (TOM)
TOM = TOMsimilarity(adjacency)

# calculate corresponding dissimilarity
dissTOM = 1-TOM

## Hierarchical clustering
# gene tree
geneTree = hclust(as.dist(dissTOM), method = 'average')
# plot
sizeGrWindow(12, 9)
plot(geneTree, xlab = '', sub = '', main = 'Gene clustering on TOM-based dissimilarity',
     labels = F, hang = 0.04)



# visualize the graph, chose a soft threshold that is above the red line, but not much far
# red line shows 0.8 R_Square. Chose something after that prefer less powered numbers
# therefore, were are choosing power __; red line crossing from __

# convert matrix to numeric
norm_counts[] <- sapply(norm_counts, as.numeric)

# it will suggest which power to chose
#sft[['powerEstimate']]

soft_power <- 14 # choose the power
# just make sure cor function uses WGCNA cor function and not from other packages
temp_cor <- cor
cor <- WGCNA::cor

## memory estimate w.r.t blocksize
bwnet <- blockwiseModules(norm_counts, maxBlockSize = 20000, TOMType = 'signed', power = soft_power,
                          mergeCutHeight = 0.25, numericLabels = F, randomSeed = 1, verbose = 3)
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
plotDendroAndColors(bwnet$dendrograms[[1]], cbind(bwnet$unmergedColors, bwnet$colors),
                    c('Unmerged', 'Merged'),
                    dendroLabels = F, addGuide = T, hang = 0.03, guideHang = 0.05)



# 6. relate module to traits  --------------------------------------------------

# change disease state to binary from character
traits <- colData %>% mutate(disease_state = ifelse(grepl(1, Diagnosis), 1, 0)) %>% dplyr::select(c(2:47))

# define number of samples and genes
nSamples <- nrow(norm_counts)
nGenes <- ncol(norm_counts)

module_trait_corr <- cor(module_eigengenes, traits, use = 'p')
module_trai_corr_pvals <- corPvalueStudent(module_trait_corr, nSamples)

# heatmap
heatmap_data <- merge(module_eigengenes, traits, by = 'row.names')
heatmap_data <- heatmap_data %>% column_to_rownames(var = 'Row.names')

# x will have trait data
# y will have all eigengenes names
names(heatmap_data[30])
CorLevelPlot(heatmap_data, x = names(heatmap_data)[30:75], y = names(heatmap_data)[1:29],
             col = c('#0d7d97', '#99c6cc', 'grey', '#ff5a5e', '#c31e23'), rotLabX = 90, cexCorval = 0.7)

# Focus on association and not correlation
# We are finding relationship of module eigengene and trait of interest, and verifying if they are statistically significant
# Apart from pearson correlation, we can also use t-test or linear model, but all yield similar results

# look for genes of a specific color
module_gene_mapping <- as.data.frame(bwnet$colors)
module_gene_mapping %>% filter('bwnet$colors' == 'salmon') %>% rownames()

# intra-molecular hub genes
module_membership_measure <- cor(module_eigengenes, norm_counts, use = 'p')
module_membership_measure_pvals <- corPvalueStudent(module_membership_measure, nSamples)
module_membership_measure_pvals <- t(module_membership_measure_pvals)

# calculate the gene significance & associated p-values with Disease condition
gene_signif_corr <- cor(norm_counts, traits$disease_state, use = 'p')
gene_signif_corr_pvals <- corPvalueStudent(gene_signif_corr, nSamples)

# view top 15 genes
gene_signif_corr_pvals %>%
  as.data.frame() %>%
  arrange(V1) %>%
  head(15)


write.csv(module_membership_measure_pvals, 'Module_membership_measure_pvals_Female.csv')
#write.csv(gene_signif_corr_pvals, 'Gene_signif_corr_pvals_Diagnosis_Female.csv')




## For each and every trait 
colnames(traits)
# Define list of columns
columns_to_process <- c("Benton", "Clock", "COGSTATE", "Epworth", "Geriatric_Depression", "Hopkins_Recall",              
                        "Hopkins_Recog", "LetterNumber", "Lexical_Fluency", "MDSP_SleepNight", "MDSP_SleepDay",
                        "MDSP_Pain", "MDSP_Urine", "MDSP_Constipate", "MDSP_LightHead", "MDSP_Fatigue", "MDS_Cognition",
                        "MDS_Hallucination", "MDS_Depress", "MDS_Anxiety", "MDS_Apathy", "MDS_DopaDefic", "Modif_Boston",
                        "Montreal_Cognitive", "Impulsive_ICD", "Impulsive_CompulsiveBehavior", "REM_Dream", "REM_Movement",
                        "REM_AwakeProblem", "REM_AwakeDream", "SCOPA_Gastro", "SCOPA_Urine", "SCOPA_Cardio", "SCOPA_Eye", 
                        "SCOPA_Thermo", "SCOPA_Sex", "Semantic", "Education", "Hand", "STAIS", "STAIA", "Symbol_Digit", 
                        "Trail_Making_A", "Trail_Making_B", "UPSIT", "disease_state"  )

# Define a function to process each column
process_column <- function(column_name) {
  # Compute correlation
  gene_signif_corr <- cor(norm_counts, traits[[column_name]], use = 'p')
  
  # Compute p-values
  gene_signif_corr_pvals <- corPvalueStudent(gene_signif_corr, nSamples)
  
  # Convert to data frame and arrange
  gene_signif_corr_pvals_df <- gene_signif_corr_pvals %>%
    as.data.frame() %>%
    arrange(V1)
  
  # Save the results
  output_file <- paste0('Gene_signif_corr_pvals_', column_name, '_Female.csv')
  write.csv(gene_signif_corr_pvals_df, output_file, row.names = T)
}

# Apply the function to each column
lapply(columns_to_process, process_column)

