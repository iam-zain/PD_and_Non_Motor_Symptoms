library(gprofiler2)
library(igraph) # For network analysis
library(STRINGdb) # For protein-protein interaction data


 ## as it had only a few genes, STRING got only 3 IDs, upon mapping we got 0 hits. So it has no results ##

setwd('S:\\PPMI_RNA\\Processed\\Male')
df <- read.csv('DEGs_Result_Male_UpDown.csv')
df$row <- sub("\\..*", "", df$row) # removing version info from ensemble id

# gene names
ensembl_genes <- df$row
gene_symbols <- gconvert(ensembl_genes, organism="hsapiens", target="ENTREZGENE", filter_na = F)$target
gene_symbols <- na.omit(gene_symbols)
gene_symbols <- as.data.frame(gene_symbols)

# '9606' for human
string_db <- STRINGdb$new(version = "11", species = 9606)
DEG_list <- gene_symbols  # your DEGs
any(is.na(DEG_list$gene_symbols))
string_ID <- string_db$map(DEG_list, 'gene_symbols')
 # string id returns only 3 genes  ---------------------------------------------
 # so other analyses can't be performed  ---------------------------------------
string_ID <- na.omit(string_ID)

# get interactions
ppi_data <- string_db$get_interactions(string_ID$STRING_id)

# Create a graph from the interaction data 
ppi_graph <- graph.data.frame(ppi_data, directed = FALSE)

# Calculate network properties

# Degree Centrality
# degree_centrality <- degree(ppi_graph) # node_degrees
# print(paste("Average node degree: ", mean(degree_centrality)))
# 
# Betweenness Centrality
# betweenness_centrality <- betweenness(ppi_graph)
# 
# Closeness Centrality
# closeness_centrality <- closeness(ppi_graph)
# 
# Eigenvector Centrality
# eigenvector_centrality <- eigen_centrality(ppi_graph)$vector
# 
# Clustering Coefficient
# clustering_coefficient <- transitivity(ppi_graph, type = "local")

# Identify important nodes
hub_score <- hub_score(ppi_graph)$vector
authority_score <- authority_score(ppi_graph)$vector

# Plot the PPI network
#plot(ppi_graph, vertex.size = node_degrees, vertex.label = NA)


# Calculate network properties all at once and store in a dataframe
results <- data.frame(
  name = V(ppi_graph)$name,
  degree = degree(ppi_graph),
  betweenness = betweenness(ppi_graph),
  closeness = closeness(ppi_graph),
  eigenvector = eigen_centrality(ppi_graph)$vector,
  clustering = transitivity(ppi_graph, type="local"))
write.csv(results, 'Nodes_Edges_Male.csv')

threshold_20 <- quantile(results$degree, 0.8) # defining threshold criteria, taking top 20, that is 80 percentile
results_20 <- results[results$degree >= threshold_20, ] # selecting/ sub-setting as per threshold for that column 
results_20 <- results_20[order(results_20$degree, decreasing = T),] # sorting as per that column
write.csv(results_20, 'Nodes_Edges_Female_Top20.csv', row.names = F)

# Plot degree centrality
ggplot(results_20, aes(x = name, y = degree)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# as increasing order
results_20$name <- factor(results_20$name, levels = results_20$name[order(results_20$degree)])
ggplot(results_20, aes(x = name, y = degree, fill = name)) + geom_bar(stat = 'identity') + theme(legend.position = 'none')
