# ==============================================================================
# Script: 07_Advanced_Stats_Correlations.R
# Purpose: Run PCA for variable contribution and Cluster-wise Correlation Matrices.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(ggplot2)
library(FactoMineR)  # For advanced PCA
library(factoextra)  # For PCA visualization
library(corrplot)    # For correlation matrices
library(gridExtra)   # For arranging plots

# 2. Load Clustered Data --------------------------------------------------
message("Loading Clustered Data...")

if(!file.exists("data/processed/04_Clustered_Data.rds")) {
  stop("Error: Input file not found. Run Script 04 first.")
}
Data_Clustered <- readRDS("data/processed/04_Clustered_Data.rds")

# Elements list
elements <- c('Cl', 'Ca', 'Si', 'Fe', 'Al', 'K', 'Ti', 
              'Cu', 'Ba', 'Zn', 'Te', 'Br', 'Pd')

# 3. PCA: Variable Contributions ------------------------------------------
message("Running PCA for Variable Contributions...")

# Run PCA
pca_res <- PCA(Data_Clustered[, elements], scale.unit = TRUE, graph = FALSE)

# Visualize Contributions to PC1, PC2, PC3
# This helps identify which elements drive the main variation
p_pc1 <- fviz_contrib(pca_res, choice = "var", axes = 1, top = 10, title = "Contrib to PC1")
p_pc2 <- fviz_contrib(pca_res, choice = "var", axes = 2, top = 10, title = "Contrib to PC2")
p_pc3 <- fviz_contrib(pca_res, choice = "var", axes = 3, top = 10, title = "Contrib to PC3")

# Arrange in a grid
if(!dir.exists("plots")) dir.create("plots")

grid_plot <- grid.arrange(p_pc1, p_pc2, p_pc3, ncol=3, 
                          top = "Variable Contributions to Principal Components")

ggsave("plots/07_PCA_Variable_Contrib.png", grid_plot, width=12, height=5)

# 4. Correlation Matrices by Cluster --------------------------------------
message("Generating Correlation Matrices per Cluster...")

# We need to calculate correlations for each cluster individually
# to see the relationships between elements within that specific source type.

plot_corr_matrix <- function(cluster_id) {
  # Filter data for this cluster
  cluster_data <- Data_Clustered %>% 
    filter(Cluster == cluster_id) %>%
    select(all_of(elements))
  
  # Calculate Correlation
  # method = "pearson" as per your script
  M <- cor(cluster_data, method = "pearson", use = "complete.obs")
  
  # Open PNG device
  png(filename = paste0("plots/07_Corr_Cluster_", cluster_id, ".png"), 
      width = 600, height = 600)
  
  # Plot
  corrplot(M, method = "number", type = "lower", 
           tl.cex = 1.0, number.cex = 0.9,
           title = paste("Correlation Matrix - Cluster", cluster_id),
           mar = c(0,0,2,0)) # Add margin for title
  
  dev.off()
}

# Run for all 3 clusters
for(k in 1:3) {
  plot_corr_matrix(k)
}

message("Script 07 Complete. All analysis finished!")