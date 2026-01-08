# ==============================================================================
# Script: 03_Exploratory_PCA_tSNE.R
# Purpose: Dimensionality reduction, Scree plot, and Outlier detection.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Rtsne)      # For t-SNE
library(dbscan)     # For LOF (Local Outlier Factor)
library(factoextra) # For PCA visualization

# 2. Load Processed Data --------------------------------------------------
message("Loading Coarse Events Data...")

if(!file.exists("data/processed/02_XACT_Coarse_Events.rds")) {
  stop("Error: Input file not found. Run Script 02 first.")
}
XACT_Data <- readRDS("data/processed/02_XACT_Coarse_Events.rds")

# 3. Select Elements & Normalization --------------------------------------
message("Selecting elements and scaling...")

# The 13 elements selected for clustering (based on your script)
target_elements <- c('Cl', 'Ca', 'Si', 'Fe', 'Al', 'K', 'Ti', 
                     'Cu', 'Ba', 'Zn', 'Te', 'Br', 'Pd')

# Subset data
Data_Subset <- XACT_Data[, c("date", "PMcoarse", target_elements)]

# Helper function: Min-Max Scaling
minMax <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

# Apply scaling
Data_Scaled <- Data_Subset
Data_Scaled[target_elements] <- lapply(Data_Subset[target_elements], minMax)

# 4. PCA Analysis (Scree Plot) --------------------------------------------
message("Running PCA...")

# Run PCA on scaled data
pca_result <- prcomp(Data_Scaled[, target_elements], scale. = TRUE)

# Calculate Variance Explained
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Plot: Scree Plot
scree_plot <- qplot(c(1:length(var_explained)), var_explained) + 
  geom_line() + 
  geom_point() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot: Variance Explained by PCs") +
  scale_x_continuous(breaks = 1:length(var_explained)) +
  theme_minimal()

print(scree_plot)

# 5. t-SNE & Outlier Detection (LOF) --------------------------------------
message("Running t-SNE and Outlier Detection...")

set.seed(13)

# Remove duplicates if any (t-SNE dislikes duplicates)
Data_Unique <- distinct(Data_Scaled[, target_elements])

# Run t-SNE
tsne_model <- Rtsne(Data_Unique, dims = 2, perplexity = 5, verbose = TRUE)

# Prepare t-SNE results dataframe
tsne_df <- as.data.frame(tsne_model$Y)
colnames(tsne_df) <- c("Dim1", "Dim2")
# Add back date (assuming rows match distinct data - usually safe if no exact duplicate rows existed)
tsne_df$date <- Data_Scaled$date[1:nrow(tsne_df)]

# Calculate Local Outlier Factor (LOF)
# k = 3 neighbors (as per your script)
lof_scores <- lof(Data_Unique, k = 3)

# Identify Outliers (Threshold > 1.1)
outliers_idx <- which(lof_scores > 1.1)
tsne_df$is_outlier <- ifelse(1:nrow(tsne_df) %in% outliers_idx, "Outlier", "Normal")

# Plot: t-SNE with Outliers
tsne_plot <- ggplot(tsne_df, aes(x=Dim1, y=Dim2, color=is_outlier, label=date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(data = subset(tsne_df, is_outlier == "Outlier"), size = 3) +
  scale_color_manual(values = c("Normal" = "grey", "Outlier" = "red")) +
  ggtitle("t-SNE Visualization with LOF Outliers") +
  theme_minimal()

print(tsne_plot)

# 6. Save Scaled Data for Clustering --------------------------------------
saveRDS(Data_Scaled, "data/processed/03_Scaled_Data_for_Clustering.rds")

# Save Plots
if(!dir.exists("plots")) dir.create("plots")
ggsave("plots/03_Scree_Plot.png", scree_plot, width=8, height=6)
ggsave("plots/03_tSNE_Outliers.png", tsne_plot, width=8, height=6)

message("Script 03 Complete. Scaled data saved for clustering.")