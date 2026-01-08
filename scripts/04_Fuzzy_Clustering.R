# ==============================================================================
# Script: 04_Fuzzy_Clustering.R
# Purpose: Optimize cluster count (Silhouette), Run FCM, Assign Membership.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(cluster)    # For silhouette
library(factoextra) # For cluster visualization
library(e1071)      # For cmeans (Fuzzy C-Means)
library(corrplot)   # For visualizing membership correlations

# 2. Load Scaled Data -----------------------------------------------------
message("Loading Scaled Data...")

if(!file.exists("data/processed/03_Scaled_Data_for_Clustering.rds")) {
  stop("Error: Input file not found. Run Script 03 first.")
}
Data_Scaled <- readRDS("data/processed/03_Scaled_Data_for_Clustering.rds")

# Elements to cluster
target_elements <- c('Cl', 'Ca', 'Si', 'Fe', 'Al', 'K', 'Ti', 
                     'Cu', 'Ba', 'Zn', 'Te', 'Br', 'Pd')

# Prepare matrix for clustering
# Set row names to dates for easier tracking
cluster_matrix <- Data_Scaled[, target_elements]
rownames(cluster_matrix) <- as.character(Data_Scaled$date)

# 3. Determine Optimal Clusters (Silhouette Analysis) ---------------------
message("Calculating Silhouette Scores for k=2 to k=6...")

# We test a range of clusters (e.g., 2 to 6)
cluster_range <- 2:6
sil_scores <- numeric(length(cluster_range))
names(sil_scores) <- cluster_range

set.seed(321)

for (k in cluster_range) {
  # Run Fuzzy C-Means
  fcm_res <- cmeans(cluster_matrix, centers = k, iter.max = 100, 
                    dist = "manhattan", m = 2, method = "cmeans")
  
  # Assign hard clusters (max membership) for Silhouette calculation
  hard_clusters <- max.col(fcm_res$membership)
  
  # Calculate Silhouette
  # Note: Silhouette needs a distance matrix (Manhattan, matching FCM)
  dist_mat <- dist(cluster_matrix, method = "manhattan")
  sil <- silhouette(hard_clusters, dist_mat)
  
  # Store average width
  sil_scores[as.character(k)] <- summary(sil)$avg.width
}

print("Average Silhouette Scores:")
print(sil_scores)

best_k <- as.numeric(names(which.max(sil_scores)))
message(paste("Optimal number of clusters (highest silhouette):", best_k))

# 4. Run Final Fuzzy Clustering (k=3) -------------------------------------
# Note: We stick to k=3 as per your original methodology.
final_k <- 3 
message(paste("Running Final FCM with k =", final_k, "..."))

set.seed(321)
res_fcm <- cmeans(cluster_matrix, centers = final_k, iter.max = 100, 
                  dist = "manhattan", m = 2)

# Extract Membership and Hard Cluster assignments
membership_df <- as.data.frame(res_fcm$membership)
colnames(membership_df) <- paste0("Prob_Cluster_", 1:final_k)
membership_df$date <- Data_Scaled$date

# Assign primary cluster (highest probability)
Data_Clustered <- Data_Scaled
Data_Clustered$Cluster <- res_fcm$cluster
# Add membership probabilities
Data_Clustered <- cbind(Data_Clustered, membership_df[, 1:final_k])

# 5. Visualize Clusters ---------------------------------------------------
message("Generating Cluster Plots...")

# Standard Cluster Plot (PCA-based)
viz_plot <- fviz_cluster(list(data = cluster_matrix, cluster = res_fcm$cluster),
                         geom = "point",
                         ellipse.type = "convex", 
                         palette = "jco",
                         ggtheme = theme_minimal(),
                         main = paste("Fuzzy C-Means Clustering (k =", final_k, ")"))

print(viz_plot)

# 6. Save Results ---------------------------------------------------------
saveRDS(Data_Clustered, "data/processed/04_Clustered_Data.rds")
saveRDS(res_fcm, "data/processed/04_FCM_Model_Object.rds")

if(!dir.exists("plots")) dir.create("plots")
ggsave("plots/04_Cluster_Visualization.png", viz_plot, width=10, height=7)

message("Script 04 Complete. Clustering results saved.")