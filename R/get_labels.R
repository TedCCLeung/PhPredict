get_labels <- function(
  labels,
  name,
  output_dir = "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/PhotoperiodMotif/wk3/"
){

  feature_file <- "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/PhotoperiodML/features.tsv"
  df_feature <- utils::read.table(feature_file, sep = "\t", header = TRUE)

  cluster_file <- "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/PhotoperiodClusters/hybrid/clusters.tsv"
  df_cluster <- utils::read.table(cluster_file, sep = "\t", header = TRUE)

  df_cluster$labels <- as.numeric(df_cluster$sub %in% labels)
  df_cluster <- df_cluster[, c("geneID", "labels")]
  colnames(df_cluster) <- c("ID", "labels")
  df_result <- dplyr::left_join(df_cluster, df_feature, by = "ID")

  df_result <- df_result[!is.na(rowSums(df_result[, 2:ncol(df_result)])), ]

  utils::write.csv(df_result,
            file = paste0(output_dir, name, ".csv"),
            row.names = FALSE)
}

# cluster_of_insterst <- list()
# names_ <- c("SD_flat")
#
# Map(get_labels, cluster_of_insterst, names_)

