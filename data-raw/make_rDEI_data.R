make_rDEI_data <- function(

){

  file <- "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/1_DailyIntegral/archive/020_classify/summary_rDEI.csv"
  rDEI_table <- utils::read.csv(file)[, c("geneID", "rDEI_LDEQ", "rDEI_SDEQ", "rDEI_SDLD")]
  colnames(rDEI_table) <- c("ID", "LDEQ", "SDEQ", "SDLD")
  usethis::use_data(rDEI_table, overwrite = TRUE)

}


make_14clusters <- function(

){

  addLeadingZeros <- function(
    num_vec
  ){
    digit_number <- floor(max(log10(num_vec)+1))
    return(stringr::str_pad(as.character(num_vec), digit_number, pad = "0"))
  }

  dir <- "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/1_DailyIntegral/archive/030_clustering/photoperiodic_geneLists"
  gene_files <- list.files(dir, full.names = TRUE)

  read_genes <- function(x){utils::read.csv(x, header = FALSE)[, 1] %>% sort()}

  genes_14_clusters <- lapply(gene_files, read_genes)
  names(genes_14_clusters) <- paste0("photoperiodic_", addLeadingZeros(seq(length(gene_files))))

  clusters_14_table <- data.frame(
    ID = unlist(genes_14_clusters),
    label = rep(names(genes_14_clusters), times = lengths(genes_14_clusters))
  )

  usethis::use_data(clusters_14_table)
}
