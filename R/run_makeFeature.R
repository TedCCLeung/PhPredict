#' Read gff3 files into Grange objects
#' @importFrom magrittr %>%
#'
#' @param gff3_paths Character vector. Full paths to gff3 files of motif mapping
#' @param max_pvalue Numeric.
#' @param max_qvalue Numeric.
#' @param output_dir Character. Output directory.
#'
#' @return Data frame.
#'
#' @export


run_makeFeature <- function(
  gff3_paths,
  max_qvalue = 0.01,
  max_pvalue = 0.01,
  output_dir
){

  ## Get a list of all gff3 files
  feature_list <- lapply(gff3_paths, function(x){gff3_to_feature(x, max_qval = max_qvalue, max_pval = max_pvalue)})

  ## Merge all results
  all_features <- feature_list %>%
    purrr::reduce(dplyr::full_join) %>%
    as.matrix() %>%
    tidyr::replace_na(0) %>%
    as.data.frame()
  return(all_features)
}



# run_makeFeature(
#   gff3_paths =
#     list.files(
#       "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/4_MotifEnrichment/CISBP_denovo/1_mapping/",
#       full.names = TRUE
#     ),
#   max_qvalue = 0.01,
#   output_dir = "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/5_MotifValidation"
# )
