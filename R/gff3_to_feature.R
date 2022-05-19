#' Read gff3 files into Grange objects
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param file Character. Full path to the gff3 files
#' @param max_pval Numeric.
#' @param max_qval Numeric.
#'
#' @return Data frame.
#'
#' @export

gff3_to_feature <- function(
  file,
  max_pval = 1,
  max_qval = 1
){

  df <- rtracklayer::import(file, format = "gff3") %>%
    as.data.frame() %>%
    dplyr::mutate(pvalue = as.numeric(.data$pvalue)) %>%
    dplyr::mutate(qvalue = as.numeric(.data$qvalue)) %>%
    dplyr::filter(as.numeric(.data$qvalue) < max_qval) %>%
    dplyr::filter(as.numeric(.data$pvalue) < max_pval) %>%
    dplyr::select(c("seqnames", "motif")) %>%
    ## Get a value of one
    dplyr::mutate(value = as.numeric(.data$seqnames == .data$seqnames)) %>%
    tidyr::pivot_wider(names_from = "motif", values_from = "value", values_fn = length) %>%
    dplyr::mutate(seqnames = .data$seqnames %>% substr(1, 9))

  ## Replace NA as 0
  df[is.na(df)] <- 0
  ## Change column name
  colnames(df)[1] <- "ID"

  return(df)
}




