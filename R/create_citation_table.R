#' create citation table
#'
#' This will be the `citation` data model
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_citation_table = function(sheet = curation_sheet()) {
  output <- sheet %>%
    dplyr::select(PMID) %>%
    unique()
  # fill in NAs for missing optional columns?
  output$DOI <- rep(NA, nrow(output))
  output$BibTex <- rep(NA, nrow(output))
  output$URI <- rep(NA, nrow(output))
  output$primary_key <- create_keys("CIT", nrow(output))
  output[,c(ncol(output), 1:(ncol(output) - 1))]
}
   
