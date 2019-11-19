#' create signature metadata table
#'
#' This will be the `signature metadata` data model
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_signaturemetadata_table = function(sheet = curation_sheet()) {
  sheet %>%
    dplyr::select(
      PMID,
      `source within paper`,
      `Free-form description`,
      `contrast (list control group last)`,
      body_site,
      condition,
      `date of curation`,
      curator,
      revision
    )
}
