#' return a reference to the google curation sheet
#'
#'
#'
#' @importFrom googlesheets gs_key gs_ls
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
#'
#' @note requires login via `gs_gs()` or will prompt for login.
#'
#' @export
google_microbial_sig_sheet = function() {
  key = data.frame(googlesheets::gs_ls()) %>%
    dplyr::filter(sheet_title=='Microbial signatures curation') %>%
    dplyr::pull(sheet_key)
  googlesheets::gs_key(key)
}

#' return the curation worksheet (sheet #2)
#'
#' Also skips first row of "data" since that is just a comment
#'
#' @importFrom readr read_csv
#' @param sheet The spreadsheet reference from \code{\link{google_microbial_sig_sheet}}
#'
#' @export
curation_sheet = function() {
  readr::read_csv(system.file(package="MungeCuratedMGS", 'extdata/curation.csv.gz'), skip=1)
}


#' create signatures table
#'
#' This will be the `signature` data model
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_signatures_table = function(sheet = curation_sheet()) {
  sheet %>%
    dplyr::select(
      PMID,
      `source within paper`,
      `Free-form description`,
      `contrast (list control group last)`,
      `body_site`,
      `condition`,
      `date of curation`,
      `curator`,
      `revision`
    )
  sheet[-1, ]
}
