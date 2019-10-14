#' return a reference to the google curation sheet
#'
#'
#'
#' @importFrom googlesheets gs_key
#'
#' @note requires login via `gs_gs()` or will prompt for login.
#' @param key the google sheet key like in constants.R
#'
#' @export
google_microbial_sig_sheet = function(key = .ws_key) {
  googlesheets::gs_key(key)
}

#' return the curation worksheet (sheet #2)
#'
#' Also skips first row of "data" since that is just a comment
#'
#' @importFrom googlesheets gs_read
#' @param sheet The spreadsheet reference from \code{\link{google_microbial_sig_sheet}}
#'
#' @export
curation_sheet = function(sheet = google_microbial_sig_sheet()) {
  tmp = gs_read(sheet, 2, skip = 1)
  return(tmp[-1,])
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
}
