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
    dplyr::filter(sheet_title == 'Microbial signatures curation') %>%
    dplyr::pull(sheet_key)
  googlesheets::gs_key(key)
}

#' return the curation worksheet (sheet #2)
#'
#' Also skips first row of "data" since that is just a comment
#'
#' @importFrom readr read_csv
#' @param sheet The spreadsheet reference from \code{\link{google_microbial_sig_sheet}}
#' @param additional arguments passed on to readr::read_csv
#'
#' @export
curation_sheet = function(...) {
  cfile <-
    system.file('extdata/curation.csv.gz', package = "MungeCuratedMGS")
  sheet <- readr::read_csv(cfile, skip = 1, ...)
  if (ncol(sheet) == 1L) {
    stop("Run `git lfs pull` to get curation sheet, then re-install package")
  }
  sheet <- sheet[-1, ]
  sheet <- sheet[!is.na(sheet$PMID), ]
  sheet <- sheet[, !colnames(sheet) %in% c("Dropbox", "PubMed")]
  ##remove all NA columns
  sheet <- sheet[, colSums(is.na(sheet)) < nrow(sheet)] 
  ## clean up blank rows
  blank <- !complete.cases(sheet[, c("source within paper", "Free-form description", "taxon 1")])
  if (sum(blank) > 0) {
    warning(paste(
      "The following PMIDs have missing data.", sum(blank), "rows were dropped from:",
      paste(sort(unique(sheet[blank, ]$PMID)), collapse=" ")
      ))
    sheet <- sheet[!blank, ]
  }
}
