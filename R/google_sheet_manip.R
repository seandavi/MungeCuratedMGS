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
  # Get rid of variants of "case =" at start of case definition
  sheet$`case definition` <-
    sub("^\\s*case\\s*=\\s*", "", sheet$`case definition`, perl = TRUE)
  ## do lower & upper bound 16S
  vlist <- strsplit(sheet$`16S variable region`, "-")
  for (i in which(!is.na(vlist))) {
    if (length(vlist[[i]]) == 1L)
      vlist[[i]] <- c(vlist[[i]], NA)
  }
  vlist <- do.call(rbind, vlist)
  sheet$`16S variable region (lower bound)` <- vlist[, 1]
  sheet$`16S variable region (upper bound)` <- vlist[, 2]
  sheet <- sheet[, !colnames(sheet) %in% "16S variable region"]
  ## add extra columns
  sheet$DOI <- NA
  sheet$BibTex <- NA
  sheet$URI <- NA
  contr <- strsplit(sheet$`contrast (list control group last)`, "\\s*[Vv][Ss]\\.*\\s*")
  for (i in which(lengths(contr) == 1)) {
    contr[[i]] <- rep(contr[[i]], 2)
  }
  contr <- do.call(rbind, contr)
  colnames(contr) <- c("Group 1 name", "Group 0 name")
  sheet <- cbind(sheet, contr)
  sheet <- sheet[, !colnames(sheet) %in% "contrast (list control group last)"]
  ## Increased abundance in Group 1?
  sheet$`Increased abundance in Group 1` <-
    ifelse(sheet$`UP or DOWN` == "DOWN", "NO", "YES")
  sheet <- sheet[, !colnames(sheet) %in% "UP or DOWN"]
  ## upper-case MHT
  sheet$`threshold corrected for MHT? yes/no` <- toupper(sheet$`threshold corrected for MHT? yes/no`)
  ## Column renaming for consistency with Ike's example
  colnames(sheet) <- sub("source within paper", "source", colnames(sheet))
  colnames(sheet) <- sub("Free-form description", "description", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("date of curation", "date", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("threshold corrected for MHT? yes/no", "MHT correction", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("control (unexposed) sample size", "Group 0 sample size", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("case (exposed) sample size", "Group 1 sample size", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("case definition", "Group 1 definition", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("Shannon index", "Shannon", colnames(sheet), fixed = TRUE)
  colnames(sheet) <- sub("Country", "location", colnames(sheet), fixed = TRUE)
  return(sheet)
}

