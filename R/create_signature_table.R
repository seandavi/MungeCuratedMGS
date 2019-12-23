#' create signature table
#'
#' This will be the `signature` data model
#'
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @importFrom reshape2 melt
#' @export
create_signature_table = function(sheet = curation_sheet()) {
    ## create signature sheet
    sheet <- sheet[, !colnames(sheet) %in% c("Dropbox", "PubMed")]
    sheet <- sheet[, colSums(is.na(sheet)) < nrow(sheet)] ##remove all NA columns
    m2n <- MungeCuratedMGS:::metaphlan2ncbi()
    ind <- grep("^taxon", colnames(sheet))
    for (i in ind){
        sheet[[sub("taxon", "NCBI", colnames(sheet)[i])]] <- as.integer(m2n[sheet[[i]]])
    }
    colnames(sheet) <- sub("^taxon.+", "Metaphlan", colnames(sheet))
    colnames(sheet) <- sub("^as\\.integer.+", "NCBI", colnames(sheet))
    return(sheet)
}
