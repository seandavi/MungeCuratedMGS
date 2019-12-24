#' create signature table
#'
#' This will be the `signature` data model
#'
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @importFrom reshape2 melt
#' @export
create_signature_table = function(sheet = curation_sheet()) {
    m2n <- MungeCuratedMGS:::metaphlan2ncbi()
    ind <- grep("^taxon", colnames(sheet))
    for (i in ind){
        sheet[[sub("taxon", "NCBI", colnames(sheet)[i])]] <- as.integer(m2n[sheet[[i]]])
    }
    colnames(sheet) <- sub("^taxon.+", "Metaphlan", colnames(sheet))
    colnames(sheet) <- sub("^NCBI.+", "NCBI", colnames(sheet))
    sheet <- sheet[, !colnames(sheet) %in% studyCols()]
    colnames(sheet) <- sub("source within paper", "source", colnames(sheet))
    colnames(sheet) <- sub("Free-form description", "description", colnames(sheet))
    return(sheet)
}
