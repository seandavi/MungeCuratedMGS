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
   
    nr.fields <- length(ind) 
    .rmDuplicates <- function(x)
    {
        ux <- unique(x[!is.na(x)])
        c(ux, rep(NA, nr.fields - length(ux)))
    }
    sheet[,ind] <- t(apply(sheet[,ind], 1, .rmDuplicates))    

    for (i in ind){
        sheet[[sub("taxon", "NCBI", colnames(sheet)[i])]] <- as.integer(m2n[sheet[[i]]])
    }
    colnames(sheet) <- sub("^taxon.+", "Metaphlan", colnames(sheet))
    colnames(sheet) <- sub("^NCBI.+", "NCBI", colnames(sheet))
    sheet <- sheet[, !colnames(sheet) %in% c(studyCols(), experimentCols())]
    return(sheet)
}
