#' create signature table
#'
#' This will be the `signature` data model
#'
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @importFrom reshape2 melt
#' @export
create_signature_table <- function(sheet = curation_sheet()) {
    m2n <- MungeCuratedMGS:::metaphlan2ncbi()
    ind <- grep("^taxon", colnames(sheet))
  
    # some rows contain duplicated taxa (curator accidentally entered twice?)
    nr.fields <- length(ind) 
    .rmDuplicates <- function(x)
    {
        ux <- unique(x[!is.na(x)])
        c(ux, rep(NA, nr.fields - length(ux)))
    }
    sheet[,ind] <- t(apply(sheet[,ind], 1, .rmDuplicates))    

    for (i in ind){
        j <- sub("taxon", "NCBI", colnames(sheet)[i])
        sheet[[j]] <- as.integer(m2n[sheet[[i]]])
        na.ind <- !is.na(sheet[,i]) & is.na(sheet[,j])
        sheet[na.ind, j] <- sheet[na.ind, i]
    }
    sheet <- sheet[,-ind]
    sheet <- sheet[, !colnames(sheet) %in% c(studyCols(), experimentCols())]
    return(sheet)
}
