#' create signature table
#'
#' This will be the `signature` data model
#'
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @importFrom reshape2 melt 
#' @export
create_signature_table = function(sheet = curation_sheet()) {

    ind <- grep("^taxon.1$", colnames(sheet))
    ind <- ind:ncol(sheet)
    msc <- apply(as.matrix(sheet[,ind]), 1, function(x) x[!is.na(x) & x != ""])
    names(msc) <- create_keys("SIG", nrow(sheet))
    sig.table <- reshape2::melt(msc)
    sig.table <- sig.table[,2:1]
    colnames(sig.table) <- c("SIG.ID", "MetaPhlan")    
    
    m2n <- metaphlan2ncbi()
    sig.table$NCBI <- unname(m2n[as.vector(sig.table$MetaPhlan)])
    as_tibble(sig.table)
}
