#' create study table
#'
#' This will be the `study` data model
#'
#' @param sheet a data.frame, the curation worksheet from google sheet
#'     (sheet 2 as of now)
#'
#' @importFrom dplyr as_tibble
#' @export
create_study_table <- function(sheet = curation_sheet())
{ 
    pmids <- unique(sheet[,"PMID"])    
    rel.cols <- c("sequencing type",
                    "16S variable region",
                    "sequencing platform",
                    "study design",
                    "matched on",
                    "confounders controlled for",
                    "antibiotics exclusion",
                    "Country")
                    # , `location`, `citation`
    feats <- lapply(rel.cols, .studyFeature, sheet = sheet)
    stbl <- do.call(cbind, feats)                  
    stbl <- cbind(pmids, stbl) 
    colnames(stbl) <- c("PMID", rel.cols)
    for(i in 6:9) stbl[[i]] <- as.character(stbl[[i]])
    as_tibble(stbl)
}

.studyFeature <- function(feature, sheet)
{
    sheet <- as.data.frame(sheet) 
    l <- split(sheet[,feature], sheet[,"PMID"])
    ul <- lapply(l, unique)
    lg1 <- lengths(ul) > 1

    if(any(lg1))
    {
        wmsg <- paste("PMIDs",
                        paste(names(lg1)[lg1], collapse = ", "),       
                        "with >1 unique", feature, ".\n", 
                        "Chosing the first one for each of them.")
        warning(wmsg)
        ul <- lapply(ul, function(x) x[1])
    }
    
    unname(unlist(ul))    
}

#create_study_table <- function() {
#    studymeta <- readr::read_csv(
#        system.file(package = "MungeCuratedMGS",
#        "resources/validate/studyMeta.csv",
#        mustWork = TRUE)
#    )
#    cnames <- studymeta[["name"]]
#    ctypes <- substr(studymeta[["rdatatype"]], 1, 1)
#    collist <- as.list(ctypes)
#    names(collist) <- cnames
#    ctypes <- do.call(readr::cols_only, collist)
#    readtable <- curation_sheet(col_types = ctypes)
#    reqnames <- cnames[studymeta[["required"]]]
#    if (!all(reqnames %in% names(readtable)))
#        stop("Required column(s) not found: ", reqnames)
#    readtable
#}
