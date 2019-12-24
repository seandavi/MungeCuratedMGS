#' create study table in two-table data model
#'
#' This will be the `study` data model
#'
#' @param sheet a data.frame, the curation worksheet from google sheet
#'     (sheet 2 as of now)
#'
#' @importFrom tibble as_tibble
#' @export
create_study_table <- function(sheet = curation_sheet())
{
    sheet <- sheet[, colnames(sheet) %in% studyCols()]
    return(sheet)
}


#' studyCols
#'
#' @return A vector with the column names of the study table (without key)
#' @export
#'
#' @examples
#' studyCols()
#' 
studyCols <- function() {
    return(
        c(
            "sequencing type",
            "16S variable region (lower bound)",
            "16S variable region (upper bound)",
            "sequencing platform",
            "study design",
            "matched on",
            "confounders controlled for",
            "antibiotics exclusion",
            "Country",
            "DOI", 
            "BibTex", 
            "URI",
            "PMID"
        )
    )
}

.studyFeature <- function(feature, sheet)
{
    sheet <- as.data.frame(sheet)
    l <- split(sheet[, feature], sheet[, "PMID"])
    ul <- lapply(l, unique)
    lg1 <- lengths(ul) > 1
    
    if (any(lg1))
    {
        wmsg <- paste(
            "PMIDs",
            paste(names(lg1)[lg1], collapse = ", "),
            "with >1 unique",
            feature,
            ".\n",
            "Chosing the first one for each of them."
        )
        warning(wmsg)
        ul <- lapply(ul, function(x)
            x[1])
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
