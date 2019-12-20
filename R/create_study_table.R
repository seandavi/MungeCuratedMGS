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
    # content
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
    colnames(stbl) <- tolower(rel.cols)

    # keys
    prim.key <- create_keys("STUD", nrow(stbl))
    
    loc.tab <- create_location_table()
    ind <- match(stbl[,"country"], loc.tab$country)
    loc.key <- loc.tab[ind, "primary_key"]
    colnames(loc.key) <- "location"
    
    pmids <- unique(sheet$PMID)    
    cit.tab <- create_citation_table(sheet)
    ind <- match(pmids, cit.tab$PMID)
    cit.key <- cit.tab[ind, "primary_key"]
    colnames(cit.key) <- "citation"

    stbl <- stbl[,-ncol(stbl)] 
    stbl <- cbind(primary_key = prim.key, stbl, loc.key, cit.key)
    for(i in c(1,6:10)) stbl[,i] <- as.character(stbl[,i]) 
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
