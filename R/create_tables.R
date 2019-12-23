#' create two-table data model
#'
#' This will be the `study` data model
#'
#' @param sheet a data.frame, the curation worksheet from google sheet
#'     (sheet 2 as of now)
#'
#' @importFrom tibble as_tibble
#' @export
create_tables <- function(sheet = curation_sheet())
{
    # content
    study.cols <- c(
        "sequencing type",
        "16s variable region (lower bound)",
        "16s variable region (upper bound)",
        "sequencing platform",
        "study design",
        "matched on",
        "confounders controlled for",
        "antibiotics exclusion",
        "Country",
        "PMID"
    )
    ## do lower & upper bound 16S
    vlist <- strsplit(sheet$`16S variable region`, "-")
    for (i in which(!is.na(vlist))) {
        if (length(vlist[[i]]) == 1L)
            vlist[[i]] <- rep(vlist[[i]], 2)
    }
    vlist <- do.call(rbind, vlist)
    sheet$`16s variable region (lower bound)` <- vlist[, 1]
    sheet$`16s variable region (upper bound)` <- vlist[, 2]
    sheet <- sheet[, !colnames(sheet) %in% "16S variable region"]
    ## create keys
    study.string <- Reduce(paste, sheet[, study.cols])
    sheet$STUDY <-
        paste0("Study ", rank(study.string, ties.method = "min"))
    sheet$SIGNATURE <- create_keys("Signature ", nrow(sheet))
    ## create study sheet
    output <- list()
    output[["study"]] <- unique(sheet[, c("STUDY", study.cols)])
    output[["study"]]$DOI <- NA
    output[["study"]]$BibTex <- NA
    output[["study"]]$URI <- NA
    ## create signature sheet
    output[["signature"]] <-
        sheet[, !colnames(sheet) %in% colnames(output[["study"]])]
    output[["signature"]]$STUDY <- sheet$STUDY
    output[["signature"]] <-
        output[["signature"]][, !colnames(output[["signature"]]) %in% c("Dropbox", "PubMed")]
    output[["signature"]] <-
        output[["signature"]][,!sapply(output[["signature"]], function(x)
            all(is.na(x)))]
    return(output)
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
