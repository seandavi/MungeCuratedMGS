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
curation_sheet = function(...) 
{
    cfile <- system.file('extdata/curation.csv.gz', package = "MungeCuratedMGS")
    sheet <- readr::read_csv(cfile, skip = 1, ...)
    sheet <- as.data.frame(sheet)
    if (ncol(sheet) == 1L) {
      stop("Run `git lfs pull` to get curation sheet, then re-install package")
    }
    sheet <- sheet[-1, ]
    sheet <- sheet[!is.na(sheet$PMID), ]
    sheet <- sheet[, !colnames(sheet) %in% c("Dropbox", "PubMed")]
    
    ##remove all NA columns
    sheet <- sheet[, colSums(is.na(sheet)) < nrow(sheet)] 
    
    # transform diversity columns
    ud <- split(sheet[, "UP or DOWN"], sheet[,"PMID"])
    odd.pmids <- names(ud)[lengths(ud) %% 2 != 0]
    sheet <- sheet[!(sheet[,"PMID"] %in% odd.pmids),] 
    warning(paste("Removing", length(odd.pmids), 
                    "PMIDs with an odd number of UP/DOWN rows:", 
                    paste(odd.pmids, collapse = ", ")))


    .transform <- function(diversity) 
    {
        if(all(is.na(diversity))) diversity <- ""
        else if(any(is.na(diversity))) diversity <- paste("Invalid combination", paste(diversity, collapse = ","))
        else if(all(diversity == rep("NO", 2))) diversity <- "unchanged" 
        else if(all(diversity == c("YES", "NO"))) diversity <- "increased"
        else if(all(diversity == c("NO", "YES"))) diversity <- "decreased"
        else diversity <- paste("Invalid combination", paste(diversity, collapse = ","))
        return(rep(diversity, 2)) 
    }
    
    div.cols <- c("Pielou",
            "Shannon index",    
            "Chao1",
            "Simpson",
            "Inverse Simpson",
            "Richness")

    stopifnot(nrow(sheet) %% 2  == 0)
    grid <- rep(seq_len(nrow(sheet) / 2), each = 2)
    for(col in div.cols)
    {
        spl.col <- split(sheet[,col], grid)
        sheet[,col] <- unlist(lapply(spl.col, .transform))
        ind <- grepl("^Invalid", sheet[,col])
        if(sum(ind))
        {
            pmids <- unique(sheet[ind, "PMID"])
            warning(paste("PMIDs", paste(pmids, collapse = ","), "have invalid", 
                col, "fields: \n", paste(unique(sheet[ind, col]), collapse = "\n"),
                "\nInvalid fields have been set to NA"))
            sheet[ind, col] <- "unknown"
        }
    }

    ## clean up blank rows
    blank <- !complete.cases(sheet[, c("source within paper", "Free-form description", "taxon 1")])
    forremoval <- blank
    if (sum(blank) > 0) {
      warning(paste(
        "The following PMIDs have missing data.", sum(forremoval), "rows were dropped from:",
        paste(sort(unique(sheet[forremoval, ]$PMID)), collapse=" ")
        ))
      sheet <- sheet[!forremoval, ]
    }
  
    # Get rid of variants of "case =" at start of case definition
    sheet$`case definition` <-
      sub("^\\s*[cC]ase\\s*=\\s*", "", sheet$`case definition`, perl = TRUE)
  
    ## do lower & upper bound 16S
    vlist <- strsplit(sheet$`16S variable region`, "-")
    for (i in which(!is.na(vlist))) {
      if (length(vlist[[i]]) == 1L)
        vlist[[i]] <- c(vlist[[i]], NA)
      else if (length(vlist[[i]]) > 2L)
        vlist[[i]] <- vlist[[i]][c(1, length(vlist[[i]]))]
    }
    vlist <- do.call(rbind, vlist)
    sheet$`16S variable region (lower bound)` <- as.integer(sub("V", "", vlist[, 1]))
    sheet$`16S variable region (upper bound)` <- as.integer(sub("V", "", vlist[, 2]))
    sheet <- sheet[, !colnames(sheet) %in% "16S variable region"]
   
    ## add extra columns
    sheet$DOI <- NA
    sheet$BibTex <- NA
    sheet$URI <- NA
    contr <- strsplit(sheet$`contrast (list control group last)`, "\\s*[Vv][Ss]\\.*\\s*")
    pcontr <- sheet$PMID[lengths(contr) != 2]
    if(length(pcontr)) 
        warning("PMIDs with not well-defined contrasts: ", 
                paste(unique(pcontr), collapse = ", "))
    for (i in which(lengths(contr) == 1)) {
      contr[[i]] <- rep(contr[[i]], 2)
    }
    contr <- do.call(rbind, contr)
    colnames(contr) <- c("Group 1 name", "Group 0 name")
    sheet <- cbind(sheet, contr)
    sheet <- sheet[, !colnames(sheet) %in% "contrast (list control group last)"]
    
    ## Increased abundance in Group 1?
    sheet$`Increased abundance in Group 1` <-
      ifelse(sheet$`UP or DOWN` == "DOWN", "increased", "decreased")
    sheet <- sheet[, !colnames(sheet) %in% "UP or DOWN"]

    ## MHT
    sheet$`threshold corrected for MHT? yes/no` <- tolower(sheet$`threshold corrected for MHT? yes/no`)
    sheet$`threshold corrected for MHT? yes/no` <- ifelse(sheet$`threshold corrected for MHT? yes/no` == "yes", "Yes", "No")
    
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

    # set all NA's to blank
    sheet[is.na(sheet)] <- ""

    # host species
    species <- c("Homo sapiens", "Mus musculus", "Rattus norvegicus")
    names(species) <- c("human", "mouse", "rat")
    sheet[["host species"]] <- unname(species[sheet[["host species"]]])

    return(sheet)
}



