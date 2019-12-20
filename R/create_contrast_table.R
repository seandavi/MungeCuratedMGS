#' create contrast table
#'
#' This will be the `contrast` data model
#'
#' @param sheet a data.frame, the curation worksheet from google sheet
#'     (sheet 2 as of now)
#'
#' @export
create_contrast_table <- function(sheet = curation_sheet()) 
{
    sheet <- sheet %>%
        dplyr::select(
            "statistical test",
            "significance threshold",  
            "threshold corrected for MHT? yes/no",
            "control (unexposed) sample size",
            "case (exposed) sample size",
            "contrast (list control group last)",
            "UP or DOWN",
    )
    
    sheet <- as.data.frame(sheet)
    
    sig.thresh <- .cleanSigThresh(sheet[["significance threshold"]])
    contrast <- .extractContrastedGroups(sheet[["contrast (list control group last)"]])
    
    mht <- sheet[["threshold corrected for MHT? yes/no"]] == "yes"  
    increased <- sheet[["UP or DOWN"]] == "UP"

    stest.tab <- create_statistical_test_table()
    ind <- match(sheet[["statistical test"]], stest.tab[["statistical test"]]) 

    ctbl <- data.frame( create_keys("CON", nrow(sheet)),    
                        stest.tab[ind, "primary_key"],
                        sig.thresh,
                        mht,
                        as.integer(sheet[["control (unexposed) sample size"]]),
                        as.integer(sheet[["case (exposed) sample size"]]),
                        contrast$controls,
                        contrast$cases,
                        increased)

    colnames(ctbl) <- c("primary_key",
                        "statistical test", 
                        "significance threshold", 
                        "MHT correction",
                        "Group 0 sample size",
                        "Group 1 sample size",
                        "Group 0 definition",
                        "Group 1 definition",
                        "Increased abundance in Group 1")

    dplyr::as_tibble(ctbl)       
}

.cleanSigThresh <- function(scol)
{
    spl <- lapply(scol, function(x) unlist(strsplit(x, "<")))
    ind <- lengths(spl) > 1
    spl[ind] <- lapply(spl[ind], function(x) x[2])
    scol <- unlist(spl)

    ind <- grepl("^LDA", scol)
    scol[ind] <- NA
    
    ind1 <- grepl("LDA", scol)
    ind2 <- grepl("p", scol)
    ind3 <- grepl("assumed", scol)
    ind4 <- grepl("FDR", scol)
    scol[ind1 | ind2 | ind3 | ind4] <- "0.05"
    scol <- gsub("\\.\\.", ".", scol)
    
    as.numeric(scol)
}
    
.extractContrastedGroups <- function(ccol)
{
    ccol <- lapply(ccol, function(x) unlist(strsplit(x, " [Vv][Ss]\\.? ")))
    lne2 <- lengths(ccol) != 2
    if(any(lne2)) for(i in which(lne2)) ccol[[i]] <- rep(NA, 2) 
    ccol <- unlist(ccol)
    list(   cases = ccol[seq(1, length(ccol) - 1, by = 2)], 
            controls = ccol[seq(2, length(ccol), by = 2)])  
}
