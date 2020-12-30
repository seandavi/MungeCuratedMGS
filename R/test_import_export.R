#' unit tests for data re-import
#'
#' @importFrom testthat expect_equal
#' @importFrom dplyr arrange
#' @export
testImportExport <- function(import.study,
                             import.experiment,
                             import.signature,
                             export.study, 
                             export.experiment,
                             export.signature)
{
    # check studies
    import.pmids <- sort(unique(as.integer(import.study$PMID)))
    export.pmids <- sort(unique(as.integer(export.study$PMID)))
    testthat::expect_equal(length(import.pmids), length(export.pmids))
    testthat::expect_equal(import.pmids, export.pmids)
    testthat::expect_equal(import.study[["study design"]], 
                           export.study[["Study design"]])

    # check experiments
    testthat::expect_equal(nrow(import.experiment), nrow(export.experiment))
    export.experiment <- dplyr::arrange(export.experiment, 
                                        as.integer(sub("Study ", "", Study)),
                                        as.integer(sub("Experiment", "", 
                                                       `Experiment page name`)))
    
    export.experiment <- as.data.frame(export.experiment)  
    export.experiment[is.na(export.experiment)] <- ""   

    for(i in seq_along(exp.import.cols))
    {
        ic <- exp.import.cols[i]
        ec <- exp.export.cols[i]
        message(ic, ", ", ec)
        res <- try(testthat::expect_equal(import.experiment[[ic]],
                                          export.experiment[[ec]]),
                   silent = TRUE)
        if(is(res, "try-error"))
        {  
            ind <- import.experiment[[ic]] != export.experiment[[ec]]
            exp <- import.experiment[ind, exp.import.cols[1:2]]
            exp <- apply(exp, 1, paste, collapse = "/")

            print(data.frame(experiment = exp,
                             import = import.experiment[ind, ic],
                             export = export.experiment[ind, ec]))
        }  
    }

    # check signatures
    testthat::expect_equal(nrow(import.signature), nrow(export.signature))
    export.signature <- dplyr::arrange(export.signature, 
                                        as.integer(sub("Study ", "", Study)),
                                        as.integer(sub("Experiment", "", 
                                                        Experiment)),
                                        as.integer(sub("Signature", "", 
                                                       `Signature page name`)))
    export.signature <- as.data.frame(export.signature)  
    export.signature[is.na(export.signature)] <- ""   

    testthat::expect_equal(nrow(import.signature), nrow(export.signature))

    for(i in seq_along(sig.import.cols))
    {
        ic <- sig.import.cols[i]
        ec <- sig.export.cols[i]
        message(ic, ", ", ec)
        res <- try(testthat::expect_equal(import.signature[[ic]],
                                          export.signature[[ec]]),
                   silent = TRUE)
        if(is(res, "try-error"))
        {  
            ind <- import.signature[[ic]] != export.signature[[ec]]
            sig <- import.signature[ind, sig.import.cols[1:3]]
            sig <- apply(sig, 1, paste, collapse = "/")
            print(data.frame(signature = sig, 
                             import = import.signature[ind, ic],
                             export = export.signature[ind, ec]))
        }  
    }
}

exp.import.cols <- c("Study", 
                     "Page Name",
                     "antibiotics exclusion",
                     "location",
                     "condition",
                     "Group 0 sample size",
                     "Group 1 sample size",
                     "Group 0 name",
                     "Group 1 name",
                     "Group 1 definition",
                     "host species",
                     "body_site",
                     "sequencing type",
                     "16S variable region (lower bound)",
                     "16S variable region (upper bound)",
                     "sequencing platform",
                     "statistical test",
                     "LDA Score >",
                     "significance threshold",
                     "MHT correction",
                     "matched on",
                     "confounders controlled for",
                     "Pielou",
                     "Shannon",
                     "Chao1",
                     "Simpson", 
                     "Inverse Simpson",
                     "Richness")

exp.export.cols <- c("Study", 
                     "Experiment page name",
                     "Antibiotics exclusion",
                     "Location of subjects", 
                     "Condition", 
                     "Group 0 sample size",
                     "Group 1 sample size",
                     "Group 0 name",
                     "Group 1 name",
                     "Group 1 definition",
                     "Host species",    
                     "Body site",
                     "Sequencing type",
                     "16s variable region",
                     "16s variable region",
                     "Sequencing platform",
                     "Statistical test",
                     "LDA Score above",
                     "Significance threshold",
                     "MHT correction",
                     "Matched on",
                     "Confounders controlled for",
                     "Pielou",
                     "Shannon",
                     "Chao1",
                     "Simpson", 
                     "Inverse Simpson",
                     "Richness")

sig.import.cols <- c("Study", 
                     "Experiment",
                     "Page Name",
                     "Increased abundance in Group 1",
                     "description",
                     "source",
                     "curator")   
 
sig.export.cols <- c("Study", 
                     "Experiment",
                     "Signature page name",
                     "Abundance in Group 1",
                     "Description",
                     "Source",
                     "Curator")
  