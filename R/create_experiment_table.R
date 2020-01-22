#' create experiment table
#'
#' This will be the `experiment` data model
#'
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_experiment_table <- function(sheet = curation_sheet()) sheet[, experimentCols()]


#' experimentCols
#'
#' @return A vector with the column names of the experiment table (without key)
#' @export
#'
#' @examples
#' experimentCols()
#' 
experimentCols <- function() {
        c(
            # subjects
            "antibiotics exclusion",
            "location",
            "condition",
            "Group 0 sample size",
            "Group 1 sample size",
            "Group 1 definition",
            "host species",
            "body_site",  
            
            # lab analysis
            "sequencing type",
            "16S variable region (lower bound)",
            "16S variable region (upper bound)",
            "sequencing platform",
            
            # statistical analysis
            "statistical test",
            "LDA Score >",
            "significance threshold",
            "MHT correction",
            "matched on",
            "confounders controlled for",

            # results
            "Pielou",
            "Shannon",                       
            "Chao1",
            "Simpson",
            "Inverse Simpson",
            "Richness"
        )
}
