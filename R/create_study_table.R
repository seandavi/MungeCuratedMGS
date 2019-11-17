#' create study table
#'
#' This will be the `study` data model
#'
#' @param sheet a data.frame, the curation worksheet from google sheet
#'     (sheet 2 as of now)
#'
#' @export
create_study_table <- function() {
    studymeta <- readr::read_csv(
        system.file(package = "MungeCuratedMGS",
        "resources/validate/studyMeta.csv",
        mustWork = TRUE)
    )
    cnames <- studymeta[["name"]]
    ctypes <- substr(studymeta[["rdatatype"]], 1, 1)
    collist <- as.list(ctypes)
    names(collist) <- cnames
    readtable <- readr::read_csv(
        file = system.file(package="MungeCuratedMGS", 'extdata/curation.csv.gz'),
        skip = 1,
        # file = "inst/extdata/curation.csv",
        # comment = "#",
        col_types = do.call(readr::cols_only, collist)
    )
    reqnames <- cnames[studymeta[["required"]]]
    if (!all(reqnames %in% names(readtable)))
        stop("Required column(s) not found: ", reqnames)
    readtable
}

# create_study_table <- function(sheet = curation_sheet()) {
#     sheet %>%
#         dplyr::select(
#             `sequencing type`,
#             `16S variable region`,
#             `sequencing platform`,
#             `study design`,
#             `matched on`,
#             `confounders controlled for`,
#             `antibiotics exclusion`,
#             `Country`
#             # , `location`, `citation`
#         )
# }
