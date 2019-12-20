#' create signature metadata table
#'
#' This will be the `signature metadata` data model
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_signaturemetadata_table = function(sheet = curation_sheet()) {
  sheet <- sheet %>%
    dplyr::select(
      PMID,
      `source within paper`,
      `Free-form description`,
      `contrast (list control group last)`,
      body_site,
      condition,
      `date of curation`,
      curator,
      revision
    )

    cit.tab <- create_citation_table()
    ind <- match(sheet$PMID, cit.tab$PMID)
    cit <- cit.tab[ind, "primary_key"]    

    stud.tab <- create_study_table()
    ind <- match(cit$primary_key, stud.tab$citation)
    stud <- stud.tab[ind, "primary_key"]

    con.tab <- create_contrast_table()
    
    dplyr::tibble(  primary_key = create_keys("SIGMET", nrow(sheet)),
                    study = stud[["primary_key"]],
                    source = sheet[["source within paper"]],
                    description = sheet[["Free-form description"]], 
                    contrast = create_keys("CON", nrow(sheet)),
                    body_site = sheet[["body_site"]],
                    condition = sheet[["condition"]],
                    date = sheet[["date of curation"]],
                    curator = sheet[["curator"]],
                    revision = sheet[["revision"]])
    
}
