#' create signature metadata table
#'
#' This will be the `signature metadata` data model
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_signature_table = function(sheet = curation_sheet()) {
  sheet <- sheet %>%
    dplyr::select(
      Pielou,
      `Shannon index`,
      Chao1,
      Simpson,
      `Inverse Simpson`,
      `richness (Specie's Diversity)`,
    )

    dplyr::tibble(  primary_key = create_keys("SIG", nrow(sheet)),
                    metadata = create_keys("SIGMET", nrow(sheet)),
                    contrast = create_keys("CON", nrow(sheet)),
                    body_site = sheet[["body_site"]],
                    condition = sheet[["condition"]],
                    date = sheet[["date of curation"]],
                    curator = sheet[["curator"]],
                    revision = sheet[["revision"]])
    
}
