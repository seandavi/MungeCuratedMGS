#' create alpha diversity table
#'
#' This will be the alpha diversity data model
#'
#' @importFrom dplyr select tibble
#' @importFrom magrittr %>%
#' @param curation_sheet a data.frame, the curation worksheet from google sheet (sheet 2 as of now)
#'
#' @export
create_diversity_table = function(sheet = curation_sheet()) {
  sheet <- sheet %>%
    dplyr::select(
      Pielou,
      `Shannon index`,
      Chao1,
      Simpson,
      `Inverse Simpson`,
      `richness (Specie's Diversity)`,
    )

    dplyr::tibble(  primary_key = create_keys("DIV", nrow(sheet)),
                    signature = create_keys("SIG", nrow(sheet)),
                    metadata = create_keys("SIGMET", nrow(sheet)),
                    Pielou = sheet[["Pielou"]],
                    Shannon = sheet[["Shannon index"]],
                    Chao1 = sheet[["Chao1"]],
                    Simpson = sheet[["Simpson"]],
                    InverseSimpson = sheet[["Inverse Simpson"]],
                    Richness = sheet[["richness (Specie\'s Diversity)"]])
    
}
