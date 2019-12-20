#' create location table
#'
#' This will be the `location` data model
#'
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @export
create_location_table <- function() 
{
    vfile <- system.file('extdata/validation.csv', package = "MungeCuratedMGS")
    sheet <- readr::read_csv(vfile)
    sheet <- sheet[,2]
    colnames(sheet) <- "country"
    tibble(primary_key = create_keys("LOC", nrow(sheet)), 
           country = sheet$country)
}
