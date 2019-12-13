#' create the statistical test table
#'
#' @importFrom readr read_tsv
#'
#' @export
create_statistical_test_table <- function()
{
    sfile <- system.file('extdata/statisticalTest.tsv', package = "MungeCuratedMGS")
    stbl <- readr::read_tsv(sfile)
    stbl$primary_key <- create_keys("STAT", nrow(stbl))
    stbl[,c(ncol(stbl), 1:(ncol(stbl) - 1))]
}
