#' create the statistical test table
#'
#' @importFrom readr read_tsv
#'
#' @export
create_statistical_test_table <- function()
{
    sfile <- system.file('extdata/statisticalTest.tsv', package = "MungeCuratedMGS")
    readr::read_tsv(sfile)
}
