#' create_keys
#'
#' @param prefix A prefix
#' @param x A list or data.frame
#'
#' @return A key that is the same for any identical rows 
#' and different for non-identical rows
#' @export
#'
#' @examples
#' (df <- data.frame(a=c(1, 1, 5, 6), b=c(1, 1, 5, 9)))
#' create_keys("key", df)
create_keys <- function(prefix, x){
    string <- Reduce(paste, x)
    l1 <- rank(string, ties.method = "min")
    l1 <- factor(l1, labels = seq(length(unique(l1))))
    l1 <- formatC(l1, format="d", width=3, flag="0")
    paste0(prefix, l1)
}

metaphlan2ncbi <- function(mid)
{
    ext.dir <- system.file("extdata", package = "MungeCuratedMGS")
    mfile <- file.path(ext.dir, "metaphlan2ncbi.txt.gz")
    map.df <- readr::read_tsv(mfile, 
                                col_names = c("metaphlan", "ncbi"),
                                col_types = "cc")
    metaphlan2ncbi <- map.df$ncbi
    names(metaphlan2ncbi) <- map.df$metaphlan
    metaphlan2ncbi  
}
