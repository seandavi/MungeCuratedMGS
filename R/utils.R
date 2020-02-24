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
#' (df <- data.frame(a=c(9, 9, 5, 6), b=c(9, 9, 5, 9)))
#' create_keys("key", df)
create_keys <- function(prefix, x){
    string <- Reduce(paste, x)
    l1 <- rank(string, ties.method = "min")
    ul1 <- rle(l1)$lengths
    keys <- rep(seq_along(ul1), ul1)
    paste0(prefix, keys)
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
