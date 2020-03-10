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

#' metaphlan2ncbi
#'
#' @param metaphlan.version character. Choose between 2.0 or 3.0.  
#' @return A named vector storing the mapping from MetaPhlan names to NCBI tax IDs
#' @export
metaphlan2ncbi <- function(metaphlan.version = c("2.0", "3.0"))
{
    metaphlan.version <- match.arg(metaphlan.version)
    ext.dir <- system.file("extdata", package = "MungeCuratedMGS")
    mfile <- ifelse(metaphlan.version == "2.0", 
                    "metaphlan2ncbi.txt.gz", 
                    "mpa_v30_CHOCOPhlAn_201901_taxonomy.txt.gz")
    mfile <- file.path(ext.dir, mfile)
    map.df <- readr::read_tsv(mfile, 
                                col_names = c("metaphlan", "ncbi"),
                                col_types = "cc")

    if(metaphlan.version == "3.0")
    {
        .f <- function(x)
        {
            spl <- unlist(strsplit(x, "\\|"))
            spl[length(spl)]
        }
        map.df$ncbi <- vapply(map.df$ncbi, .f, character(1), USE.NAMES = FALSE)
        colnames(map.df) <- c("metaphlan", "ncbi")
    }
    metaphlan2ncbi <- map.df$ncbi
    names(metaphlan2ncbi) <- map.df$metaphlan
    metaphlan2ncbi  
}
