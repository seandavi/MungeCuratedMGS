create_keys <- function(prefix, n) paste0(prefix, seq_len(n))

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
