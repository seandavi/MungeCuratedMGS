#' download and untar ncbi taxonomy
#'
#' @param download_dir do file manipulation here
#' @param taxon_dump_url url of `taxdump.tar.gz` file at NCBI
#'
#' @return
#'   The path to the directory containing the `.dmp`` files
#'
#' @export
get_taxon_dump_files = function(
  download_dir = tempdir(),
  taxon_dump_url = 'ftp://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump.tar.gz'
) {
  fname = 'taxdump.tar.gz'
  tfile = file.path(download_dir, fname)
  download.file(taxon_dump_url, destfile = tfile)
  untar(tfile, exdir = download_dir)
  return(download_dir)
}


#' get taxon file
#'
#' Generic parser for weird taxon dmp format
#'
#' Run after \link{\code{get_taxon_dump_files}} ans supply
#' the download directory name.
#'
#' @param download_dir The directory as prepared by get_taxon_dump_files
#' @param taxon_file The string name (without tht .dmp) of the file to parse, such as 'name'
#'
#' @importFrom readr read_tsv
get_taxon_file = function(download_dir, taxon_file = 'names') {
  l = readLines(file.path(download_dir, 'names.dmp'))
  l = gsub('\t\\|', '', l)
  l = paste(l, collapse="\n")
  rows = readr::read_tsv(l, col_names = FALSE, quote = '')
  rows
}

#' get and parse the `names.dmp` from taxon dump archive
#'
#' Run after \link{\code{get_taxon_dump_files}} ans supply
#' the download directory name.
#'
#' @param download_dir The directory as prepared by get_taxon_dump_files
#'
#' @importFrom readr read_tsv
#'
#' @export
get_taxon_names = function(download_dir) {
  return(get_taxon_file(download_dir, 'names'))
}


