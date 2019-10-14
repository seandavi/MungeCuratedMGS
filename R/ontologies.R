#' Helper function to do download since files are .gzipped
#'
#' @param bpo_url taken from the csv download link of a page like \url{https://bioportal.bioontology.org/ontologies/EFO}
#'
#' @importFrom readr read_csv
.read_ontology_from_bioportal_url = function(bpo_url) {
  z = tempfile(fileext = '.gz')
  download.file(bpo_url, z)
  return(readr::read_csv(z, guess_max=2000, n_max= Inf)[,1:8])
}


#' Download the common columns from EFO
#'
#' See \url{https://bioportal.bioontology.org/ontologies/EFO}
#'
#' @export
get_efo = function() {
  .read_ontology_from_bioportal_url('http://data.bioontology.org/ontologies/EFO/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb&download_format=csv')
}

#' Download the common columns from UBERON
#'
#' See \url{https://bioportal.bioontology.org/ontologies/UBERON}
#'
#' @export
get_uberon = function() {
  .read_ontology_from_bioportal_url("http://data.bioontology.org/ontologies/UBERON/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb&download_format=csv")
}

