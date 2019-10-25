# Installation

```{r eval=FALSE}
BiocManager::install('seandavi/MungeCuratedMGS')
```

```{r message=FALSE, warning=FALSE}
library(MungeCuratedMGS)
```

# Data entities


See [working document](https://docs.google.com/spreadsheets/d/11i0qB4EXyk34qfms48GlDIh977rWqsjS0-G9_gk88BM/edit?usp=sharing) for details of where we are headed.


## Signatures

Before running this vignette

```{r eval=FALSE}
library(googlesheets)
token = gs_auth()
saveRDS(token, file = "/tmp/googlesheets_token.rds")
```



```{r sig_table_from_gs}
library(googlesheets)
suppressMessages(gs_auth(token = "/tmp/googlesheets_token.rds", verbose = FALSE))
sig_table = create_signatures_table()
head(sig_table)
```



## Ontologies

### EFO

```{r efo}
efo = get_efo()
head(efo)
```

### UBERON

```{r uberon}
uberon = get_uberon()
head(uberon)
```

Ontologies can be combined like so:

```{r}
library(dplyr)
full_ontologies = dplyr::bind_rows(uberon, efo)
```

## NCBI Taxonomy

### Names and Taxon IDs

```{r taxon_names}
taxon_dir = get_taxon_dump_files()
taxon_id_names = get_taxon_names(taxon_dir)
head(taxon_id_names, 10)
```
