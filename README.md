
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigvis

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sigvis)](https://CRAN.R-project.org/package=sigvis)
[![R-CMD-check](https://github.com/selkamand/sigvis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/sigvis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Create interactive visualisations that summarise mutational signature
analyses

## Installation

You can install the development version of sigvis like so:

``` r
# install.packages('remotes')
remotes::install_github('selkamand/sigvis')
```

## Usage

### Visualise Signatures

``` r
library(sigverse)
#> ── Attaching core sigverse packages ───────────────────── sigverse 0.0.0.9000 ──
#> ✔ sigshared 0.0.0.9000     ✔ sigstats  0.0.0.9000
#> ✔ sigsim    0.0.0.9000     ✔ sigstory  0.0.0.9000
#> ✔ sigstash  0.0.0.9000     ✔ sigvis    0.0.0.9000

# Load Signature 
signatures <- sig_load('COSMIC_v3.3.1_SBS_GRCh38')

# Visualise a single signature
sig_visualise(signatures[["SBS2"]], title = "SBS2")
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Visualise Decompositions

``` r
# Load library containing results of TCGA mutational signature analysis
library(TCGAdecomp)

# Load decompositions from sigstash
brca_decompositions <- decomp_load('BRCA')

# Visualise a decomposition
sig_visualise(
  brca_decompositions[['TCGA-D8-A1XU-01A-11D-A14K-09']], 
  class = "decomposition", 
  title = 'TCGA-D8-A1XU-01A-11D-A14K-09'
)
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Visualise a Signature Model

Visualise the decomposition expected from a signature model, where
signature ‘SBS2’ explains 60% of the mutations in a sample, and ‘SBS13’
explains the remaining 40%

``` r

# Visualise a model (combination of signatures)

model = sig_combine(signatures, model = c('SBS2' = 0.6, 'SBS13' = 0.4))
sig_visualise(model, class = 'model')
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
