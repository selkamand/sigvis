
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigvis

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sigvis)](https://CRAN.R-project.org/package=sigvis)
[![R-CMD-check](https://github.com/selkamand/sigvis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/sigvis/actions/workflows/R-CMD-check.yaml)
![GitHub Issues or Pull
Requests](https://img.shields.io/github/issues-closed/selkamand/ggoncoplot)
[![code
size](https://img.shields.io/github/languages/code-size/selkamand/sigvis.svg)](https://github.com/selkamand/sigvis)
![GitHub last
commit](https://img.shields.io/github/last-commit/selkamand/sigvis)
<!-- badges: end -->

Create interactive visualisations that summarise mutational signature
analyses

## Installation

You can install the development version of sigvis like so:

``` r
if (!require("pak", quietly = TRUE))
    install.packages("pak")

pak::pak("selkamand/sigverse")
```

## Usage

### Visualise Signatures

``` r
library(sigvis)
library(sigstash)

# Load Signature
signatures <- sig_load("COSMIC_v3.3.1_SBS_GRCh38")

# Select just SBS2
SBS2 <- signatures[["SBS2"]]

# Plot
sig_visualise(signature = SBS2, title = "SBS2")
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Visualise catalogues

``` r
# Load library containing results of TCGA mutational signature analysis
library(TCGAcatalogues)


# Load a catalogue
tally <- catalogues_load("BRCA", type = "SBS_96")

# Select a single sample of interest
sample = "TCGA-5L-AAT1-01A-12D-A41F-09"
tally_single_sample <- tally[[sample]]

tally_single_sample
#> # A tibble: 96 × 4
#>    channel type  fraction count
#>    <chr>   <chr>    <dbl> <int>
#>  1 A[C>A]A C>A   0.00151      3
#>  2 A[C>A]C C>A   0.00151      3
#>  3 A[C>A]G C>A   0.000503     1
#>  4 A[C>A]T C>A   0            0
#>  5 A[C>G]A C>G   0.00151      3
#>  6 A[C>G]C C>G   0.00151      3
#>  7 A[C>G]G C>G   0            0
#>  8 A[C>G]T C>G   0.00201      4
#>  9 A[C>T]A C>T   0.00402      8
#> 10 A[C>T]C C>T   0.00151      3
#> # ℹ 86 more rows

# Visualise Observed Mutational Profile
sig_visualise(tally_single_sample, class = "catalogue", title = sample)
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Visualise a Signature Model

Visualise the catalogue expected from a signature model, where signature
‘SBS2’ explains 60% of the mutations in a sample, and ‘SBS13’ explains
the remaining 40%

``` r
# Load sigstats library for combining signature models
library(sigverse)
#> ── Attaching core sigverse packages ───────────────────── sigverse 0.0.0.9000 ──
#> ✔ sigshared 0.0.0.9000     ✔ sigstats  0.0.0.9000
#> ✔ sigsim    0.0.0.9000     ✔ sigstory  0.0.0.9000

# Visualise a model (combination of signatures)
model = sig_combine(signatures, model = c('SBS2' = 0.6, 'SBS13' = 0.4), format = "combined")
sig_visualise(model, class = 'model', title = "Model", subtitle = "Combination of SBS2 (60%) and SBS13 (40%)")
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Visualise a Signature Model Against the Observed Mutation Catalogue

Visualise the catalogue expected from a signature model, where signature
‘SBS2’ explains 60% of the mutations in a sample, and ‘SBS13’ explains
the remaining 40%, but overlay on top of the observed mutational process

``` r
library(TCGAcatalogues) # For pulling example TCGA catalogue data

# Load Signature
signatures <- sig_load("COSMIC_v3.3.1_SBS_GRCh38")

# Create a model (combination of signatures)
model <- sig_combine(signatures, model = c('SBS2' = 0.6, 'SBS13' = 0.4), format = "signature")

# Load a catalogue (Tally of variant types)
tally <- catalogues_load("BRCA", type = "SBS_96")

# Get tally of a single sample
sample = "TCGA-5L-AAT1-01A-12D-A41F-09"
tally_single_sample <- tally[[sample]]

# Visualise the overlay
sig_visualise_compare_reconstructed_to_observed(
  catalogue = tally_single_sample,
  signature = model
)
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### Visualise Bootstraps

``` r
# Small Example
bootstraps <- example_bootstraps()
sig_visualise_bootstraps(bootstraps)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r

# Larger (Horizontal) Example
colo829_bootstraps <- example_bootstraps_colo829()
sig_visualise_bootstraps(colo829_bootstraps, min_contribution_threshold = 0.05, pvalue = 0.05, horizontal = TRUE)
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

### Visualise Signature Dotplot

Visualise the contributions of a signature across an entire cohort (or a
collection of bootstrap experiments)

``` r
dotplot_data <- example_dotplot_data()
sig_visualise_dotplot(
  dotplot_data, 
  col_sample = "sample", 
  col_contribution = "contribution",
  col_fill = "sample_info",
  palette_fill = c(sample_of_interest = "#D55E00", "cancer_type"="#009E73", "other" = "#000000"),
  sort_by = "palette_fill",
  xlab = "SBS2"
  )
```

<img src="man/figures/README-signature_dotplot-1.png" width="100%" />

### Visualise Minimal Signature Plot + Proportion Bar

Rendering a minimal version of a signature or catalogue with an
accompanying proportion bar can be highly informative. This
visualization technique allows you to display a signature from your
model alongside the proportion of all mutations that the signature
explains. Additionally, if you’ve conducted a similarity analysis on
mutation catalogues, you can use these plots to visualize both the most
similar sample’s catalogue and its cosine similarity to your reference
sample.

For practical examples of how these plots are utilized, see
[sigstory](https://github.com/selkamand/sigstory)

``` r

# Visualize Signature SBS2 that explains 60% of a sample
sig_visualise_minified(signature = SBS2, proportion = 0.6)
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-minified-1.png" width="100%" />

``` r

# Visualize a sample catalogue with a cosine similarity of 0.3 to a sample of interest,
sig_visualise_minified(
  tally_single_sample,
  proportion = 0.3,
  format = fmt_round(digits = 2)
)
#> ✔ All channels matched perfectly to set [sbs_96]. Using this set for sort order
#> ✔ All types matched perfectly to set [sbs_type]. Using this set for sort order
#> ✔ Types matched perfectly to palette [snv_type]
```

<img src="man/figures/README-minified-2.png" width="100%" />

### Visualise Signature Model as Donut

``` r
model = c('SBS2' = 0.6, 'SBS13' = 0.2)
sig_visualise_donut(model, palette = c(SBS2="#E72826", SBS13="black", "Unexplained" = "grey90"))
```

<img src="man/figures/README-donut_model-1.png" width="100%" />
