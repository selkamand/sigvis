
# Colour Palettes ---------------------------------------------------------

#' Palettes
#'
#' Colour palettes for common signature types
#'
#' @return Named vector. Names are types, values are colours
#' @export
#'
#' @examples
#' sig_palette_snv_type()
#' sig_palette_doublet_type()
#' sig_palette_indel_type()
#'
#' @details
#' Palettes are consistent with COSMIC signature database (https://cancer.sanger.ac.uk/signatures/)
#'
sig_palette_snv_type = function(){
  c(
    "C>A" = "#16AFE9",
    "C>G" = "black",
    "C>T" = "#D9111E",
    "T>A" = "#C1BDBE",
    "T>C" = "#92C446",
    "T>G" = "#E6B8B9"
  )
}

#' @inherit sig_palette_snv_type title description details seealso sections references examples
#' @export
sig_palette_doublet_type = function(){
 c(
   "AC>NN" = "#03BDEE",
   "AT>NN" = "#0266CC",
   "CC>NN" = "#A3CD61",
   "CG>NN" = "#016500",
   "CT>NN" = "#FE9798",
   "GC>NN" = "#E42A24",
   "TA>NN" = "#FEAF64",
   "TC>NN" = "#FC8002",
   "TG>NN" = "#CA99FB",
   "TT>NN" = "#4B029A"
 )
}

#' @inherit sig_palette_snv_type title description details seealso sections references examples
#' @export
sig_palette_indel_type = function(){
  c(
    "1:Del:C" = '#FAB15B',
    "1:Del:T" = "#FA6A0C",
    "1:Ins:C" = "#A1D670",
    "1:Ins:T" = "#2F9024",
    "2:Del:R" = "#F7BCA5",
    "3:Del:R" = "#F97356",
    "4:Del:R" = "#E72D28",
    "5:Del:R" = "#AC0016",
    "2:Ins:R" = "#C6D9EC",
    "3:Ins:R" = "#83B6DA",
    "4:Ins:R" = "#3D81BB",
    "5:Ins:R" = "#164C9A",
    "2:Del:M" = "#DBD9E2",
    "3:Del:M" = "#A6A3CF",
    "4:Del:M" = "#716DAD",
    "5:Del:M" = "#4E2C84"
  )
}


#' @inherit sig_palette_snv_type title description details seealso sections references examples
#' @export
sig_palette_cn_type = function(){
  c(
    "0" = "#0600CF",
    "1" = "#545454",
    "2" = "#228935",
    "3-4" = "#7F1CC8",
    "5-8" = "#D28526",
    "9+" = "#850B49"
  )
}

#' @inherit sig_palette_snv_type title description details seealso sections references examples
#' @export
sig_palette_cn2_type = function(){
  c(
    "HD:0" = "#0600CF",
    "LOH:1" = "#545454",
    "LOH:2" = "#228935",
    "LOH:3-4" = "#7F1CC8",
    "LOH:5-8" = "#D28526",
    "LOH:9+" = "#850B49",
    "Het:2" = "#1E8D2E",
    "Het:3-4" = "#7F1CC8",
    "Het:5-8:" = "#D1812A",
    "Het:9+" = "#850B49"
  )
}

#' @inherit sig_palette_snv_type title description details seealso sections references examples
#' @export
sig_palette_sv_type <- function(){
  c(
    "Deletion" = "#8B4512",
    "Tandem Duplication" = "#A72624",
    "Inversion" = "#038791",
    "Translocation" = "#068C82"
  )
}

#' @inherit sig_palette_snv_type title description details seealso sections references examples
#' @export
sig_palette_rnasnv_type <- function(){
  c(
    "C>A" = "#20BDF3",
    "C>G" = "#060709",
    "C>T" = "#E72826",
    "T>A" = "#CACACA",
    "T>C" = "#A5CE61",
    "T>G" = "#EEC8C5",
    "G>T" = "#98D4EA",
    "G>C" = "#5D6164",
    "G>A" = "#E06D15",
    "A>T" = "#EAE8EB",
    "A>G" = "#DBF578",
    "A>C" = "#F7DADB"
  )
}

#' The set2 colour palette
#'
#' @return the set2 colour paletted as a named vector
#' @export
#'
pal_set2 <- function(){
  c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
    "#E5C494", "#B3B3B3")
}


# Autoselect Palette ---------------------------------------


#' Automatically select a color palette based on signature types
#'
#' This function takes a vector of signature 'types' and selects an appropriate color palette.
#' It checks if the types match predefined palettes and
#' returns the matching palette or a default palette [pal_set2()] if no exact match is found.
#'
#' @param types A character vector of types e.g. from a sigverse signature 'type' column.
#' @param default The default color palette to use if no exact match is found.
#' @param verbose verbose (flag)
#' @return A color palette (named vector where names are types and values are colours).
#'
#'
#' @examples
#' \dontrun{# Example usage:
#' auto_palette(c("T>G", "T>G"))
#' }
#'
auto_palette <- function(types, default = pal_set2(), verbose = TRUE){
  assertions::assert_character(types)
  types <- na.omit(types)

  ls_pals <- list(
    snv_type = sig_palette_snv_type(),
    indel_type = sig_palette_indel_type(),
    dbs_type = sig_palette_doublet_type(),
    cn_type = sig_palette_cn_type(),
    cn2_type = sig_palette_cn2_type(),
    sv_type = sig_palette_sv_type(),
    rnasnv_type = sig_palette_rnasnv_type()
    )

  # Check if there are any duplicated palettes
  indistinguishable_palettes <- get_indistinguishable_palettes(ls_pals)
  if(length(indistinguishable_palettes) > 0){
   cli::cli_abort(
     "Not all sigvis palettes are distinguishable.
     Please create a new issue at {.url https://github.com/selkamand/sigvis/issues} flagging that the following palettes are indistinguishable:
     [{indistinguishable_palettes}]",
     )
  }

  n_matches = vapply(ls_pals, \(pal){ sum(types %in% names(pal)) }, FUN.VALUE = numeric(1))
  names(n_matches) <- names(n_matches)
  palette_perfectly_match_status = vapply(ls_pals, \(pal){ setequal(types, names(pal)) }, FUN.VALUE = logical(1))

  all_matched <- n_matches == length(types)
  palettes_matched <- names(Filter(f = isTRUE, palette_perfectly_match_status))

  if(sum(palette_perfectly_match_status) == 1){
    if(verbose) cli::cli_alert_success('Types matched perfectly to palette [{palettes_matched}]')
    return(ls_pals[[palettes_matched]])
  }
  else if(length(palettes_matched) == 0 & any(n_matches >= 2)){
    index_best_match <- which.max(n_matches)
    best_match_n = n_matches[index_best_match]
    best_match <- names(n_matches)[index_best_match]
    number_types = length(types)

    not_matched <- ls_pals[[best_match]]
    if(verbose) {
      cli::cli_alert_warning(
      'No exact palette match, returning default
      The [{best_match}] palette was the closest match, with {best_match_n} / {number_types} type{?s} matching.'
      )
    }
    return(default)
  }
  else{
    if(verbose) cli::cli_alert_warning('No exact palette matches, returning default. It is highly recommended to supply a custom palette. See the {.arg palette} argument')
    return(default)
  }
}


# Channel/Type Levels For Ordering ----------------------------------------------------------
levels_snv_type = function(){
  c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
}



levels_snv = function(){
  # Create a character vector with the mutation signature analysis catalogue channels
  mutation_channels <- c(
    "A[C>A]A", "A[C>A]C", "A[C>A]G", "A[C>A]T", "C[C>A]A", "C[C>A]C", "C[C>A]G", "C[C>A]T",
    "G[C>A]A", "G[C>A]C", "G[C>A]G", "G[C>A]T", "T[C>A]A", "T[C>A]C", "T[C>A]G", "T[C>A]T",

    "A[C>G]A", "A[C>G]C", "A[C>G]G", "A[C>G]T", "C[C>G]A", "C[C>G]C", "C[C>G]G", "C[C>G]T",
    "G[C>G]A", "G[C>G]C", "G[C>G]G", "G[C>G]T", "T[C>G]A", "T[C>G]C", "T[C>G]G", "T[C>G]T",

    "A[C>T]A", "A[C>T]C", "A[C>T]G", "A[C>T]T", "C[C>T]A", "C[C>T]C", "C[C>T]G", "C[C>T]T",
    "G[C>T]A", "G[C>T]C", "G[C>T]G", "G[C>T]T", "T[C>T]A", "T[C>T]C", "T[C>T]G", "T[C>T]T",

    "A[T>A]A", "A[T>A]C", "A[T>A]G", "A[T>A]T", "C[T>A]A", "C[T>A]C", "C[T>A]G", "C[T>A]T",
    "G[T>A]A", "G[T>A]C", "G[T>A]G", "G[T>A]T", "T[T>A]A", "T[T>A]C", "T[T>A]G", "T[T>A]T",

    "A[T>C]A", "A[T>C]C", "A[T>C]G", "A[T>C]T", "C[T>C]A", "C[T>C]C", "C[T>C]G", "C[T>C]T",
    "G[T>C]A", "G[T>C]C", "G[T>C]G", "G[T>C]T", "T[T>C]A", "T[T>C]C", "T[T>C]G", "T[T>C]T",

    "A[T>G]A", "A[T>G]C", "A[T>G]G", "A[T>G]T", "C[T>G]A", "C[T>G]C", "C[T>G]G", "C[T>G]T",
    "G[T>G]A", "G[T>G]C", "G[T>G]G", "G[T>G]T", "T[T>G]A", "T[T>G]C", "T[T>G]G", "T[T>G]T"
  )

  return(mutation_channels)
}

levels_indel_type <- function(){
  c(
    "1:Del:C", "1:Del:T", "1:Ins:C", "1:Ins:T", "2:Del:R", "3:Del:R",
    "4:Del:R", "5:Del:R", "2:Ins:R", "3:Ins:R", "4:Ins:R", "5:Ins:R",
    "2:Del:M", "3:Del:M", "4:Del:M", "5:Del:M"
  )
}

levels_indel <- function(){
  c("1:Del:C:0", "1:Del:C:1", "1:Del:C:2", "1:Del:C:3", "1:Del:C:4",
    "1:Del:C:5", "1:Del:T:0", "1:Del:T:1", "1:Del:T:2", "1:Del:T:3",
    "1:Del:T:4", "1:Del:T:5", "1:Ins:C:0", "1:Ins:C:1", "1:Ins:C:2",
    "1:Ins:C:3", "1:Ins:C:4", "1:Ins:C:5", "1:Ins:T:0", "1:Ins:T:1",
    "1:Ins:T:2", "1:Ins:T:3", "1:Ins:T:4", "1:Ins:T:5", "2:Del:R:0",
    "2:Del:R:1", "2:Del:R:2", "2:Del:R:3", "2:Del:R:4", "2:Del:R:5",
    "3:Del:R:0", "3:Del:R:1", "3:Del:R:2", "3:Del:R:3", "3:Del:R:4",
    "3:Del:R:5", "4:Del:R:0", "4:Del:R:1", "4:Del:R:2", "4:Del:R:3",
    "4:Del:R:4", "4:Del:R:5", "5:Del:R:0", "5:Del:R:1", "5:Del:R:2",
    "5:Del:R:3", "5:Del:R:4", "5:Del:R:5", "2:Ins:R:0", "2:Ins:R:1",
    "2:Ins:R:2", "2:Ins:R:3", "2:Ins:R:4", "2:Ins:R:5", "3:Ins:R:0",
    "3:Ins:R:1", "3:Ins:R:2", "3:Ins:R:3", "3:Ins:R:4", "3:Ins:R:5",
    "4:Ins:R:0", "4:Ins:R:1", "4:Ins:R:2", "4:Ins:R:3", "4:Ins:R:4",
    "4:Ins:R:5", "5:Ins:R:0", "5:Ins:R:1", "5:Ins:R:2", "5:Ins:R:3",
    "5:Ins:R:4", "5:Ins:R:5", "2:Del:M:1", "3:Del:M:1", "3:Del:M:2",
    "4:Del:M:1", "4:Del:M:2", "4:Del:M:3", "5:Del:M:1", "5:Del:M:2",
    "5:Del:M:3", "5:Del:M:4", "5:Del:M:5")
}


levels_doublet_type <- function(){
  c("AC>NN", "AT>NN", "CC>NN", "CG>NN", "CT>NN", "GC>NN", "TA>NN",
    "TC>NN", "TG>NN", "TT>NN")
}

levels_dbs <- function(){
  c("AC>CA", "AC>CG", "AC>CT", "AC>GA", "AC>GG", "AC>GT", "AC>TA",
    "AC>TG", "AC>TT", "AT>CA", "AT>CC", "AT>CG", "AT>GA", "AT>GC",
    "AT>TA", "CC>AA", "CC>AG", "CC>AT", "CC>GA", "CC>GG", "CC>GT",
    "CC>TA", "CC>TG", "CC>TT", "CG>AT", "CG>GC", "CG>GT", "CG>TA",
    "CG>TC", "CG>TT", "CT>AA", "CT>AC", "CT>AG", "CT>GA", "CT>GC",
    "CT>GG", "CT>TA", "CT>TC", "CT>TG", "GC>AA", "GC>AG", "GC>AT",
    "GC>CA", "GC>CG", "GC>TA", "TA>AT", "TA>CG", "TA>CT", "TA>GC",
    "TA>GG", "TA>GT", "TC>AA", "TC>AG", "TC>AT", "TC>CA", "TC>CG",
    "TC>CT", "TC>GA", "TC>GG", "TC>GT", "TG>AA", "TG>AC", "TG>AT",
    "TG>CA", "TG>CC", "TG>CT", "TG>GA", "TG>GC", "TG>GT", "TT>AA",
    "TT>AC", "TT>AG", "TT>CA", "TT>CC", "TT>CG", "TT>GA", "TT>GC",
    "TT>GG")
}


levels_cn <- function(){
  c("0:homdel:0-100kb", "0:homdel:100kb-1Mb", "0:homdel:>1Mb",
    "1:LOH:0-100kb", "1:LOH:100kb-1Mb", "1:LOH:1Mb-10Mb", "1:LOH:10Mb-40Mb", "1:LOH:>40Mb",
    "2:LOH:0-100kb", "2:LOH:100kb-1Mb", "2:LOH:1Mb-10Mb", "2:LOH:10Mb-40Mb", "2:LOH:>40Mb",
    "3-4:LOH:0-100kb", "3-4:LOH:100kb-1Mb", "3-4:LOH:1Mb-10Mb", "3-4:LOH:10Mb-40Mb", "3-4:LOH:>40Mb",
    "5-8:LOH:0-100kb", "5-8:LOH:100kb-1Mb", "5-8:LOH:1Mb-10Mb", "5-8:LOH:10Mb-40Mb", "5-8:LOH:>40Mb",
    "9+:LOH:0-100kb", "9+:LOH:100kb-1Mb", "9+:LOH:1Mb-10Mb", "9+:LOH:10Mb-40Mb", "9+:LOH:>40Mb",
    "2:het:0-100kb", "2:het:100kb-1Mb", "2:het:1Mb-10Mb", "2:het:10Mb-40Mb", "2:het:>40Mb",
    "3-4:het:0-100kb", "3-4:het:100kb-1Mb", "3-4:het:1Mb-10Mb", "3-4:het:10Mb-40Mb", "3-4:het:>40Mb",
    "5-8:het:0-100kb", "5-8:het:100kb-1Mb", "5-8:het:1Mb-10Mb", "5-8:het:10Mb-40Mb", "5-8:het:>40Mb",
    "9+:het:0-100kb", "9+:het:100kb-1Mb", "9+:het:1Mb-10Mb", "9+:het:10Mb-40Mb", "9+:het:>40Mb"
  )
}

levels_cn_type <- function(){
  c("0", "1", "2", "3-4", "5-8", "9+")
}


levels_cn2_type <- function(){
  c("HD:0", "LOH:1", "LOH:2", "LOH:3-4", "LOH:5-8", "LOH:9+", "Het:2",
    "Het:3-4", "Het:5-8", "Het:9+")
}


levels_sv <- function(){
  c("clustered_del_1-10Kb", "clustered_del_10-100Kb", "clustered_del_100Kb-1Mb",
    "clustered_del_1Mb-10Mb", "clustered_del_>10Mb", "clustered_tds_1-10Kb",
    "clustered_tds_10-100Kb", "clustered_tds_100Kb-1Mb", "clustered_tds_1Mb-10Mb",
    "clustered_tds_>10Mb", "clustered_inv_1-10Kb", "clustered_inv_10-100Kb",
    "clustered_inv_100Kb-1Mb", "clustered_inv_1Mb-10Mb", "clustered_inv_>10Mb",
    "clustered_trans", "non-clustered_del_1-10Kb", "non-clustered_del_10-100Kb",
    "non-clustered_del_100Kb-1Mb", "non-clustered_del_1Mb-10Mb",
    "non-clustered_del_>10Mb", "non-clustered_tds_1-10Kb", "non-clustered_tds_10-100Kb",
    "non-clustered_tds_100Kb-1Mb", "non-clustered_tds_1Mb-10Mb",
    "non-clustered_tds_>10Mb", "non-clustered_inv_1-10Kb", "non-clustered_inv_10-100Kb",
    "non-clustered_inv_100Kb-1Mb", "non-clustered_inv_1Mb-10Mb",
    "non-clustered_inv_>10Mb", "non-clustered_trans")
}

levels_sv_type <- function(){
  c("Deletion",
    "Tandem Duplication",
    "Inversion",
    "Translocation"
  )
}

levels_rnasnv <- function(){
  c(
    "A[C>A]A", "A[C>A]C", "A[C>A]G", "A[C>A]T",
    "C[C>A]A", "C[C>A]C", "C[C>A]G", "C[C>A]T",
    "G[C>A]A", "G[C>A]C", "G[C>A]G", "G[C>A]T",
    "T[C>A]A", "T[C>A]C", "T[C>A]G", "T[C>A]T",

    "A[C>G]A", "A[C>G]C", "A[C>G]G", "A[C>G]T",
    "C[C>G]A", "C[C>G]C", "C[C>G]G", "C[C>G]T",
    "G[C>G]A", "G[C>G]C", "G[C>G]G", "G[C>G]T",
    "T[C>G]A", "T[C>G]C", "T[C>G]G", "T[C>G]T",

    "A[C>T]A", "A[C>T]C", "A[C>T]G", "A[C>T]T",
    "C[C>T]A", "C[C>T]C", "C[C>T]G", "C[C>T]T",
    "G[C>T]A", "G[C>T]C", "G[C>T]G", "G[C>T]T",
    "T[C>T]A", "T[C>T]C", "T[C>T]G", "T[C>T]T",

    "A[T>A]A", "A[T>A]C", "A[T>A]G", "A[T>A]T",
    "C[T>A]A", "C[T>A]C", "C[T>A]G", "C[T>A]T",
    "G[T>A]A", "G[T>A]C", "G[T>A]G", "G[T>A]T",
    "T[T>A]A", "T[T>A]C", "T[T>A]G", "T[T>A]T",

    "A[T>C]A", "A[T>C]C", "A[T>C]G", "A[T>C]T",
    "C[T>C]A", "C[T>C]C", "C[T>C]G", "C[T>C]T",
    "G[T>C]A", "G[T>C]C", "G[T>C]G", "G[T>C]T",
    "T[T>C]A", "T[T>C]C", "T[T>C]G", "T[T>C]T",

    "A[T>G]A", "A[T>G]C", "A[T>G]G", "A[T>G]T",
    "C[T>G]A", "C[T>G]C", "C[T>G]G", "C[T>G]T",
    "G[T>G]A", "G[T>G]C", "G[T>G]G", "G[T>G]T",
    "T[T>G]A", "T[T>G]C", "T[T>G]G", "T[T>G]T",

    "A[G>T]A", "A[G>T]C", "A[G>T]G", "A[G>T]T",
    "C[G>T]A", "C[G>T]C", "C[G>T]G", "C[G>T]T",
    "G[G>T]A", "G[G>T]C", "G[G>T]G", "G[G>T]T",
    "T[G>T]A", "T[G>T]C", "T[G>T]G", "T[G>T]T",

    "A[G>C]A", "A[G>C]C", "A[G>C]G", "A[G>C]T",
    "C[G>C]A", "C[G>C]C", "C[G>C]G", "C[G>C]T",
    "G[G>C]A", "G[G>C]C", "G[G>C]G", "G[G>C]T",
    "T[G>C]A", "T[G>C]C", "T[G>C]G", "T[G>C]T",

    "A[G>A]A", "A[G>A]C", "A[G>A]G", "A[G>A]T",
    "C[G>A]A", "C[G>A]C", "C[G>A]G", "C[G>A]T",
    "G[G>A]A", "G[G>A]C", "G[G>A]G", "G[G>A]T",
    "T[G>A]A", "T[G>A]C", "T[G>A]G", "T[G>A]T",

    "A[A>T]A", "A[A>T]C", "A[A>T]G", "A[A>T]T",
    "C[A>T]A", "C[A>T]C", "C[A>T]G", "C[A>T]T",
    "G[A>T]A", "G[A>T]C", "G[A>T]G", "G[A>T]T",
    "T[A>T]A", "T[A>T]C", "T[A>T]G", "T[A>T]T",

    "A[A>G]A", "A[A>G]C", "A[A>G]G", "A[A>G]T",
    "C[A>G]A", "C[A>G]C", "C[A>G]G", "C[A>G]T",
    "G[A>G]A", "G[A>G]C", "G[A>G]G", "G[A>G]T",
    "T[A>G]A", "T[A>G]C", "T[A>G]G", "T[A>G]T",

    "A[A>C]A", "A[A>C]C", "A[A>C]G", "A[A>C]T",
    "C[A>C]A", "C[A>C]C", "C[A>C]G", "C[A>C]T",
    "G[A>C]A", "G[A>C]C", "G[A>C]G", "G[A>C]T",
    "T[A>C]A", "T[A>C]C", "T[A>C]G", "T[A>C]T"
  )
}

levels_rnasnv_type <- function(){
  c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G", "G>T", "G>C", "G>A",
    "A>T", "A>G", "A>C")
}


# Autosort Levels -------------------------------------------------------
# Takes a set of channel / types and if theres an exact match in channel_sets, sort in the sigvis defined order.
# Otherwise return the unique set that was inserted excapt in alphabetical order
auto_level <- function(set, type = c('channel', 'type')) {
  type = rlang::arg_match(type)
  assertions::assert_character(set)
  unique_set <- sort(unique(set))

  channel_sets <- list(
    sbs_type = levels_snv_type(),
    sbs_96 = levels_snv(),
    indel_type = levels_indel_type(),
    id_83 = levels_indel(),
    doublet_type = levels_doublet_type(),
    dbs_78 = levels_dbs(),
    cn_48 = levels_cn(),
    cn_type = levels_cn_type(),
    cn2_type = levels_cn2_type(),
    sv_32 = levels_sv(),
    sv_type = levels_sv_type(),
    rnasnv = levels_rnasnv(),
    rnasnv_type = levels_rnasnv_type()
  )

  # Check if unique set match any known channel/type set
  matching_set <- NULL
  for (set_name in names(channel_sets)) {
    matches_set <- setequal(unique_set, channel_sets[[set_name]])

    if (matches_set) {
      cli::cli_alert_success('All {type}s matched perfectly to set [{set_name}]. Using this set for sort order')
      return(channel_sets[[set_name]])
    }
  }

  if (!is.null(matching_set)) {
    return(matching_set)
  } else {
    cli::cli_alert_warning("No exact {type} set match found, sorting in order of appearance")
    return(unique_set)
  }
}


type2colour <- function(type, palette, na.value = "grey"){
  colours <- palette[match(type, names(palette))]
  colours[is.na(colours)] <- "grey"

  return(colours)

}


# Return duplicated elements
# get_indistinguishable_palettes(list(1:5, "Billy" = c("a"= 3,"b"= 4), "bob" = c("a" = 5, "b" = 3)))
get_indistinguishable_palettes <- function(ls){
  list_of_names <- lapply(ls, \(x) {names(x)})
  duplicates = duplicated(list_of_names) | duplicated(list_of_names, fromLast=TRUE)
  names(ls[duplicates])
}

# Suppress messages
sm <- suppressMessages
