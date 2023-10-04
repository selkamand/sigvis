
# Colour Palettes ---------------------------------------------------------
sig_palette_snv_type = function(){
  c(
    "C>A" = "#16AFE9",
    "C>G" = "black",
    "C>T" = "#D9111E",
    "T>A" = "#C1BDBE",
    "T>C"= "#92C446",
    "T>G" = "#E6B8B9"
  )
}

pal_set2 <- function(){
  c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
    "#E5C494", "#B3B3B3")
}

#' Automatically select a color palette based on signature types
#'
#' This function takes a vector of signature 'types' and selects an appropriate color palette.
#' It checks if the types match predefined palettes and
#' returns the matching palette or a default palette [pal_set2()] if no exact match is found.
#'
#' @param types A character vector of types e.g. from a sigverse signature 'type' column.
#' @param default The default color palette to use if no exact match is found.
#'
#' @return A color palette (named vector where names are types and values are colours).
#'
#' @importFrom stats na.omit
#'
#' @examples
#' # Example usage:
#' auto_palette(c("T>G", "T>G"))
#'
auto_palette <- function(types, default = pal_set2()){
  assertions::assert_character(types)
  types <- na.omit(types)

  ls_pals <- list(
    snv_type = sig_palette_snv_type()
    )

  n_matches = vapply(ls_pals, \(pal){ sum(types %in% names(pal)) }, FUN.VALUE = numeric(1))
  names(n_matches) <- names(n_matches)
  all_matched <- n_matches == length(types)
  palettes_matched <- names(Filter(f = isTRUE, all_matched))

  if(length(palettes_matched) == 1){
    cli::cli_alert_success('Types matched perfectly to palette [{palettes_matched}]')
    return(ls_pals[[palettes_matched]])
  }
  else if(length(palettes_matched) == 0 & any(n_matches >= 2)){
    index_best_match <- which.max(n_matches)
    best_match_n = n_matches[index_best_match]
    best_match <- names(n_matches)[index_best_match]
    number_types = length(types)

    not_matched <- ls_pals[[best_match]]
    cli::cli_alert_warning(
    'No exact palette match, returning default
    The [{best_match}] palette was the closest match, with {best_match_n} / {number_types} type{?s} matching.'
    )
    return(default)
  }
  else{
    cli::cli_alert_warning('No exact palette matches, returning default')
    return(default)
  }
}


# Channel/Type Levels For Ordering ----------------------------------------------------------
levels_snv_type = function(){
  c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
}



levels_snv = function(){
  # Create a character vector with the mutation signature analysis decomposition channels
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

auto_level <- function(set, type = c('channel', 'type')) {
  type = rlang::arg_match(type)
  assertions::assert_character(set)
  unique_set <- unique(set)

  channel_sets <- list(
    sbs_type = levels_snv_type(),
    sbs_96 = levels_snv()
  )

  # Check if unique set match any known channel/type set
  matching_set <- NULL
  for (set_name in names(channel_sets)) {
    lgl_matching_set <- unique_set %in% channel_sets[[set_name]]

    if (all(lgl_matching_set)) {
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

