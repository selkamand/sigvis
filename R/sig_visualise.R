
#' Visualise Signatures
#'
#' Visualises Signatures.
#' The order channels are displayed are based on the order they appear in the signature data.frame
#'
#' If the channels match a known channel type, channel display order will instead be based on a predefined order
#'
#' @param signature a sigverse style signature data.frame. See [sigshared::example_signature()].
#' @param class type of input signature. Is it a signature (e.g. from sigstash database), a catalogue (e.g. from TCGAcatalogues or a signature analysis), or a model (combination of signatures designed to approximate an observed mutational profile)
#' @param title plot title
#' @param subtitle plot subtitle
#' @param palette colours based on the 'type' column. By default 'auto' will automatically pick a palette if the values of the 'type' column matches COSMIC SBS, Doublet or Indel mutations. Otherwise should be a named vector where names = types and values are colours.
#' @param channel_order How channels should be ordered on the X axis. By default 'auto' will automatically pick palette if channels are recognised as a standard COSMIC SBS/DBS/INDEL signature, or if not sorts in alphabetical order. Can also be a vector of channels in the order they should appear
#' @param na.value colour to use when type = NA
#' @param options other visualisation options. See [vis_options()] for details
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col theme scale_y_continuous scale_x_continuous ylab xlab expansion
#'
#' @examples
#' library(sigstash)
#' library(sigstats)
#' library(TCGAcatalogues)
#'
#' # Load Signature
#' signatures <- sig_load('COSMIC_v3.3.1_SBS_GRCh38')
#'
#' # Visualise a single signature
#' sig_visualise(signatures[["SBS2"]])
#'
#' # Visualise a catalogue
#' brca_catalogues <- catalogues_load('BRCA')
#' sig_visualise(brca_catalogues[["TCGA-3C-AALI-01A-11D-A41F-09"]], class = 'catalogue')
#'
#' # Visualise a model (combination of signatures
#' model <- sig_combine(signatures, model = c('SBS2' = 0.5, 'SBS13' = 0.5), format = "combined")
#' sig_visualise(model, class = 'model')
#'
#' # Make Visualisations Interactive
#' gg = sig_visualise(model, class = 'model')
#' sig_make_interactive(gg)
sig_visualise <- function(signature, class = c('signature', 'catalogue', 'model'), title = NULL, subtitle = NULL, palette = "auto", channel_order = "auto", na.value = "grey", options = vis_options()){

  class <- rlang::arg_match(class)
  if (class == "signature") {
    sigshared::assert_signature(signature, must_sum_to_one = FALSE)

    col_y = "fraction"
    col_data_id = "channel"
    col_tooltip = "channel"

    label_y = "Fraction"
    labels_y = scales::label_percent()
  }
  else if (class == "catalogue") {
    sigshared::assert_catalogue(signature)
    col_y = "count"
    col_data_id = "channel"
    col_tooltip = "channel"

    label_y = "Count"
    labels_y = scales::label_number(accuracy = 1, big.mark = ',')
  }
  else if (class == "model") {
    #sigshared::assert_model(signature) Need to create assert_model
    col_group = "signature"
    col_y = "fraction"
    col_data_id = "signature"
    col_tooltip = "signature"

    label_y = "Fraction"
    labels_y = scales::label_percent()
  }

  # Determine channel and type order
  if (length(channel_order) == 1 && channel_order == "auto") {
   channel_levels = auto_level(signature[['channel']], type = "channel")
   type_levels = auto_level(signature[['type']], type = "type")
  }
  else{
    channel_levels = channel_order
  }

  # Convert to factors and reorder
  signature[['type']] <- forcats::as_factor(signature[['type']])
  signature[['channel']] <- forcats::as_factor(signature[['channel']])
  signature[['type']] <- forcats::fct_relevel(signature[['type']], type_levels)
  signature[['channel']] <- forcats::fct_relevel(signature[['channel']], channel_levels)


  # Create the main plot
  gg <- ggplot(
    signature,
    aes(
      x = .data[["channel"]],
      y = .data[[col_y]],
      fill = .data[["type"]],
      data_id = .data[[col_data_id]],
      tooltip = .data[[col_tooltip]]
      )
    ) +
    ggiraph::geom_col_interactive(
      width = 0.7,
      position = "stack"
      ) +
    scale_y_continuous(labels = labels_y, expand = expansion(mult = c(0,0.05))) +
    ylab(label_y) +
    xlab(NULL) +
    # ggplot2::facet_grid(~.data[["type"]], scales = "free_x") +
    theme_sigverse(
      fontsize_x = options$fontsize_x,
      fontsize_y = options$fontsize_y,
      fontsize_title = options$fontsize_title,
      hjust_title = options$hjust_title
    )

  # Determine colors
  if (length(palette) == 1 && palette == "auto") {
    pal <- auto_palette(unique(as.character(signature[['type']])), default = pal_set2())
  } else {
    assertions::assert_vector(palette)
    assertions::assert_names_include(palette, signature[['type']])
    pal <- palette
  }
  gg <- gg + ggplot2::scale_fill_manual(values = pal, na.value = na.value, name = "")

  # Add title and subtitle if provided
  if (!is.null(title) | !is.null(subtitle)) {
    gg <- gg + ggplot2::ggtitle(title, subtitle)
  }

  # Return the plot
  return(gg)
}



#' Compare two signature catalogues by overlaying one over the other
#'
#' @param catalogue1,catalogue2 a sigverse catalogue (tally) representing an observed mutational profile (data.frame)
#' @param compare what metric to compare (counts or fractions)
#' @param names a 2 element character vector describing the names that should be used to describe catalogue 1 and 2 retrospectively.
#' @inheritParams sig_visualise
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' library(sigstats) # For combining signature models
#' library(TCGAcatalogues) # For pulling example TCGA catalogue data
#'
#' # Load a catalogue collection (Tally of variant types)
#' catalogues <- catalogues_load("BRCA", type = "SBS_96")
#'
#' # Get catalogue of 2 different samples
#' sample1 = "TCGA-OL-A6VQ-01A-12D-A41F-09"
#' sample2 = "TCGA-OL-A97C-01A-32D-A41F-09"
#' catalogue_sample1 <- catalogues[[sample1]]
#' catalogue_sample2 <- catalogues[[sample2]]
#'
#'
#' # Compare the two catalogues
#' ggcompare <- sig_visualise_compare(
#'   catalogue1 = catalogue_sample1,
#'   catalogue2 = catalogue_sample2
#' )
#'
#' # Make interactive
#' sig_make_interactive(ggcompare)
#'
sig_visualise_compare <- function(catalogue1, catalogue2, compare = c("count", "fraction"), names = c("Sample 1", "Sample 2"), channel_order = "auto", palette = "auto", na.value = "grey", subtitle = NULL, title = NULL, options = vis_options()){

# Assertions
sigshared::assert_catalogue(catalogue1)
sigshared::assert_catalogue(catalogue2)
compare <- rlang::arg_match(compare)
assertions::assert_character_vector(names)
assertions::assert_equal(length(names), 2, msg = "{.arg names} must contain 2 values describing the names of catalogue1 and catalogue2 respectively. Found [{length(names)}] values.")

assertions::assert(
  setequal(catalogue1[["channel"]], catalogue2[["channel"]]),
  msg = "Channels in the catalogue data.frames must contain the same elements"
)

assertions::assert(
  setequal(catalogue1[["type"]], catalogue2[["type"]]),
  msg = "Type of mutations in the catalogue data.frames contain the same elements"
)

# Determine channel and type order
if (length(channel_order) == 1 && channel_order == "auto") {
  channel_levels = auto_level(catalogue1[['channel']], type = "channel")
  type_levels = auto_level(catalogue1[['type']], type = "type")
}
else{
  channel_levels = channel_order
}


# Convert to factors and reorder
catalogue1[['type']] <- forcats::as_factor(catalogue1[['type']])
catalogue1[['channel']] <- forcats::as_factor(catalogue1[['channel']])
catalogue1[['type']] <- forcats::fct_relevel(catalogue1[['type']], type_levels)
catalogue1[['channel']] <- forcats::fct_relevel(catalogue1[['channel']], channel_levels)

# Convert to factors and reorder
catalogue2[['type']] <- forcats::as_factor(catalogue2[['type']])
catalogue2[['channel']] <- forcats::as_factor(catalogue2[['channel']])
catalogue2[['type']] <- forcats::fct_relevel(catalogue2[['type']], type_levels)
catalogue2[['channel']] <- forcats::fct_relevel(catalogue2[['channel']], channel_levels)

# Add Data-IDs for merge
catalogue1[["data_id"]] <- paste(catalogue1[["type"]], catalogue1[["channel"]])
catalogue2[["data_id"]] <- paste(catalogue2[["type"]], catalogue2[["channel"]])

# Remove everything but data_id and values
catalogue2 <- catalogue2[,c("data_id","count", "fraction")]

# Left Join to make 1 wide table
catalogue <- merge(catalogue1, catalogue2, by = "data_id", all.x = TRUE, suffixes = c(".c1", ".c2"))


# Determine colors
if (length(palette) == 1 && palette == "auto") {
  pal <- auto_palette(unique(as.character(catalogue1[['type']])), default = pal_set2())
} else {
  assertions::assert_vector(palette)
  assertions::assert_names_include(palette, catalogue1[['type']])
  pal <- palette
}

# Add tooltip
catalogue[["tooltip"]] <- make_tooltip(
  channels = catalogue[["channel"]],
  type = catalogue[["type"]],
  mutations1 = catalogue[["count.c1"]],
  mutations2 = catalogue[["count.c2"]],
  name1 = names[1],
  name2 =  names[2],
  pal = pal
)


# Setup properties of plot
if(compare == "count"){
  labels_y = scales::label_number(accuracy = 1, big.mark = ',')
  y_name = "Count"
}
else if(compare == "fraction"){
  labels_y = scales::label_percent(accuracy = 1)
  y_name = "Fraction"
}

# Create the main plot
gg <- ggplot() +
  ggiraph::geom_col_interactive(
    data = catalogue,
    mapping = aes(
      x = .data[["channel"]],
      y = .data[[paste0(compare, ".c2")]],
      data_id = .data[["data_id"]],
      tooltip = .data[["tooltip"]],
      onclick = paste0('navigator.clipboard.writeText("',.data[["channel"]],'")')
    ),
    hover_nearest = TRUE,
    position = "identity",
    fill = "white",
    color = "black",
    width = 1
  ) +
  ggplot2::scale_y_continuous(
    name = y_name,
    labels = labels_y,
    expand = expansion(mult = c(0,0.05))
  ) +
  ggiraph::geom_col_interactive(
    data = catalogue,
    mapping = aes(
      x = .data[["channel"]],
      y = .data[[paste0(compare, ".c1")]],
      fill = .data[["type"]],
      data_id = .data[["data_id"]],
      tooltip = .data[["tooltip"]],
      onclick = paste0('navigator.clipboard.writeText("',.data[["channel"]],'")')
    ),
    hover_nearest = TRUE,
    position = "identity",
    linewidth = 0,
    width = 0.6,
    alpha = 1
  ) +
  xlab(NULL) +
  theme_sigverse(
    fontsize_x = options$fontsize_x,
    fontsize_y = options$fontsize_y,
    fontsize_title = options$fontsize_title,
    hjust_title = options$hjust_title,
    fontsize_axis_title_y = options$fontsize_axis_title_y
  )


gg <- gg + ggplot2::scale_fill_manual(values = pal, na.value = na.value, name = "")

# Add title and subtitle if provided
if (!is.null(title) | !is.null(subtitle)) {
  gg <- gg + ggplot2::ggtitle(title, subtitle)
}

# Return the plot
return(gg)
}

#' Make a tooltip
#'
#' @param channels vector of channel names
#' @param type vector of channel types
#' @param mutations1 mutation counts for catalog 1 (numeric vector)
#' @param mutations2 mutation counts for catalog 2 (numeric vector)
#' @param name1 name of sample from which catalog 1 was derived (string)
#' @param name2 name of sample from which catalog 2 was derived (string)
#' @param pal named vector where names are mutation types and values are colours
#'
#' @return a character vector encoding the tooltip to display
#'
make_tooltip <- function(channels, type, mutations1, mutations2, name1, name2, pal){
  # Add full tooltip to catalogue 2 with something more informative
  colours <- type2colour(type, pal)
  paste0(
    span(channels, color = "white", color_bg = colours, font_weight = "bold"),
    "<hr>",
    span(name1, color = colours, font_weight = "bold"), ": ",
    round(mutations1, digits = 1), " mutations",
    "<br>",
    span(name2, color = "black", font_weight = "bold"), ": ",
    round(mutations2, digits = 1), " mutations",
    "<hr>",
    span("Difference", color = "black", font_weight = "bold"), ": ",
    round(mutations1-mutations2, digits = 2), " mutations"
  )
}

#' Visualise a signature overlayed on observed mutational profile
#'
#' @param signature a sigverse signature object representing the exposure model (data.frame)
#' @param catalogue a sigverse catalogue (tally) representing an observed mutational profile (data.frame)
#' @param foreground choose whether the signature or the catalogue are rendered in the foreground as coloured bars instead of in the background as a black outline  (one of \strong{reconstructed} or \strong{observed})
#' @inheritParams sig_visualise_compare
#' @inheritDotParams sig_visualise_compare
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' library(sigstats) # For combining signature models
#' library(sigstash) # For pulling signatures
#' library(TCGAcatalogues) # For pulling example TCGA catalogue data
#'
#' # Load Signature
#' signatures <- sig_load("COSMIC_v3.3.1_SBS_GRCh38")
#'
#' # Create a model (combination of signatures)
#' model_signature <- sig_combine(
#'   signatures,
#'   model = c('SBS2' = 0.6, 'SBS13' = 0.4),
#'   format = "signature"
#' )
#'
#' # Load a catalogue (Tally of variant types)
#' tally <- catalogues_load("BRCA", type = "SBS_96")
#'
#' # Get tally of a single sample
#' sample = "TCGA-5L-AAT1-01A-12D-A41F-09"
#' tally_single_sample <- tally[[sample]]
#'
#' # Visualise the overlay
#' gg <- sig_visualise_compare_reconstructed_to_observed(
#'   catalogue = tally_single_sample,
#'   signature = model_signature
#' )
#'
#' # Make interactive
#' sig_make_interactive(gg)
sig_visualise_compare_reconstructed_to_observed <- function(signature, catalogue, ..., foreground = c("reconstructed", "observed")){

  # Assertions
  sigshared::assert_catalogue(catalogue)
  sigshared::assert_signature(signature, must_sum_to_one = FALSE)
  assertions::assert(
    setequal(catalogue[["channel"]], signature[["channel"]]),
    msg = "Channels in the catalogue and signature data.frames must contain the same elements"
  )
  assertions::assert(
    setequal(catalogue[["type"]], signature[["type"]]),
    msg = "Type of mutations in the catalogue and signature data.frames contain the same elements"
  )
  foreground <- rlang::arg_match(foreground)

  # # Determine channel and type order
  # if (length(channel_order) == 1 && channel_order == "auto") {
  #   channel_levels = auto_level(signature[['channel']], type = "channel")
  #   type_levels = auto_level(signature[['type']], type = "type")
  # }
  # else{
  #   channel_levels = channel_order
  # }

  # Turn Signature Into a Catalogue
  total_mutations <- sum(catalogue[["count"]])
  catalogue_reconstructed <- sigstats::sig_reconstruct(signature, total_mutations)

  # Choose which catalogue should be in the foreground
  foreground_catalogue <- if(foreground == "observed") catalogue else catalogue_reconstructed
  background_catalogue <- if(foreground == "observed") catalogue_reconstructed else catalogue
  names <- if(foreground == "observed") c("Observed", "Reconstructed") else c("Reconstructed", "Observed")

  # Compare reconstructed vs observed
  gg <- sig_visualise_compare(catalogue1 = foreground_catalogue, catalogue2 = background_catalogue, names = names, ...)

  # Return the plot
  return(gg)

}

#' Sigverse Theme for Signature Plots
#'
#'
#' @importFrom ggplot2 %+replace%
#' @inheritDotParams ggplot2::theme_bw
#' @inheritParams vis_options
#' @return ggplot2 theme
#' @export
#'
theme_sigverse <- function(fontsize_x = NULL, fontsize_y = NULL, fontsize_title = NULL, hjust_title = 0.5, fontsize_axis_title_y = NULL, ...){
  ggplot2::theme_bw(...) %+replace%
    theme(
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.title.y = ggplot2::element_text(size = fontsize_axis_title_y, face = "bold", angle = 90),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_text(size = fontsize_x),
      axis.text.y.left = ggplot2::element_text(size = fontsize_y),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "black"),
      strip.text = ggplot2::element_text(colour = "white", face = "bold"),
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", hjust = hjust_title, size = fontsize_title),
      plot.subtitle = ggplot2::element_text(hjust = hjust_title, vjust = 0, margin = ggplot2::margin(5, 2, 0, 2))
      )
}

#' Set Visualization Options for Signature Plots
#'
#' This function provides a way to set various options for visualizing signature plots.
#'
#' @param fontsize_x fontsize of x axis text (number)
#' @param fontsize_y fontsize of y axis text (number)
#' @param hjust_title title horizontal justification (number)
#' @param fontsize_title fontsize of title (number)
#' @param fontsize_axis_title_y fontsize of the y axis title (number)
#' @return A list of visualization options.
#'
#' @export
#'
vis_options <- function(fontsize_x = 6, fontsize_y = 9, hjust_title = 0.5, fontsize_title = 16, fontsize_axis_title_y = 14){
  opts = list(
    fontsize_x = fontsize_x,
    fontsize_y = fontsize_y,
    hjust_title = hjust_title,
    fontsize_title = fontsize_title,
    fontsize_axis_title_y = fontsize_axis_title_y
  )

  return(opts)
}



#' Make sigverse overlay visualisation interactive
#'
#' @param gg the plot returned from any sigverse visualisation
#' @inheritParams ggiraph::girafe
#' @inheritParams ggiraph::opts_sizing
#' @return a ggiraph interactive visualisation
#' @export
#'
sig_make_interactive <- function(gg, width_svg = 12, height_svg = NULL, rescale = FALSE, width = 1){
  ggi <- ggiraph::girafe(ggobj = gg, width_svg = width_svg, height_svg = height_svg)
  ggi <- ggiraph::girafe_options(
    ggi,
      ggiraph::opts_hover(
        css = "stroke-width: 2",
        nearest_distance = 50
      ),
    ggiraph::opts_hover_inv(
      css = "fill-opacity:0.5; stroke-opacity: 0.5"
    ),
    ggiraph::opts_tooltip(
      css = ggiraph::girafe_css(
        css = "background-color:white;color:black;padding:5px;border-radius:3px;border:2px solid black",line = "color:black"
        ),
      opacity = 1
      ),
    ggiraph::opts_sizing(
      rescale = rescale, width = width
      )
    )
  return(ggi)
}
