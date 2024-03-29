
#' Visualise Signatures
#'
#' Visualises Signatures.
#' The order channels are displayed are based on the order they appear in the signature data.frame
#'
#' If the channels match a known channel type, channel display order will instead be based on a predefined order
#'
#' @param signature a sigverse signature object
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
#' model <- sig_combine(signatures, model = c('SBS2' = 0.5, 'SBS13' = 0.5))
#' sig_visualise(model, class = 'model')
#'
#' # Make Visualisations Interactive
#' gg = sig_visualise(model, class = 'model')
#' sig_make_interactive(gg)
sig_visualise <- function(signature, class = c('signature', 'catalogue', 'model'), title = NULL, subtitle = NULL, palette = "auto", channel_order = "auto", na.value = "grey", options = vis_options()){

  class <- rlang::arg_match(class)
  if (class == "signature") {
    sigshared::assert_signature(signature)

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


#' Sigverse Theme for Signature Plots
#'
#'
#' @importFrom ggplot2 %+replace%
#' @inheritDotParams ggplot2::theme_bw
#' @inheritParams vis_options
#' @return ggplot2 theme
#' @export
#'
theme_sigverse <- function(fontsize_x = NULL, fontsize_y = NULL, fontsize_title = NULL, hjust_title = 0.5, ...){
  ggplot2::theme_bw(...) %+replace%
    theme(
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.title.y = ggplot2::element_text(face = "bold", angle = 90),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_text(size = fontsize_x),
      axis.text.y.left = ggplot2::element_text(size = fontsize_y),
      axis.ticks.x = ggplot2::element_blank(),
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
#'
#' @return A list of visualization options.
#'
#' @export
#'
vis_options <- function(fontsize_x = 6, fontsize_y = 9, hjust_title = 0.5, fontsize_title = 16){
  opts = list(
    fontsize_x = fontsize_x,
    fontsize_y = fontsize_y,
    hjust_title = hjust_title,
    fontsize_title = fontsize_title
  )

  return(opts)
}

#' Make any sigverse visualisation interactive
#'
#' @param gg the plot returned from any sigverse visualisation
#'
#' @return a ggiraph interactive visualisation
#' @export
#'
sig_make_interactive <- function(gg){
  ggiraph::girafe(ggobj = gg, width_svg = 12)
}

