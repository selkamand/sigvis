
#' Visualise Signatures
#'
#' Visualises Signatures.
#' The order channels are displayed are based on the order they appear in the signature data.frame
#'
#' If the channels match a known channel type, channel display order will instead be based on a predefined order
#'
#' @param signature a sigverse signature object
#'
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col theme scale_y_continuous scale_x_continuous ylab xlab expansion
#'
#' @examples
#' library(sigstash)
#' library(sigstats)
#' library(TCGAdecomp)
#'
#' # Load Signature
#' signatures <- sig_load('COSMIC_v3.3.1_SBS_GRCh38')
#'
#' # Visualise a single signature
#' sig_visualise(signatures[["SBS2"]])
#'
#' # Visualise a decomposition
#' brca_decompositions <- decomp_load('BRCA')
#' sig_visualise(brca_decompositions[["TCGA-3C-AALI-01A-11D-A41F-09"]], class = 'decomposition')
#'
#' # Visualise a model (combination of signatures
#' model = sig_combine(signatures, model = c('SBS2' = 0.5, 'SBS13' = 0.5))
#' sig_visualise(model, class = 'model')
#'
#' # Make Visualisations Interactive
#' gg = sig_visualise(model, class = 'model')
#' sig_make_interactive(gg)
sig_visualise <- function(signature, class = c('signature', 'decomposition', 'model'), title = NULL, subtitle = NULL, palette = "auto", channel_order = "auto", na.value = "grey", options = vis_options()){

  class <- rlang::arg_match(class)
  if(class == "signature"){
    sigshared::assert_signature(signature)

    col_y = "fraction"
    col_data_id = "channel"
    col_tooltip = "channel"

    label_y = "Fraction"
    labels_y = scales::label_percent()
  }
  else if(class == "decomposition"){
    sigshared::assert_decomposition(signature)
    col_y = "count"
    col_data_id = "channel"
    col_tooltip = "channel"

    label_y = "Count"
    labels_y = scales::label_number(accuracy = 1, big.mark = ',')
  }
  else if(class == "model"){
    #sigshared::assert_model(signature) Need to create assert_model
    col_group = "signature"
    col_y = "fraction"
    col_data_id = "signature"
    col_tooltip = "signature"

    label_y = "Fraction"
    labels_y = scales::label_percent()
  }

  # Determine channel and type order
  if(channel_order == "auto"){
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
  if (palette == "auto") {
    pal <- auto_palette(unique(as.character(signature[['type']])), default = pal_set2())
  } else {
    assertions::assert_vector(palette)
    assertions::assert_names_include(palette, signature[['type']])
    pal <- palette
  }
  gg <- gg + ggplot2::scale_fill_manual(values = pal, na.value = na.value, name = "")

  # Add title and subtitle if provided
  if(!is.null(title) | !is.null(subtitle)){
    gg <- gg + ggplot2::ggtitle(title, subtitle)
  }

  # Return the plot
  return(gg)
}


#' Sigverse Theme for Signature Plots
#'
#'
#' @importFrom ggplot2 %+replace%
#'
#' @return ggplot2 theme
#' @export
#'
theme_sigverse <- function(fontsize_x = NULL, fontsize_y = NULL, fontsize_title = NULL, hjust_title = 0.5,...){
  ggplot2:::theme_bw(...) %+replace%
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
#' @return A list of visualization options.
#'
#' @export
#'
vis_options <- function(){
  opts = list(
    fontsize_x = 6,
    fontsize_y = 9,
    hjust_title = 0.5,
    fontsize_title = 16
  )
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

