#' Visualise Signature Contributions with a Dot Plot
#'
#' This function creates a dot plot to visualise signature contributions in genomic data.
#'
#' @param data A data frame describing the contributions of one signature across an entire cohort (or set of bootstrapping experiments).
#' @param col_sample The name of the column in `data` that contains the sample identifiers. Default is `"sample"`.
#' @param col_contribution The name of the column in `data` that contains the contributions (exposures) to plot (unit: proportions of total mutations explained by the signature). Default is `"contribution"`.
#' @param binwidth The width of the bins used in the dot plot. Default is `0.02`.
#' @param col_fill Optional. The name of the column in `data` that defines the fill colour grouping.
#' @param col_colour Optional. The name of the column in `data` that defines the colour grouping.
#' @param binpositions Positioning method for the bins. Either `"bygroup"` or `"all"`. Default is `"bygroup"`.
#' @param palette_fill Optional. A vector of colours for the fill aesthetic.
#' @param palette_colour Optional. A vector of colours for the colour aesthetic.
#' @param sort_by When `col_fill` or `col_colour` are supplied, in which order should the different groups be drawn.
#' Choices are:
#' \describe{
#'   \item{\strong{"default"}}{Uses the existing levels of `col_fill`/`col_colour`.}
#'   \item{\strong{"frequency_fill"}}{Renders groups based on how many samples share each fill colour (smallest group on top).}
#'   \item{\strong{"frequency_colour"}}{Renders groups based on how many samples share each colour (smallest group on top).}
#'   \item{\strong{"palette_fill"}}{Renders groups based on the order their fill colour was supplied to `palette_fill`.}
#'   \item{\strong{"palette_colour"}}{Renders groups based on the order their colour was supplied to `palette_colour`.}
#' }
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis. Default is `stringy::prettytext(col_contribution)`.
#' @param show_legend Logical. If `TRUE`, shows the legend. Default is `TRUE`.
#' @param legend_title_fill Title for the fill legend. Default is `stringy::prettytext(col_fill)`.
#' @param legend_title_colour Title for the colour legend. Default is `stringy::prettytext(col_colour)`.
#' @param ... Additional arguments passed to `ggplot2::geom_dotplot`.
#' @return A `ggplot` object representing the dot plot.
#' @export
#'
#' @inheritDotParams ggplot2::geom_dotplot
#'
#' @examples
#' data <- example_dotplot_data()
#' sig_visualise_dotplot(data)
#'
#' # Example with fill colours
#' sig_visualise_dotplot(
#'   data,
#'   col_fill = "sample_info",
#'   palette_fill = c("sample_of_interest" = "red", "cancer_type" = "blue", "other" = "grey")
#' )
sig_visualise_dotplot <- function(
    data, col_sample = "sample", col_contribution = "contribution",
    binwidth = 0.02, col_fill = NULL, col_colour = NULL, binpositions = c("bygroup", "all"),
    palette_fill = NULL, palette_colour = NULL,
    sort_by = c("default", "frequency_fill", "frequency_colour", "palette_fill", "palette_colour"),
    xlab = NULL,
    ylab = stringy::prettytext(col_contribution),
    show_legend = TRUE,
    legend_title_fill = stringy::prettytext(col_fill),
    legend_title_colour = stringy::prettytext(col_colour),
    ...
) {
  # Ensure that 'data' is a data frame and contains the required columns
  assertions::assert_dataframe(data)
  assertions::assert_names_include(data, c(col_sample, col_contribution, col_fill, col_colour))

  # Match the 'sort_by' argument
  sort_by <- rlang::arg_match(sort_by)

  # Match the 'binpositions' argument
  binpositions <- rlang::arg_match(binpositions)

  # Define default palette colours if none are provided
  default_palette <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"
  ) # From grDevices::palette.colors()

  # Sort data by fill colour so that smaller groups are rendered on top
  if (!is.null(col_fill)) {
    if (sort_by == "frequency_fill") {
      # Reorder levels by decreasing frequency
      data[[col_fill]] <- forcats::fct_infreq(data[[col_fill]])
    }
    else if (sort_by == "frequency_colour") {
      data[[col_fill]] <- forcats::fct_infreq(data[[col_fill]])
    }
    else if (sort_by == "palette_fill") {
      # Relevel factor according to 'palette_fill'
      assertions::assert(
        !is.null(palette_fill),
        msg = "Please supply the {.arg palette_fill} argument (required when {.arg sort_by} = 'palette_fill')"
      )
      data[[col_fill]] <- forcats::fct_relevel(data[[col_fill]], rev(names(palette_fill)))
    }
    else if (sort_by == "palette_colour") {
      assertions::assert(
        !is.null(palette_colour),
        msg = "Please supply the {.arg palette_colour} argument (required when {.arg sort_by} = 'palette_colour')"
      )
      data[[col_colour]] <- forcats::fct_relevel(data[[col_colour]], rev(names(palette_colour)))
    }
    else if (
      sort_by == "default" & binpositions == "bygroup" &
      (!is.null(col_fill) | !is.null(col_colour))
    ) {
      # Provide a warning if groups may be hidden behind larger groups
      recommended_sort <- if (!is.null(col_fill)) "frequency_fill" else "frequency_colour"
      recommended_sort2 <- if (!is.null(col_fill)) "palette_fill" else "palette_colour"
      cli::cli_alert_warning(
        "
        Creating separate, overlapping stacks for points with different fill/colour.
        To ensure your groups of interest are not hidden behind a larger group,
        please specify {.arg sort_by = '{recommended_sort}' or '{recommended_sort2}'}
        "
      )
    }
  }

  # Use default palettes if none are provided
  if (is.null(palette_fill)) palette_fill <- default_palette
  if (is.null(palette_colour)) palette_colour <- default_palette

  # Create the dot plot using ggplot2
  dot_plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = "", y = .data[[col_contribution]])) +
    ggplot2::geom_dotplot(
      ggplot2::aes(
        fill = if (!is.null(col_fill)) .data[[col_fill]] else NULL,
        colour = if (!is.null(col_colour)) .data[[col_colour]] else NULL
      ),
      stackdir = "center",
      binaxis = "y",
      binwidth = binwidth,
      binpositions = binpositions,
      stackgroups = binpositions == "all",
      ...
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(title.position = "top", title = legend_title_fill),
      colour = ggplot2::guide_legend(title.position = "top", title = legend_title_colour)
    ) +
    ggplot2::scale_fill_discrete(type = palette_fill) +
    ggplot2::scale_color_discrete(type = palette_colour) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  # Hide the legend if 'show_legend' is FALSE
  if (!show_legend) {
    dot_plot <- dot_plot + ggplot2::theme(legend.position = "none")
  }

  # Return the ggplot object
  return(dot_plot)
}

#' Generate Example Data for Dot Plot Visualization
#'
#' This function generates an example dataset to be used with \code{\link{sig_visualise_dotplot}}.
#' It returns a data frame containing sample identifiers, their contributions, and sample group information.
#'
#' @return A data frame with 50 rows and 3 columns:
#'   \describe{
#'     \item{\code{sample}}{Character vector of sample identifiers (e.g., "Sample1", "Sample2", etc.).}
#'     \item{\code{contribution}}{Numeric vector of contributions ranging from 0 to 1.}
#'     \item{\code{sample_info}}{Character vector indicating the group each sample belongs to ("sample_of_interest", "cancer_type", or "other").}
#'   }
#' @examples
#' example_dotplot_data()
#'
#' @export
example_dotplot_data <- function() {
  data.frame(
    sample = c(
      "Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6", "Sample7",
      "Sample8", "Sample9", "Sample10", "Sample11", "Sample12", "Sample13",
      "Sample14", "Sample15", "Sample16", "Sample17", "Sample18", "Sample19",
      "Sample20", "Sample21", "Sample22", "Sample23", "Sample24", "Sample25",
      "Sample26", "Sample27", "Sample28", "Sample29", "Sample30", "Sample31",
      "Sample32", "Sample33", "Sample34", "Sample35", "Sample36", "Sample37",
      "Sample38", "Sample39", "Sample40", "Sample41", "Sample42", "Sample43",
      "Sample44", "Sample45", "Sample46", "Sample47", "Sample48", "Sample49",
      "Sample50"
    ),
    contribution = c(
      0.0791, 0.0597, 0.0441, 0.0576, 0.0675, 0.0871, 0.0379, 0, 0.0501, 0.037,
      0.0499, 0.9531, 0.0662, 0.0437, 0.0557, 0.0294, 0.055, 0.0095, 0.0792, 0.112,
      0.0427, 0.9246, 0.0316, 0.0651, 0.049, 0.0327, 0.0746, 0.9372, 0.0225, 0.9731,
      0.9687, 0.0459, 0.0694, 0.0275, 0.0423, 0.0486, 0.9611, 0.9575, 1, 0.0414,
      0.9396, 0.9319, 0.0503, 0.069, 0.0184, 0.0519, 0.0514, 0.0934, 0.0651, 0.0743
    ),
    sample_info = c(
      "sample_of_interest", rep("cancer_type", times = 10),
      rep("other", times = 39)
    )
  )
}

