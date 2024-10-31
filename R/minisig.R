#' Create a Proportion Bar Plot
#'
#' Creates a horizontal bar plot representing a single proportion, with optional formatting and colors.
#'
#' @param proportion Numeric value between 0 and 1 representing the proportion to be plotted.
#' @param format Function to format the proportion label. Defaults to [fmt_percent()]. Most common alternative is to use [fmt_round()].
#' @param fgcol Color for the foreground (proportion) part of the bar. Defaults to `"maroon"`.
#' @param bgcol Color for the background (remaining) part of the bar. Defaults to `"grey90"`.
#' @param size Numeric value for the text size of the proportion label. Defaults to `NA`.
#' @param textcol Colour of proportion label when `proportion` is less than `proportion_inversion_point`
#' @param textcol_inverted Colour of proportion label when `proportion` greater than `proportion_inversion_point`
#' @param proportion_inversion_point The proportion above which the proportion will be rendered on top of progress bar, not
#'
#' @return A ggplot object representing the proportion bar plot.
#' @export
#'
#' @details
#' The for proportion values below proportion_inversion_point, the text label
#' will be placed on the far right side of the bar and coloured by `textcol`.
#' Otherwise, will be aligned on the inside of the bar and colour will be set to
#' `textcol_inverted`
#'
#'
#' @examples
#' proportion_bar(0.75)
#' proportion_bar(0.5, fgcol = "blue", bgcol = "lightblue")
#'
#'
proportion_bar <- function(proportion, format = fmt_percent(), fgcol = "maroon", bgcol = "grey90", textcol = "black", textcol_inverted = "white", proportion_inversion_point = 0.5,  size = NA) {


  # Assertions
  assertions::assert_function(format)
  assertions::assert_number(proportion)


  # Prep Data
  dd <- data.frame(
    contribution = c(proportion, 1 - proportion),
    measure = c("contribution", "all")
  )

  # Adjust label settings based on proportion_inversion_point
  hjust <- 1.2
  text_colour <- if(proportion < proportion_inversion_point) textcol else textcol_inverted
  x <- if(proportion < proportion_inversion_point) 1 else proportion

  # Create Plot
  ggplot2::ggplot(dd, ggplot2::aes(x = .data[["contribution"]], y = "")) +
    ggplot2::geom_col(fill = c(fgcol, bgcol)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::annotate(
      geom = "text",
      fontface = "bold",
      colour = text_colour,
      x = x,
      y = "",
      label = format(proportion),
      hjust = hjust,
      size = size
    ) +
    ggplot2::theme_void()
}




#' Create a Minified Signature Plot with Proportion Bar
#'
#' Generates a compact visualization of a signature plot with a proportion bar underneath.
#'
#' @param signature A `sigverse` style signature data.frame.
#' @param proportion Numeric value between 0 and 1 representing the proportion to display.
#' @inheritParams proportion_bar
#' @param fgcol Color for the foreground (proportion) part of the bar. Defaults to `"maroon"`.
#' @param bgcol Color for the background (remaining) part of the bar. Defaults to `"grey90"`.
#' @param text_size_prop_label Numeric value for the text size of the proportion label in the bar. Defaults to `NA`.
#' @param heights Numeric vector specifying the relative heights of the signature plot and proportion bar.
#' @inheritParams proportion_bar
#'
#' @return A combined ggplot object of the signature plot and proportion bar.
#' @export
#'
#' @examples
#' sig <- sigshared::example_signature()
#' sig_visualise_minified(sig, proportion = 0.75)
#'
#' # Change size ratio between signature plot & bar
#' sig_visualise_minified(sig, proportion = 0.75, heights = c(0.6, 0.4))
sig_visualise_minified <- function(signature, proportion, format = fmt_percent(), fgcol = "maroon", bgcol = "grey90", text_size_prop_label = NA, heights = c(0.85, 0.15),
                                   textcol = "black", textcol_inverted = "white", proportion_inversion_point = 0.5) {

  # Assertions
  assertions::assert_numeric_vector(heights)
  assertions::assert(length(heights) == 2, msg = "{.arg height} must be a vector of length 2, not [{length(heights)}]")
  assertions::assert(sum(heights) == 1, msg = "{.arg heights} must sum to 1, not [{sum(heights)}]")

  # Create Signature
  gg_sig <- sig_visualise(signature = signature)
  gg_sig <- gg_sig + theme_minisig()

  # Create Proportion Bar
  gg_proportion <- proportion_bar(
    proportion, format = format, fgcol = fgcol, bgcol = bgcol, size = text_size_prop_label,
    textcol = "black", textcol_inverted = "white", proportion_inversion_point = 0.5
    )

  # Align vertically
  gg_mini <- patchwork::wrap_plots(gg_sig, gg_proportion, ncol = 1, heights = heights)

  return(gg_mini)
}
