#' Create a Proportion Bar Plot
#'
#' Creates a horizontal bar plot representing a single proportion, with optional formatting and colors.
#'
#' @param proportion Numeric value between 0 and 1 representing the proportion to be plotted.
#' @param format Function to format the proportion label. Defaults to [fmt_percent()]. Most common alternative is to use [fmt_round()].
#' @param fgcol Color for the foreground (proportion) part of the bar. Defaults to `"maroon"`.
#' @param bgcol Color for the background (remaining) part of the bar. Defaults to `"grey90"`.
#' @param size Numeric value for the text size of the proportion label. Defaults to `NA`.
#'
#' @return A ggplot object representing the proportion bar plot.
#' @export
#'
#' @examples
#' proportion_bar(0.75)
#' proportion_bar(0.5, fgcol = "blue", bgcol = "lightblue")
proportion_bar <- function(proportion, format = fmt_percent(), fgcol = "maroon", bgcol = "grey90", size = NA) {


  # Assertions
  assertions::assert_function(format)

  dd <- data.frame(
    contribution = c(proportion, 1 - proportion),
    measure = c("contribution", "all")
  )

  ggplot2::ggplot(dd, ggplot2::aes(x = .data[["contribution"]], y = "")) +
    ggplot2::geom_col(fill = c(fgcol, bgcol)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::annotate(
      geom = "text",
      x = 1,
      y = "",
      label = format(proportion),
      hjust = 1.3,
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
#' @param heights Numeric vector specifying the relative heights of the signature plot and proportion bar. Defaults to `c(0.9, 0.1)`.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A combined ggplot object of the signature plot and proportion bar.
#' @export
#'
#' @examples
#' sig <- sigshared::example_signature()
#' sig_visualise_minified(sig, proportion = 0.75)
#'
sig_visualise_minified <- function(signature, proportion, format = fmt_percent(), fgcol = "maroon", bgcol = "grey90", text_size_prop_label = NA, heights = c(0.9, 0.1), ...) {

  # Create Signature
  gg_sig <- sig_visualise(signature = signature)
  gg_sig <- gg_sig + theme_minisig()

  # Create Proportion Bar
  gg_proportion <- proportion_bar(proportion, format = format, fgcol = fgcol, bgcol = bgcol, size = text_size_prop_label)

  # Align vertically
  gg_mini <- patchwork::wrap_plots(gg_sig, gg_proportion, ncol = 1, heights = heights)

  return(gg_mini)
}
