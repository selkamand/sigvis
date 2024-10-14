#' Create a Proportion Bar Plot
#'
#' Creates a horizontal bar plot representing a single proportion, with optional formatting and colors.
#'
#' @param proportion Numeric value between 0 and 1 representing the proportion to be plotted.
#' @param format Function to format the proportion label. Defaults to [fmt_percent()].
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
proportion_bar <- function(proportion, format = fmt_percent, fgcol = "maroon", bgcol = "grey90", size = NA) {
  dd <- data.frame(
    contribution = c(proportion, 1 - proportion),
    measure = c("contribution", "all")
  )

  ggplot2::ggplot(dd, ggplot2::aes(x = contribution, y = "")) +
    ggplot2::geom_col(fill = c(fgcol, bgcol)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::annotate(
      geom = "text",
      x = 1,
      y = "",
      label = fmt_percent(proportion, 0),
      hjust = 1.3,
      size = size
    ) +
    ggplot2::theme_void()
}

#' Format Proportion as Percentage String
#'
#' Formats a numeric proportion as a percentage string with optional number of digits and spacing.
#'
#' @param proportion Numeric value representing the proportion to format (between 0 and 1).
#' @param digits Integer indicating the number of decimal places. Defaults to `0`.
#' @param space Logical indicating whether to include a space before the percent sign. Defaults to `TRUE`.
#'
#' @return A character string representing the formatted percentage.
#' @export
#'
#' @examples
#' fmt_percent(0.75)
#' fmt_percent(0.12345, digits = 2)
#' fmt_percent(0.5, space = FALSE)
fmt_percent <- function(proportion, digits = 0, space = TRUE) {
  s <- if (space) " " else ""
  paste0(round(proportion * 100, digits = digits), s, "%")
}

#' Format Value as Rounded String
#'
#' Formats a numeric value as a rounded character string with specified number of digits.
#'
#' @param value Numeric value to format.
#' @param digits Integer indicating the number of decimal places. Defaults to `0`.
#'
#' @return A character string representing the rounded value.
#' @export
#'
#' @examples
#' fmt_round(3.14159)
#' fmt_round(2.71828, digits = 2)
fmt_round <- function(value, digits = 0) {
  as.character(round(value, digits = digits))
}

#' Create a Minified Signature Plot with Proportion Bar
#'
#' Generates a compact visualization of a signature plot with a proportion bar underneath.
#'
#' @param signature A `sigverse` style signature data.frame.
#' @param proportion Numeric value between 0 and 1 representing the proportion to display.
#' @param format Function to format the proportion label. Defaults to [fmt_percent()].
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
sig_visualise_minified <- function(signature, proportion, format = fmt_percent, fgcol = "maroon", bgcol = "grey90", text_size_prop_label = NA, heights = c(0.9, 0.1), ...) {
  # Create Signature
  gg_sig <- sig_visualise(signature = signature)
  gg_sig <- gg_sig + theme_minisig()

  # Create Proportion Bar
  gg_proportion <- proportion_bar(proportion, format = format, fgcol = fgcol, bgcol = bgcol, size = text_size_prop_label)

  # Align vertically
  gg_mini <- patchwork::wrap_plots(gg_sig, gg_proportion, ncol = 1, heights = heights)

  return(gg_mini)
}
