#' Visualize Bootstrap Contributions for Signatures
#'
#' This function takes a dataframe of bootstrap contributions for signatures and visualizes them using boxplots.
#' It highlights significant contributions based on a p-value threshold and a minimum contribution threshold.
#'
#' @param bootstraps A dataframe in sigverse style describing bootstraps. See [sigshared::example_bootstraps()].
#' @param min_contribution_threshold Numeric value representing the minimum contribution threshold to consider (default is 0.05). See threshold argument of [sigstats::sig_compute_experimental_p_value()].
#' @param pvalue The p-value threshold for significance (default is 0.05). P values are computed using [sigstats::sig_compute_experimental_p_value()]
#' @param horizontal flip the coordinates so that signatures are on the X axis and contributions are on the Y axis.
#' @inheritParams boxplotstats::plot_boxplot_stats
#' @inheritDotParams boxplotstats::plot_boxplot_stats
#'
#' @return A ggplot object representing the boxplot visualization of bootstrap contributions.
#' @export
#'
#' @examples
#' library(sigshared)
#' sig_visualise_bootstraps(example_bootstraps())
#'
sig_visualise_bootstraps <- function(
    bootstraps, min_contribution_threshold = 0.05, pvalue = 0.05,
    horizontal = FALSE, width = 0.6, staplewidth = 0.8,
    ...
    ) {
  # Assert that the bootstraps dataframe is in the correct format
  sigshared::assert_bootstraps(bootstraps)

  # Summarize bootstrap statistics with minimum contribution threshold
  stats <- sigstats::sig_summarise_bootstraps(bootstraps, min_contribution_threshold)

  # Determine significance based on p-value threshold
  stats[["significant"]] <- stats[["p_value"]] < pvalue

  # Assign signature IDs for plotting
  stats[["id"]] <- stats[["signatures"]]

  # Create boxplot visualization using the calculated statistics
  plot <- boxplotstats::plot_boxplot_stats(
    stats,
    ...,
    ylab = "Signature",
    xlab = "Contribution",
    col_fill = "significant",
    col_colour = "significant",
    col_data_id = "signatures",
    show_legend = FALSE,
    sort = TRUE,
    descending = if(horizontal) FALSE else TRUE,
    width = width,
    staplewidth=staplewidth
  ) +
    # Add vertical grid lines
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(linetype = "longdash", colour = "lightgrey")) +
    # Add vertical line at the minimum contribution threshold
    ggplot2::geom_vline(xintercept = min_contribution_threshold, linetype = "dashed", colour = "red") +
    # Format x-axis labels as percentages
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(),
      ) +
    # Define fill colors for significant and non-significant contributions
    ggplot2::scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "#AFE1AF")) +
    # Define outline colors for significant and non-significant contributions
    ggplot2::scale_colour_manual(values = c("FALSE" = "grey30", "TRUE" = "#077969"))


  if(horizontal){
    plot <- plot +
      ggplot2::coord_flip(xlim = c(0, NA), expand = TRUE, clip = "off") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust = 1),
        panel.grid.major.y = ggplot2::element_line(linetype = "longdash", colour = "lightgrey"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        )
  }
  return(plot)
}

