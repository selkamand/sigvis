#' Visualize Proportions as a Donut Plot
#'
#' @param proportions A named numeric vector where the values represent proportions, and names represent labels.
#' @param unexplained_label Label for the remaining proportion if proportions sum to less than 1. Defaults to "Unexplained".
#' @param palette A named character vector where the names match the `proportions` vector and `unexplained_label`, and values are colors.
#' @param guide_title The legend's title. Set to `NULL` for no title.
#' @param inner_radius Radius of the hole in the center of the donut (0 to 1 scale).
#'
#' @return A ggplot donut plot.
#' @export
#'
#' @examples
#' model = c('SBS2' = 0.6, 'SBS13' = 0.2)
#' sig_visualise_donut(model)
#'
sig_visualise_donut <- function(proportions, unexplained_label = "Unexplained", inner_radius = 0.6, palette = NULL, guide_title = NULL) {

  if(!is.null(guide_title)) assertions::assert_string(guide_title)
  # Ensure the 'proportions' input is valid
  sigshared::assert_model(proportions)

  # Sort proportions in ascending order for better visualization
  proportions <- sort(proportions, decreasing = FALSE)

  # Calculate the unexplained portion (remaining proportion)
  unexplained <- 1 - sum(proportions)

  # If there is an unexplained proportion, append it to the proportions vector
  if (unexplained > 0) {
    names(unexplained) <- unexplained_label
    proportions <- c(proportions, unexplained)
  }

  # Prepare the data frame for plotting
  data <- data.frame(
    labels = names(proportions),
    proportion = proportions
  )

  # Reorder labels based on proportions for cleaner plotting
  data[["labels"]] <- forcats::fct_reorder(data[["labels"]], data[["proportion"]])
  data[["tooltip"]] <- paste0(data[["labels"]], " (", fmt_percent(data[["proportion"]], digits = 0, space = FALSE), ")")

  # Create the donut plot
  gg_donut <- ggplot(data, aes(x = .data[["proportion"]], y = "", fill = .data[["labels"]])) +
    # Use ggiraph for interactive tooltip and copy-to-clipboard functionality
    ggiraph::geom_col_interactive(aes(data_id = .data[["labels"]], tooltip = .data[["tooltip"]], onclick = paste0('navigator.clipboard.writeText("', .data[["labels"]], '")'))) +
    # Convert bar plot to radial (donut) with specified inner radius
    ggplot2::coord_radial(inner.radius = inner_radius, direction = 1, expand = FALSE) +
    # Add legend with optional title
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, title = guide_title)) +
    # Remove default theme elements for a clean donut plot
    ggplot2::theme_void() +
    # Set legend title to blank if no title is provided
    ggplot2::theme(legend.title = element_blank())

  # If a custom color palette is provided, apply it
  if (!is.null(palette)) {
    # Ensure the palette is a valid character vector and matches the labels
    assertions::assert_character_vector(palette)
    assertions::assert_names_include(palette, data[["labels"]])

    # Apply custom colors to the donut plot
    gg_donut <- gg_donut + ggplot2::scale_fill_manual(values = palette)
  }

  # Return the completed donut plot
  return(gg_donut)
}
