
#' Sigverse Theme for Signature Plots
#'
#'
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

#' Theme for small signature/catalogue plots
#'
#' A minimal theme with no axis text or ticks.
#' Commonly used for smaller signature/catalogue plots
#'
#' @inheritDotParams ggplot2::theme_bw
#' @inheritParams vis_options
#' @return ggplot2 theme
#' @export
#'
theme_minisig <- function(...){
  ggplot2::theme_bw(...) %+replace%
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "none",
      axis.text.x.bottom= element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y.left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x.bottom = ggplot2::element_line(linewidth = 1)
    )
}
