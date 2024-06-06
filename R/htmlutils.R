span <- function(x, color = "black", font_weight = "bold", color_bg = "inherit"){

  paste0('<span style="color:', color, ";font-weight:", font_weight,  '; background-color:', color_bg, '">',x, '</span>')
}
