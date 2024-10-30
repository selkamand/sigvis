
#' Create a Percent Formatting Function
#'
#' Generates a function that formats numeric values as percentage strings with optional digits, spacing, and multiplication by 100.
#'
#' @param digits Integer indicating the number of decimal places. Defaults to `0`.
#' @param space Logical indicating whether to include a space before the percent sign. Defaults to `TRUE`.
#' @param multiply_by_100 Logical indicating whether to multiply the input value by 100. Defaults to `TRUE`.
#'
#' @return A function that takes a numeric value and returns a formatted percentage string.
#' @export
#'
#' @examples
#' # Create a formatter with default settings
#' formatter <- fmt_percent()
#' formatter(0.75)  # "75 %"
#'
#' # Create a formatter with 2 decimal places
#' formatter2 <- fmt_percent(digits = 2)
#' formatter2(0.12345)  # "12.35 %"
#'
#' # Create a formatter without space before the percent sign
#' formatter3 <- fmt_percent(space = FALSE)
#' formatter3(0.5)  # "50%"
fmt_percent <- function(digits = 0, space = TRUE, multiply_by_100 = TRUE) {
  function(proportion){
    s <- if (space) " " else ""
    proportion <- if(multiply_by_100) proportion * 100 else proportion
    paste0(round(proportion, digits = digits), s, "%")
  }
}

percent <- function(x, digits = 0, space = TRUE, multiply_by_100 = TRUE){
  formatter <- fmt_percent(digits = digits, space = space, multiply_by_100 = multiply_by_100)
  formatter(x)
}

#' Create a Rounding Function
#'
#' Generates a function that rounds numeric values to a specified number of digits and returns them as character strings.
#'
#' @param digits Integer indicating the number of decimal places. Defaults to `0`.
#'
#' @return A function that takes a numeric value and returns a character string of the rounded value.
#' @export
#'
#' @examples
#' # Create a formatter with default settings
#' formatter <- fmt_round()
#' formatter(3.14159)  # "3"
#'
#' # Create a formatter with 2 decimal places
#' formatter2 <- fmt_round(digits = 2)
#' formatter2(2.71828)  # "2.72"
fmt_round <- function(digits = 0) {
  function(value) { as.character(round(value, digits = digits)) }
}
