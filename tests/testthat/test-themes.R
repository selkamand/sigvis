test_that("sigverse theme works", {
  p = ggplot2::ggplot(iris, ggplot2::aes(x=.data[["Sepal.Length"]], y = .data[["Sepal.Width"]]))
  expect_no_error(p + theme_sigverse())
  expect_no_error(p + theme_minisig())
})
