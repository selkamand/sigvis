test_that("Auto Palette works", {
  sm(expect_no_error(auto_palette(c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G"))))
})
