# Tests for fmt_round()
test_that("fmt_round() returns a formatter function", {
  formatter <- fmt_round()
  expect_true(is.function(formatter))
})

test_that("fmt_round() rounds numbers correctly with default parameters", {
  formatter <- fmt_round()
  expect_equal(formatter(3.14159), "3")
  expect_equal(formatter(2.71828), "3")
  expect_equal(formatter(-1.5), "-2")
})

test_that("fmt_round() respects the 'digits' parameter", {
  formatter <- fmt_round(digits = 2)
  expect_equal(formatter(3.14159), "3.14")
  expect_equal(formatter(2.71828), "2.72")
})

test_that("fmt_round() handles negative 'digits' parameter", {
  formatter <- fmt_round(digits = -1)
  expect_equal(formatter(123), "120")
  expect_equal(formatter(123.45), "120")
})

test_that("fmt_round() handles NA and special values", {
  formatter <- fmt_round()
  expect_equal(formatter(NA_real_), NA_character_)
  expect_equal(formatter(Inf), "Inf")
  expect_equal(formatter(-Inf), "-Inf")
  expect_equal(formatter(NaN), "NaN")
})
