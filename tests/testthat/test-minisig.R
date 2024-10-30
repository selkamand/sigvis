test_that("sig_visualise_minified works", {

  # Example Signature Visualisation fails without error
  expect_no_error(sm(sig_visualise_minified(signature = sigshared::example_signature(), proportion = 0.4)))

  # Similar Sample
  expect_no_error(sm(sig_visualise_minified(tally_single_sample, 0.5, format = fmt_round(digits=2))))
})
