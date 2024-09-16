test_that("sig_visualise_bootstraps works", {
  colo829_bootstraps <- sigshared::example_bootstraps_colo829()


  gg <- expect_no_error(
    plot(sig_visualise_bootstraps(colo829_bootstraps, min_contribution_threshold = 0.05, pvalue = 0.05, horizontal = TRUE)),
  )

  expect_s3_class(gg, "gg")

})
