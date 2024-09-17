test_that("sig_visualise_bootstraps works", {
  colo829_bootstraps <- sigshared::example_bootstraps_colo829()

  gg <- expect_no_error(
    plot(sig_visualise_bootstraps(colo829_bootstraps, min_contribution_threshold = 0.05, pvalue = 0.05, horizontal = TRUE)),
  )

  expect_s3_class(gg, "gg")

})


test_that("sig_visualise_bootstraps throws error when an inconsistent number of bootstraps has been run for differents signatures", {
  colo829_bootstraps <- sigshared::example_bootstraps_colo829()

  # Drop first row (imbalancing the number of bootstraps)
  colo829_bootstraps <- colo829_bootstraps[2:nrow(colo829_bootstraps),]

  # Ensure sig_visualise_bootstraps throws an error
  expect_error(
    plot(sig_visualise_bootstraps(colo829_bootstraps, min_contribution_threshold = 0.05, pvalue = 0.05, horizontal = TRUE)),
    "[iI]nconsistent.*[bB]ootstraps"
  )

})
