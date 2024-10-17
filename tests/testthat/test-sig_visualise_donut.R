test_that("sig_visualise_donut works", {
  model = c('SBS2' = 0.6, 'SBS13' = 0.2)
  expect_no_error(sig_visualise_donut(model))
  expect_error(sig_visualise_donut(c(0.6, sbs13=0.2)), regexp = "[Aa]ll elements must be named")

  expect_error(sig_visualise_donut(model, palette = c(Bob="red")), regexp = "'palette' is missing 3 required names: `SBS13, SBS2, and Unexplained`")
  expect_no_error(sig_visualise_donut(model, palette = c(SBS13="red", SBS2="black", Unexplained="grey"), inner_radius=0.5, guide_title="Hello"))

})
